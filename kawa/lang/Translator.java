package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.bytecode.ArrayClassLoader;
import gnu.bytecode.ClassType;
import gnu.bytecode.Field;
import gnu.bytecode.Member;
import gnu.bytecode.Type;
import gnu.bytecode.ZipLoader;
import gnu.text.SourceMessages;
import gnu.lists.*;
import gnu.kawa.lispexpr.*;
import java.util.*;
import gnu.kawa.functions.GetNamedPart;
import gnu.kawa.functions.CompileNamedPart;
import gnu.kawa.functions.MakeSplice;
import gnu.kawa.functions.MultiplyOp;
import gnu.kawa.functions.Expt;
import gnu.kawa.xml.XmlNamespace;
import gnu.math.DFloNum;
import gnu.math.IntNum;
import gnu.math.Unit;
import gnu.text.Char;
import gnu.text.SourceLocator;
import gnu.text.StandardNamedChars;
/* #ifdef enable:XML */
import gnu.xml.NamespaceBinding;
/* #endif */
import kawa.standard.Scheme;
import kawa.standard.require.DeclSetMapper;

/** Used to translate from source to Expression.
 * The result has macros expanded, lexical names bound, etc, and is
 * ready for code generation.
 * This is sometimes called a "compilation environment",
 * but we modify it as we go along - there is a single Translator for
 * each top-level form.
 */

public class Translator extends Compilation
{
  // Global environment used to look for syntax/macros.
  private Environment env;

  /** Set if we're processing (as opposed to expanding)
   * a <code>define-syntax</code> or <code>defmacro</code>. */
  public Macro currentMacroDefinition;

  /** Innermost current scope of pattern variable,
   * from a <code>syntax-case</code>. */
  public PatternScope patternScope;

  public Declaration templateScopeDecl;

  /** A "mark" created for the current macro application.
   * This is (more-or-less) the mark specified by the syntax-case
   * specification (in r6rs-lib), applied to the output of a transformer.
   * However, instead of "applying" a mark to the transformer output,
   * we remember in the TemplateScope an object unique to the application.
   */
  Object currentMacroMark = null;

  /** A variable to hold the matched values for syntax-case
   * pattern variables. */
  public Declaration matchArray;

  /** A stack of aliases pushed by <code>pushRenamedAlias</code>. */
  private Stack<Declaration> renamedAliasStack;

  public Object pendingForm;

  public LambdaExp curMethodLambda;

  /* #ifdef enable:XML */
  public NamespaceBinding xmlElementNamespaces = NamespaceBinding.predefinedXML;
  /* #endif */

  public static final Declaration getNamedPartDecl;
  static {
    // Declare the special symbol $lookup$ (from the reader)
    // and bind it to getNamedPartDecl.
    String cname = "gnu.kawa.functions.GetNamedPart";
    String fname = "getNamedPart";
    getNamedPartDecl = Declaration.getDeclarationFromStatic(cname, fname);
    LispLanguage.getNamedPartLocation.setDeclaration(getNamedPartDecl);
  }

  private static Expression errorExp = new ErrorExp ("unknown syntax error");

    public Translator(Language language, SourceMessages messages,
                      NameLookup lexical, Environment env) {
        super(language, messages, lexical);
        this.env = env;
    }

    public Translator(Language language, SourceMessages messages,
                      NameLookup lexical) {
        super(language, messages, lexical);
        this.env = Environment.getCurrent();
    }

    @Override
    public final Environment getGlobalEnvironment() { return env; }

  public Expression parse (Object input)
  {
    return rewrite(input);
  }

  public final Expression rewrite_car (Pair pair, SyntaxForm syntax)
  {
    return rewrite_car(pair, syntax == null ? current_scope : syntax.getScope());
  }

  public final Expression rewrite_car (Pair pair, ScopeExp templateScope)
  {
    if (templateScope == current_scope
	|| pair.getCar() instanceof SyntaxForm)
      return rewrite_car(pair, false);
    ScopeExp save_scope = setPushCurrentScope(templateScope);
    try
      {
	return rewrite_car(pair, false);
      }
    finally
      {
	setPopCurrentScope(save_scope);
      }
  }

  public final Expression rewrite_car (Pair pair, boolean function)
  {
      return rewrite_car(pair, function ? 'F' : 'N');
  }

    public final Expression rewrite_car(Pair pair, char mode) {
        Object saved = pushPositionOf(pair);
        try {
            return rewrite(pair.getCar(), mode);
        } finally {
            popPositionOf(saved);
        }
    }

    /** Similar to rewrite_car.
     * However, we check for (quasiquote exp) specially, and handle that
     * directly.  This is in case quasiquote isn't in scope.
     */
    public final Expression rewrite_car_for_lookup(Pair pair) {
        Object car = pair.getCar();
        if (car instanceof Pair) {
            Pair pcar = (Pair) car;
            if (pcar.getCar() == LispLanguage.quasiquote_sym) {
                Object pos = pushPositionOf(pair);
                Expression ret = Quote.quasiQuote.rewrite(pcar.getCdr(), this);
                popPositionOf(pos);
                return ret;
            }
        }
        return rewrite_car(pair, false);
    }

  Syntax currentSyntax;
  public Syntax getCurrentSyntax() { return currentSyntax; }

  /**  The module instance containing the current macro.
   * This is only used temporarily, set when resolving a Declaration
   * bound to a macro, and used to set the macroContext field of the
   * TemplateScope created when expanding the macro's template(s). */
  Declaration macroContext;

  /**
   * Apply a Syntax object.
   * @param syntax the Syntax object whose rewrite method we call
   * @param form the syntax form (including the macro name)
   * @return the re-written form as an Expression object
   */
  Expression apply_rewrite (Syntax syntax, Pair form)
  {
    Expression exp = errorExp;
    Syntax saveSyntax = currentSyntax;
    currentSyntax = syntax;
    try
      {
	exp = syntax.rewriteForm(form, this);
      }
    finally
      {
        currentSyntax = saveSyntax;
      }
    return exp;
  }

  /** Check if declaraton is an alias for some other name.
   * This is needed to chase identifiers renamed for hygienic macro
   * expansion - see SyntaxRules.expand. */
  static ReferenceExp getOriginalRef(Declaration decl)
  {
    if (decl != null && decl.isAlias() && ! decl.isIndirectBinding())
      {
	Expression value = decl.getValue();
	if (value instanceof ReferenceExp)
	  return (ReferenceExp) value;
      }
    return null;
  }

    public final boolean keywordsAreSelfEvaluating() {
        return ((LispLanguage) getLanguage()).keywordsAreSelfEvaluating();
    }

  public final boolean selfEvaluatingSymbol (Object obj)
  {
    return ((LispLanguage) getLanguage()).selfEvaluatingSymbol(obj);
  }

  /** True iff a form matches a literal symbol. */
  public final boolean matches(Object form, String literal)
  {
    return matches(form, null, literal);
  }

  public boolean matches(Object form, SyntaxForm syntax, String literal)
  {
    if (syntax != null)
      {
        // FIXME
      }
    if (form instanceof SyntaxForm)
      {
	// FIXME
	form = ((SyntaxForm) form).getDatum();
      }
    if (form instanceof SimpleSymbol && ! selfEvaluatingSymbol(form))
      {
	ReferenceExp rexp = getOriginalRef(lexical.lookup(form, -1));
	if (rexp != null)
	  form = rexp.getSymbol();
      }
    return form instanceof SimpleSymbol
      && ((Symbol) form).getLocalPart() == literal;
  }

  public boolean matches(Object form, SyntaxForm syntax, Symbol literal)
  {
    if (syntax != null)
      {
        // FIXME
      }
    if (form instanceof SyntaxForm)
      {
	// FIXME
	form = ((SyntaxForm) form).getDatum();
      }
    if (form instanceof SimpleSymbol && ! selfEvaluatingSymbol(form))
      {
	ReferenceExp rexp = getOriginalRef(lexical.lookup(form, -1));
	if (rexp != null)
	  form = rexp.getSymbol();
      }
    return form == literal;
  }

  public Object matchQuoted (Pair pair)
  {
    if (matches(pair.getCar(), LispLanguage.quote_str)
        && pair.getCdr() instanceof Pair
        && (pair = (Pair) pair.getCdr()).getCdr() == LList.Empty)
      return pair.getCar();
    return null;
  }

  public Declaration lookup(Object name, int namespace)
  {
    Declaration decl = lexical.lookup(name, namespace);
    if (decl != null && getLanguage().hasNamespace(decl, namespace))
      return decl;
    return currentModule().lookup(name, getLanguage(), namespace);
  }

  /** Find global Declaration, creating one if not found. */
  public Declaration lookupGlobal(Object name)
  {
    return lookupGlobal(name, -1);
  }

  /** Find global Declaration, creating one if not found. */
  public Declaration lookupGlobal(Object name, int namespace)
  {
    ModuleExp module = currentModule();
    Declaration decl = module.lookup(name, getLanguage(), namespace);
    if (decl == null)
      {
        decl = module.getNoDefine(name);
        decl.setIndirectBinding(true);
      }
    return decl;
  }

  /** Check if a Declaration is bound to a Syntax.
   * @param decl the Declaration to check
   * @return the Syntax bound to decl, or null.
   * In the former case, macroContext may be set as a side effect.
   */
  Syntax check_if_Syntax (Declaration decl)
  {
    Declaration d = Declaration.followAliases(decl);
    Object obj = null;
    Expression dval = d.getValue();
    if (dval != null && d.getFlag(Declaration.IS_SYNTAX))
      {
        try
          {
            if (decl.getValue() instanceof ReferenceExp)
              {
                Declaration context
                  = ((ReferenceExp) decl.getValue()).contextDecl();
                if (context != null)
                  macroContext = context;
                else if (current_scope instanceof TemplateScope)
                  macroContext = ((TemplateScope) current_scope).macroContext;
              }
            else if (current_scope instanceof TemplateScope)
              macroContext = ((TemplateScope) current_scope).macroContext;
            obj = dval.eval(env);
          }
        catch (Error ex)
          {
            ex.printStackTrace();
            throw ex;
          }
        catch (Throwable ex)
          {
            ex.printStackTrace();
            error('e', "unable to evaluate macro for "+decl.getSymbol());
          }
      }
    else if (decl.getFlag(Declaration.IS_SYNTAX) && ! decl.needsContext())
      {
	StaticFieldLocation loc = StaticFieldLocation.make(decl);
	obj = loc.get(null);
      }

    return obj instanceof Syntax ? (Syntax) obj : null;
  }

  public Expression rewrite_pair(Pair p, char mode)
  {
    Object p_car = p.getCar();
    Expression func;
    boolean useHelper = true;
    if (p_car instanceof Pair
        && ((Pair) p_car).getCar() == LispLanguage.splice_sym) {
        func = gnu.kawa.reflect.MakeAnnotation.makeAnnotationMaker
            (rewrite_car((Pair) ((Pair) p_car).getCdr(), false));
        useHelper = false;
    }
    else
        func = rewrite_car(p, 'F');
    Object proc = null;
    if (func instanceof QuoteExp)
      {
        proc = func.valueIfConstant();
        if (proc instanceof Syntax)
          return apply_rewrite((Syntax) proc, p);
      }
    ReferenceExp ref = null;
    if (func instanceof ReferenceExp)
      {
	ref = (ReferenceExp) func;
        Declaration decl = ref.getBinding();
	if (decl == null)
	  {
	    Object sym = ref.getSymbol();
	    Symbol symbol;
	    String name;
	    if (sym instanceof Symbol && ! selfEvaluatingSymbol(sym))
	      {
		symbol = (Symbol) sym;
		name = symbol.getName();
	      }
	    else
	      {
		name = sym.toString();
		symbol = env.getSymbol(name);
	      }
	    proc = env.get(symbol,
			   getLanguage().hasSeparateFunctionNamespace()
			   ? EnvironmentKey.FUNCTION
			   : null,
			   null);
	    if (proc instanceof Syntax)
	      return apply_rewrite ((Syntax) proc, p);
            if (proc instanceof AutoloadProcedure)
              {
                try
                  {
                    proc = ((AutoloadProcedure) proc).getLoaded();
                  }
                catch (RuntimeException ex)
                  {
                    proc = null;
                  }
              }
	  }
        else
	  {
            Declaration saveContext = macroContext;
            Syntax syntax = check_if_Syntax (decl);
            if (syntax != null)
              {
                Expression e = apply_rewrite (syntax, p);
                macroContext = saveContext;
                return e;
              }
	  }

	ref.setProcedureName(true);
	if (getLanguage().hasSeparateFunctionNamespace())
	  func.setFlag(ReferenceExp.PREFER_BINDING2);
      }

    boolean isNamedPartDecl = func instanceof ReferenceExp
        && (((ReferenceExp) func).getBinding()==getNamedPartDecl);
    if (isNamedPartDecl)
      useHelper = false;

    Object cdr = p.getCdr();
    int cdr_length = listLength(cdr);

    if (cdr_length < 0)
      return syntaxError
          ("improper list (circular or dotted) is not allowed here");
    Expression applyFunction =  useHelper ? applyFunction(func) : null;

    Stack vec = new Stack();
    if (applyFunction != null) {
        vec.add(func);
        func = applyFunction;
    }

    ScopeExp save_scope = current_scope;
    int first_keyword = -1;
    int last_keyword = -1;
    boolean bad_keyword_reported = false;
    int firstSpliceArg = -1;
    int i = 0;
    while (cdr != LList.Empty)
      {
	if (cdr instanceof SyntaxForm)
	  {
	    SyntaxForm sf = (SyntaxForm) cdr;
	    cdr = sf.getDatum();
	    // I.e. first time do equivalent of setPushCurrentScope
            if (current_scope == save_scope)
              lexical.pushSaveTopLevelRedefs();
	    setCurrentScope(sf.getScope());
	  }
        Object save_pos = pushPositionOf(cdr);
	Pair cdr_pair = (Pair) cdr;
        Object cdr_car = cdr_pair.getCar();
        Object cdr_cdr = cdr_pair.getCdr();
        Expression arg;
        if (cdr_car instanceof Keyword) {
            if (first_keyword < 0) {
                first_keyword = i;
                last_keyword = i - 2; // To suppress incorrect warnings
            }
            if (bad_keyword_reported)
                ;
            else if (keywordsAreSelfEvaluating())
                last_keyword = i;
            else if (i == last_keyword + 1 || i + 1 == cdr_length) {
                bad_keyword_reported = true;
                error('w', "missing value after unquoted keyword");
            } else if (i != last_keyword + 2) {
                bad_keyword_reported = true;
                error('w', "keyword separated from other keyword arguments");
            } else
                last_keyword = i;
            arg = QuoteExp.getInstance(cdr_car, this);
            arg.setFlag(QuoteExp.IS_KEYWORD);
        } else if (cdr_cdr instanceof Pair
                   // FIXME should check binding for ... is builtin ...
                   && ((Pair) cdr_cdr).getCar() == LispLanguage.dots3_sym) { 
            LambdaExp dotsLambda = new LambdaExp();
            pushScanContext(dotsLambda);
            dotsLambda.body = rewrite_car(cdr_pair, false);
            ScanContext scanContext = getScanContext();
            LinkedHashMap<Declaration,Declaration> sdecls
                = scanContext.decls;
            int nseqs = sdecls.size();
            ArrayList<Expression> scanExps = scanContext.scanExpressions;
            int nexps = scanExps == null ? 0 : scanExps.size();
            Expression[] subargs = new Expression[nseqs + nexps + 1];
            subargs[0] = dotsLambda;
            popScanContext();
            Iterator<Declaration> sit = sdecls.keySet().iterator();
            int j = 1;
            while (sit.hasNext()) {
                Declaration sdecl = sit.next();
                if (curScanNesting() > 0) {
                    sdecl = getScanContext().addSeqDecl(sdecl);
                }
                ReferenceExp rexp = new ReferenceExp(sdecl);
                subargs[j++] = rexp;
            }
            for (int k = 0; k < nexps; k++) {
                subargs[j++] = scanExps.get(k);
            }
            arg = new ApplyExp(Scheme.map, subargs);
            arg = new ApplyExp(MakeSplice.quoteInstance, arg);
            cdr_cdr = ((Pair) cdr_cdr).getCdr();
            if (firstSpliceArg < 0)
                firstSpliceArg = i + (applyFunction != null ? 1 : 0);
        } else {
            Object cdr_car_car;
            if (cdr_car instanceof Pair
                && ((cdr_car_car = ((Pair) cdr_car).getCar()) == LispLanguage.splice_sym
                    || cdr_car_car == LispLanguage.splice_colon_sym)) {
                arg = rewrite_car((Pair) ((Pair) cdr_car).getCdr(), false);
                QuoteExp splicer = cdr_car_car == LispLanguage.splice_sym
                    ? MakeSplice.quoteInstance
                    : MakeSplice.quoteKeywordsAllowedInstance;
                arg = new ApplyExp(splicer, arg);
                if (firstSpliceArg < 0)
                    firstSpliceArg = i + (applyFunction != null ? 1 : 0);
            }
            else 
                arg = rewrite_car(cdr_pair, isNamedPartDecl ? 'R' : 'N');
        }
        i++;

        vec.addElement(arg);
	cdr = cdr_cdr;
        popPositionOf(save_pos);
      }


    Expression[] args = new Expression[vec.size()];
    vec.copyInto(args);

    if (save_scope != current_scope)
      setPopCurrentScope(save_scope);

    if (isNamedPartDecl)
        return rewrite_lookup(args[0], args[1], mode);

    ApplyExp app = new ApplyExp(func, args);
    app.firstSpliceArg = firstSpliceArg;
    if (first_keyword >= 0)
      {
        app.numKeywordArgs = (last_keyword - first_keyword) / 2 + 1;
        app.firstKeywordArgIndex = first_keyword + (applyFunction != null ? 2 : 1);
      }
    return app;
  }

    public Expression rewrite_lookup(Expression part1, Expression part2, char mode) {
        Symbol sym = namespaceResolve(part1, part2);
        if (sym != null)
          return rewrite(sym, mode);
        // FIXME don't copy the args array in makeExp ...
        return CompileNamedPart.makeExp(part1, part2);
    }

  public Namespace namespaceResolvePrefix (Expression context)
  {
    if (context instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) context;
        Declaration decl = rexp.getBinding();
        Object val;
        if (decl == null || decl.getFlag(Declaration.IS_UNKNOWN))
          {
            Object rsym = rexp.getSymbol();
            Symbol sym = rsym instanceof Symbol ? (Symbol) rsym
              : env.getSymbol(rsym.toString());
            val = env.get(sym, null);
          }
        else if (decl.isNamespaceDecl())
          {
            val = decl.getConstantValue();
          }
        else
          val = null;
        if (val instanceof Namespace)
          {
            Namespace ns = (Namespace) val;
            String uri = ns.getName();
            if (uri != null && uri.startsWith("class:"))
              return null;
            return ns;
          }
      }
    return null;
  }

  public Symbol namespaceResolve (Namespace ns, Expression member)
  {
    if (ns != null && member instanceof QuoteExp)
      {
        String mem = ((QuoteExp) member).getValue().toString().intern();
        return ns.getSymbol(mem);
      }
    return null;
  }

  public Symbol namespaceResolve (Expression context, Expression member)
  {
    return namespaceResolve(namespaceResolvePrefix(context), member);
  }

  public static Object stripSyntax (Object obj)
  {
    while (obj instanceof SyntaxForm)
      obj = ((SyntaxForm) obj).getDatum();
    return obj;
  }

  public static Object safeCar (Object obj)
  {
    while (obj instanceof SyntaxForm)
      obj = ((SyntaxForm) obj).getDatum();
    if (! (obj instanceof Pair))
      return null;
    return stripSyntax(((Pair) obj).getCar());
  }

  public static Object safeCdr (Object obj)
  {
    while (obj instanceof SyntaxForm)
      obj = ((SyntaxForm) obj).getDatum();
    if (! (obj instanceof Pair))
      return null;
    return stripSyntax(((Pair) obj).getCdr());
  }

  /** Returns the length of a syntax list.
   * Returns Integer.MIN_VALUE for cyclic lists.
   * For impure lists returns the negative of one more than
   * the number of pairs before the "dot".
   * Similar to LList.listLength, but handles SyntaxForm more efficiently. */
  public static int listLength(Object obj)
  {
    // Based on list-length implementation in
    // Guy L Steele jr: "Common Lisp:  The Language", 2nd edition, page 414
    int n = 0;
    Object slow = obj;
    Object fast = obj;
    for (;;)
      {
	// 'n' is number of previous Pairs before 'fast' cursor.
	while (fast instanceof SyntaxForm)
	  fast = ((SyntaxForm) fast).getDatum();
	while (slow instanceof SyntaxForm)
	  slow = ((SyntaxForm) slow).getDatum();
	if (fast == LList.Empty)
	  return n;
	if (! (fast instanceof Pair))
	  return -1-n;
	n++;
	Object next = ((Pair) fast).getCdr();
	while (next instanceof SyntaxForm)
	  next = ((SyntaxForm) next).getDatum();
	if (next == LList.Empty)
	  return n;
	if (! (next instanceof Pair))
	  return -1-n;
	slow = ((Pair)slow).getCdr();
	fast = ((Pair)next).getCdr();
	n++;
	if (fast == slow)
	  return Integer.MIN_VALUE;
      }
  }

  public void rewriteInBody (Object exp)
  {
    if (exp instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) exp;
	ScopeExp save_scope = setPushCurrentScope(sf.getScope());
	try
	  {
	    rewriteInBody(sf.getDatum());
	  }
	finally
	  {
	    setPopCurrentScope(save_scope);
	  }
      }
    else if (exp instanceof ValuesFromLList)
      {
          // Optimization of following case.
          // More importantly, we make use of the line number information.
          for (Object vs = ((ValuesFromLList) exp).values;
               vs != LList.Empty; )
          {
              Pair p = (Pair) vs;
              pushForm(rewrite_car(p, false));
              vs = p.getCdr();
          }
      }
    else if (exp instanceof Values)
      {
	Object[] vals = ((Values) exp).getValues();
	for (int i = 0;  i < vals.length;  i++)
	  rewriteInBody(vals[i]);
      }
    else {
        Expression e = rewrite(exp, 'N');
        setLineOf(e);
        pushForm(e);
    }
  }

    public int getCompletions(Environment env,
                              String nameStart, Object property,
                              String namespaceUri,
                              List<? super String> matches) {
        LocationEnumeration e = env.enumerateAllLocations();
        int count = 0;
        while (e.hasMoreElements()) {
            Location loc = e.nextLocation();
            Symbol sym = loc.getKeySymbol();
            String local = sym == null ? null : sym.getLocalPart();
            if (local != null && local.startsWith(nameStart)
                && property == loc.getKeyProperty()
                && namespaceUri == sym.getNamespaceURI()) {
                count++;
                matches.add(local);
            }
        }
        return count;
    }

    public Object namespaceResolve(Object name) {
        Object prefix = null;
        Expression part2 = null;
        Pair p;
        if (name instanceof Pair
            && safeCar(p = (Pair) name) == LispLanguage.lookup_sym
            && p.getCdr() instanceof Pair
            && (p = (Pair) p.getCdr()).getCdr() instanceof Pair) {
            prefix = namespaceResolve(p.getCar());
            if (! (stripSyntax(prefix) instanceof Symbol))
                return name;
            part2 = rewrite_car_for_lookup((Pair) p.getCdr());
        }
        else if (name instanceof Symbol) {
            Symbol s = (Symbol) name;
            if (s.hasUnknownNamespace()) {
                String loc = s.getLocalPart();
                prefix = Symbol.valueOf(s.getPrefix());
                part2 = QuoteExp.getInstance(Symbol.valueOf(s.getLocalPart()));
            }
        }
        if (part2 != null) {
            Expression part1 = rewrite(prefix, 'R');
            Symbol sym = namespaceResolve(part1, part2);
            if (sym != null)
                return sym;
            String combinedName = CompileNamedPart.combineName(part1, part2);
            if (combinedName != null)
                return Namespace.EmptyNamespace.getSymbol(combinedName);
        }
        return name;
    }

    /**
     * Re-write a Scheme expression in S-expression format into internal form.
     */
    public Expression rewrite(Object exp) {
        return rewrite(exp, 'N');
    }

    /**
     * Re-write a Scheme expression in S-expression format into internal form.
     */
    public Expression rewrite(Object exp, boolean function) {
        return rewrite(exp, function ? 'F' : 'N');
    }

    /** Re-write a Scheme expression in S-expression format into internal form.
     * @param mode either 'N' (normal), 'F' (function application context),
     *  'M' (macro-checking), 'Q' (colon-form in quote),
     *  or 'R' (namespace-resolve).
     */
    public Expression rewrite(Object exp, char mode) {
        if (exp instanceof SyntaxForm) {
            SyntaxForm sf = (SyntaxForm) exp;
            ScopeExp save_scope = setPushCurrentScope(sf.getScope());
            try {
                Expression s = rewrite(sf.getDatum(), mode);
                return s;
            } finally {
                setPopCurrentScope(save_scope);
            }
        }
        boolean function = mode != 'N' && mode != 'R';
        if (exp instanceof Pair && mode != 'Q') {
            Expression e = rewrite_pair((Pair) exp, mode);
            setLineOf(e);
            return e;
        } else if (exp instanceof Symbol && ! selfEvaluatingSymbol(exp)) {
            Symbol s = (Symbol) exp;

            // Check if we're handling a completion request.
            int complete = s.getLocalName()
                .indexOf(CommandCompleter.COMPLETE_REQUEST);
            boolean separate = getLanguage().hasSeparateFunctionNamespace();
            if (complete >= 0) {
                List<String> candidates = new ArrayList<String>();
                String prefix = s.toString().substring(0, complete);
                Object property = function && separate ? EnvironmentKey.FUNCTION
                    : null;
                int symspace = function ? Language.FUNCTION_NAMESPACE
                    : Language.VALUE_NAMESPACE;
                getCompletions(env, prefix, property, s.getNamespaceURI(),
                               candidates);
                lexical.getCompletingSymbols(prefix, symspace,
                                             candidates);
                throw new CommandCompleter(complete, candidates,
                                           prefix, prefix.length(), this);
            }

            if (s.hasUnknownNamespace()) {
                String loc = s.getLocalPart();
                return rewrite_lookup(rewrite(Symbol.valueOf(s.getPrefix()), false),
                                      QuoteExp.getInstance(Symbol.valueOf(s.getLocalPart())),
                                      mode);
            }
            Declaration decl = lexical.lookup(exp, function);
            Declaration cdecl = null;

            // If we're nested inside a class (in a ClassExp) then the field
            // and methods names of this class and super-classes/interfaces
            // need to be searched.
            ScopeExp scope = current_scope;
            int decl_nesting = decl == null ? -1 : ScopeExp.nesting(decl.context);
            String dname;
            if (exp instanceof SimpleSymbol)
                dname = exp.toString();
            else {
                dname = null;
                scope = null;
            }
            for (;scope != null; scope = scope.getOuter()) {
                if (scope instanceof LambdaExp
                    && scope.getOuter() instanceof ClassExp // redundant? FIXME
                    && ((LambdaExp) scope).isClassMethod()
                    && mode != 'M') {
                    if (decl_nesting >= ScopeExp.nesting(scope.getOuter()))
                        break;
                    LambdaExp caller = (LambdaExp) scope;
                    ClassExp cexp = (ClassExp) scope.getOuter();
                    ClassType ctype = (ClassType) cexp.getClassType();
                    // If ctype is a class that hasn't been compiled yet (and
                    // ClassExp#declareParts hasn't been called yet)
                    // then we may may get a tentative Field created
                    // by ClassExp#createFields.
                    // BUG: This doesn't work for not-yet compiled methods.
                    Member part = SlotGet.lookupMember(ctype, dname, ctype);
                    boolean contextStatic
                        = (caller == cexp.clinitMethod
                           || (caller != cexp.initMethod 
                               && caller.nameDecl.isStatic()));
                    if (part == null) {
                        PrimProcedure[] methods
                            = ClassMethods.getMethods(ctype, dname,
                                                      contextStatic ? 'S' : 'V',
                                                      ctype, language);
                        if (methods.length == 0)
                            continue;
                    } else {
                        gnu.expr.SourceName snameAnn =
                            part.getAnnotation(gnu.expr.SourceName.class);
                        // If SourceName annotation, require exact match.
                        if (snameAnn != null && ! dname.equals(snameAnn.name()))
                            continue;
                    }
                    Expression part1;
                    // FIXME We're throwing away 'part', which is wasteful.
                    if (contextStatic)
                        part1 = new ReferenceExp(((ClassExp) caller.getOuter()).nameDecl);
                    else
                        part1 = new ThisExp(caller.firstDecl());
                    return CompileNamedPart.makeExp(part1,
                                                    QuoteExp.getInstance(dname));
                }
            }

            Object nameToLookup;
            if (decl != null) {
                nameToLookup = decl.getSymbol();
                exp = null;
                ReferenceExp rexp = getOriginalRef(decl);
                if (rexp != null) {
                    decl = rexp.getBinding();
                    if (decl == null) {
                        exp = rexp.getSymbol();
                        nameToLookup = exp;
                    }
                }
            } else {
                nameToLookup = exp;
            }
            Symbol symbol = (Symbol) exp;
            if (decl != null) {
                if (current_scope instanceof TemplateScope && decl.needsContext())
                    cdecl = ((TemplateScope) current_scope).macroContext;
                else if (decl.getFlag(Declaration.FIELD_OR_METHOD)
                         && ! decl.isStatic()) {
                    scope = currentScope();
                    for (;;) {
                        if (scope == null)
                            throw new Error("internal error: missing "+decl);
                        if (scope.getOuter() == decl.context) // I.e. same class.
                            break;
                        scope = scope.getOuter();
                    }
                    cdecl = scope.firstDecl();
                }
            } else {
                Location loc
                    = env.lookup(symbol,
                                 function && separate ? EnvironmentKey.FUNCTION
                                 : null);
                if (loc != null)
                    loc = loc.getBase();
                if (loc instanceof FieldLocation) {
                    FieldLocation floc = (FieldLocation) loc;
                    try {
                        decl = floc.getDeclaration();
                        if (! inlineOk(null)
                            // A kludge - we get a bunch of testsuite failures
                            // if we don't inline $lookup$.  FIXME.
                            && (decl != getNamedPartDecl
                                // Another kludge to support "object" as a
                                // type specifier.
                                && ! isObjectSyntax(floc.getDeclaringClass(),
                                                    floc.getMemberName())))
                            decl = null;
                        else if (immediate) {
                            if (! decl.isStatic()) {
                                cdecl = new Declaration("(module-instance)");
                                cdecl.setValue(new QuoteExp(floc.getInstance()));
                            }
                        } else if (decl.isStatic()) {
                            // If the class has been loaded through ZipLoader
                            // or ArrayClassLoader then it might not be visible
                            // if loaded through some other ClassLoader.
                            Class fclass = floc.getRClass();
                            ClassLoader floader;
                            if (fclass == null
                                || ((floader = fclass.getClassLoader())
                                    instanceof ZipLoader)
                                || floader instanceof ArrayClassLoader)
                                decl = null;
                        } else
                            decl = null;
                    } catch (Exception ex) {
                        error('e',
                              "exception loading '" + exp
                              + "' - " + ex.getMessage());
                        decl = null;
                    }
                }
                else if (mode != 'M' && (loc == null || ! loc.isBound()))
                {
                    Expression e = checkDefaultBinding(symbol, this);
                    if (e != null)
                        return e;
                }
                /*
                else if (Compilation.inlineOk && function) {
                    // Questionable.  fail with new set_b implementation,
                    // which just call rewrite_car on the lhs,
                    // if we don't require function to be true.  FIXME.
                    decl = Declaration.getDeclaration(proc);
                }
                */
            }
            if (decl != null) {
                // A special kludge to deal with the overloading between the
                // object macro and object as being equivalent to java.lang.Object.
                // A cleaner solution would be to use an identifier macro.
                Field dfield = decl.getField();
                if (! function && dfield != null
                    && isObjectSyntax(dfield.getDeclaringClass(),
                                      dfield.getName()))
                    return QuoteExp.getInstance(Object.class);

                if (decl.getContext() instanceof PatternScope)
                    return syntaxError("reference to pattern variable "+decl.getName()+" outside syntax template");
            }

            if (decl == null && function
                && nameToLookup==LispLanguage.lookup_sym) {
                decl = getNamedPartDecl;
            }
            int scanNesting = decl == null ? 0
                : Declaration.followAliases(decl).getScanNesting();
            if (scanNesting > 0) {
                if (scanNesting > curScanNesting())
                    error('e', "using repeat variable '"+decl.getName()+"' while not in repeat context");
                else {
                    return new ReferenceExp
                        (scanContextStack.get(scanNesting-1).addSeqDecl(decl));
                }
            }
            ReferenceExp rexp = new ReferenceExp (nameToLookup, decl);
            rexp.setContextDecl(cdecl);
            rexp.setLine(this);
            /* FUTURE?  do error reporting early
            if (decl == null && mode != 'M' && mode != 'R' && mode != 'Q') {
                if (warnUndefinedVariable())
                    error('w', "no declaration seen for "+nameToLookup);
            }
            */
            if (function && separate)
                rexp.setFlag(ReferenceExp.PREFER_BINDING2);
            return rexp;
        } else if (exp instanceof LangExp)
            return rewrite(((LangExp) exp).getLangValue(), function);
        else if (exp instanceof Expression)
            return (Expression) exp;
        else if (exp == Special.abstractSpecial)
            return QuoteExp.abstractExp;
        else if (exp == Boolean.TRUE)
            return QuoteExp.trueExp;
        else if (exp == Boolean.FALSE)
            return QuoteExp.falseExp;
        else if (exp == Special.nativeSpecial)
            return QuoteExp.nativeExp;
        else {
            if (exp instanceof Keyword && ! keywordsAreSelfEvaluating())
                error('w', "keyword should be quoted if not in argument position");
            if (exp instanceof String)
                exp = new IString((String) exp);
            return QuoteExp.getInstance(Quote.quote(exp, this), this);
        }
    }

    /** 
     * If a symbol is lexically unbound, look for a default binding.
     * The default implementation does the following:
     * 
     * If the symbol is the name of an existing Java class, return that class.
     * Handles both with and without (semi-deprecated) angle-brackets:
     *   {@code <java.lang.Integer>} and {@code java.lang.Integer}.
     * Also handles arrays, such as {@code java.lang.String[]}.
     *
     * If the symbol starts with {@code '@'} parse as an annotation class.
     *
     * Recognizes quanties with units, such as {@code 2m} and {@code 3m/s^2}.
     *
     * Handles the xml and unit namespaces.
     *
     * @return null if no binding, otherwise an Expression.
     * 
     * FIXME: This method should be refactored. The quantities parsing should
     *        be moved to its own method at least.
     */
    public Expression checkDefaultBinding(Symbol symbol, Translator tr) {
        Namespace namespace = symbol.getNamespace();
        String local = symbol.getLocalPart();
        String name = symbol.toString();
        int len = name.length();

        if (namespace instanceof XmlNamespace)
            return makeQuoteExp(((XmlNamespace) namespace).get(local));
        String namespaceName = namespace.getName();
        if (namespaceName == LispLanguage.unitNamespace.getName()) {
            Object val = Unit.lookup(local);
            if (val != null)
                return makeQuoteExp(val);
        }
        if (namespaceName == LispLanguage.entityNamespace.getName()) {
            Object val = lookupStandardEntity(local);
            if (val == null) {
                tr.error('e', "unknown entity name "+local);
                val = "{"+namespace.getPrefix()+":"+local+"}";
            }
            return makeQuoteExp(val);
        }

        char ch0 = name.charAt(0);

        if (ch0 == '@') { // Deprecated - reader now returns ($splice$ ATYPE).
            String rest = name.substring(1);
            Expression classRef = tr.rewrite(Symbol.valueOf(rest));
            return MakeAnnotation.makeAnnotationMaker(classRef);
        }

        // Look for quantities.
        if (ch0 == '-' || ch0 == '+' || Character.digit(ch0, 10) >= 0) {
            // 1: initial + or -1 seen.
            // 2: digits seen
            // 3: '.' seen
            // 4: fraction seen
            // 5: [eE][=+]?[0-9]+ seen
            int state = 0;
            int i = 0;
      
            for (; i < len; i++) {
                char ch = name.charAt(i);
                if (Character.digit(ch, 10) >= 0)
                    state = state < 3 ? 2 : state < 5 ? 4 : 5;
                else if ((ch == '+' || ch == '-') && state == 0)
                    state = 1;
                else if (ch == '.' && state < 3)
                    state = 3;
                else if ((ch == 'e' || ch == 'E') && (state == 2 || state == 4)
                         && i + 1 < len) {
                    int j = i + 1;
                    char next = name.charAt(j);
                    if ((next == '-' || next == '+') && ++j < len)
                        next = name.charAt(j);
                    if (Character.digit(next, 10) < 0)
                        break;
                    state = 5;
                    i = j + 1;
                }
                else
                    break;
            }
            tryQuantity:
            if (i < len && state > 1) {
                DFloNum num = new DFloNum(name.substring(0, i));
                boolean div = false;
                ArrayList vec = new ArrayList();
                for (; i < len;) {
                    char ch = name.charAt(i++);
                    if (ch == '*') {
                        if (i == len)
                            break tryQuantity;
                        ch = name.charAt(i++);
                    } else if (ch == '/') {
                        if (i == len || div)
                            break tryQuantity;
                        div = true;
                        ch = name.charAt(i++);
                    }
                    int unitStart = i - 1;
                    int unitEnd;
                    for (;;) {
                        if (!Character.isLetter(ch)) {
                            unitEnd = i - 1;
                            if (unitEnd == unitStart)
                                break tryQuantity;
                            break;
                        }
                        if (i == len) {
                            unitEnd = i;
                            ch = '1';
                            break;
                        }
                        ch = name.charAt(i++);
                    }
                    vec.add(name.substring(unitStart, unitEnd));
                    boolean expRequired = false;
                    if (ch == '^') {
                        expRequired = true;
                        if (i == len)
                            break tryQuantity;
                        ch = name.charAt(i++);
                    }
                    boolean neg = div;
                    if (ch == '+') {
                        expRequired = true;
                        if (i == len)
                            break tryQuantity;
                        ch = name.charAt(i++);
                    } else if (ch == '-') {
                        expRequired = true;
                        if (i == len)
                            break tryQuantity;
                        ch = name.charAt(i++);
                        neg = !neg;
                    }
                    int nexp = 0;
                    int exp = 0;
                    for (;;) {
                        int dig = Character.digit(ch, 10);
                        if (dig <= 0) {
                            i--;
                            break;
                        }
                        exp = 10 * exp + dig;
                        nexp++;
                        if (i == len)
                            break;
                        ch = name.charAt(i++);
                    }
                    if (nexp == 0) {
                        exp = 1;
                        if (expRequired)
                            break tryQuantity;
                    }
                    if (neg)
                        exp = -exp;
                    vec.add(IntNum.make(exp));
                }
                if (i == len) {
                    int nunits = vec.size() >> 1;
                    Expression[] units = new Expression[nunits];
                    for (i = 0; i < nunits; i++) {
                        String uname = (String) vec.get(2 * i);
                        Symbol usym = LispLanguage.unitNamespace.getSymbol(uname.intern());
                        Expression uref = tr.rewrite(usym);
                        IntNum uexp = (IntNum) vec.get(2 * i + 1);
                        if (uexp.longValue() != 1)
                            uref = new ApplyExp(Expt.expt,
                                                new Expression[] {
                                    uref, makeQuoteExp(uexp)
                                });
                        units[i] = uref;
                    }
                    Expression unit;
                    if (nunits == 1)
                        unit = units[0];
                    else
                        unit = new ApplyExp(MultiplyOp.TIMES, units);
                    return new ApplyExp(MultiplyOp.TIMES,
                                        new Expression[] {
                            makeQuoteExp(num),
                            unit
                        });
                }
            }
        }

        boolean sawAngle;
        if (len > 2 && ch0 == '<' && name.charAt(len - 1) == '>') {
            name = name.substring(1, len - 1);
            len -= 2;
            sawAngle = true;
        } else
            sawAngle = false;
        int rank = 0;
        while (len > 2 && name.charAt(len - 2) == '['
               && name.charAt(len - 1) == ']') {
            len -= 2;
            rank++;
        }
        //(future) String cname = (namespace == LispPackage.ClassNamespace) ? local : name;
        String cname = name;
        if (rank != 0)
            cname = name.substring(0, len);
        try {
            Type type = getLanguage().getNamedType(cname);
            if (rank > 0 && (!sawAngle || type == null)) {
                Symbol tsymbol = namespace.getSymbol(cname.intern());
                Expression texp = tr.rewrite(tsymbol, false);
                texp = InlineCalls.inlineCalls(texp, tr);
                if (!(texp instanceof ErrorExp))
                    type = tr.getLanguage().getTypeFor(texp);
            }
            if (type != null) {
                // Somewhat inconsistent: Types named by getNamedType are Type,
                // while standard type/classes are Class.  FIXME.
                while (--rank >= 0) {
                    type = gnu.bytecode.ArrayType.make(type);
                }
                return makeQuoteExp(type);
            }
            Class clas;
            type = Type.lookupType(cname);
            if (type instanceof gnu.bytecode.PrimType)
                clas = type.getReflectClass();
            else {
                if (cname.indexOf('.') < 0)
                    cname = (tr.classPrefix
                             + Mangling.mangleNameIfNeeded(cname));
                if (rank == 0) {
                    ModuleManager mmanager = ModuleManager.getInstance();
                    ModuleInfo typeInfo = mmanager.searchWithClassName(cname);
                    if (typeInfo != null) {
                        Compilation tcomp = typeInfo.getCompilation();
                        if (tcomp != null && tcomp.mainClass != null) {
                            QuoteExp qexp = new QuoteExp(tcomp.mainClass,
                                                         Type.javalangClassType);
                            qexp.setLocation(this);
                            return qexp;
                        }
                    }
                }

                clas = ClassType.getContextClass(cname);
            }
            if (clas != null) {
                if (rank > 0) {
                    type = Type.make(clas);
                    while (--rank >= 0) {
                        type = gnu.bytecode.ArrayType.make(type);
                    }
                    clas = type.getReflectClass();
                }
                return makeQuoteExp(clas);
            }
        } catch (ClassNotFoundException ex) {
            Package pack = gnu.bytecode.ArrayClassLoader.getContextPackage(name);
            if (pack != null)
                return makeQuoteExp(pack);
        } catch (NoClassDefFoundError ex) {
            tr.error('w', "error loading class " + cname + " - " + ex.getMessage() + " not found");
        } catch (Exception ex) {
        }
        if (name.startsWith("array")) {
            Type atype = LispLanguage.decodeArrayType(name);
            if (atype != null)
                return makeQuoteExp(atype);
        }

        return null;
    }

    static Map<String,String> standardEntities;
    public static synchronized String lookupStandardEntity(String key) {
        if (standardEntities == null) {
            standardEntities = new HashMap<String,String>();
            Char.addNamedChars(standardEntities);
        }
        String val = standardEntities.get(key);
        if (val != null)
            return val;
        return val = StandardNamedChars.instance.get(key);
    }

  public static void setLine(Expression exp, Object location)
  {
    if (location instanceof SourceLocator)
      exp.setLocation((SourceLocator) location);
  }

  public static void setLine(Declaration decl, Object location)
  {
    if (location instanceof SourceLocator)
      decl.setLocation((SourceLocator) location);
  }

  PairWithPosition positionPair;

  /*
  public Object pushPositionOfCar(Object pair)
  {
    if (pair instanceof Pair)
      {
        Object car = ((Pair) pair).getCar();
        if (car instanceof PairWithPosition)
          pair = car;
      }
    return pushPositionOf(pair);
    }*/

  /** Note current line number position from a PairWithPosition.
   * Return an object to pass to popPositionOf.
   */
  public Object pushPositionOf(Object pos)
  {
    if (pos instanceof SyntaxForm)
      pos = ((SyntaxForm) pos).getDatum();
    PairWithPosition pair;
    if (pos instanceof PairWithPosition)
        pair = (PairWithPosition) pos;
    else if (pos instanceof SourceLocator)
        pair = new PairWithPosition((SourceLocator) pos, null, null);
    else
      return null;
    Object saved;
    if (positionPair == null
	|| positionPair.getFileName() != getFileName()
	|| positionPair.getLineNumber() != getLineNumber()
	|| positionPair.getColumnNumber() != getColumnNumber())
      {
        saved = new PairWithPosition(this, this, positionPair);
      }
    else
      saved = positionPair;
    setLine(pos);
    positionPair = pair;
    return saved;
  }

  /** Restore  line number position from a previous pushPositionOf.
   * @param saved value returned by matching pushPositionOf.
   */
  public void popPositionOf(Object saved)
  {
    if (saved == null)
      return;
    setLine(saved);
    positionPair = (PairWithPosition) saved;
    if (positionPair.getCar() == this)
      positionPair = (PairWithPosition) positionPair.getCdr();
  }

    public void errorWithPosition(String message, Object form) {
        Object save = pushPositionOf(form);
        error('e', message);
        popPositionOf(save);
    }

    public void errorIfNonEmpty(Object form) {
        if (form != LList.Empty)
            error('e', "invalid improper (dotted) list");
    }

  /** Set the line position of the argument to the current position. */

  public void setLineOf (Expression exp)
  {
    // "Special" QuoteExps may be shared, but the position gets set (in the
    // call to QuoteExp.getInstance at end of re-write) for normal ones.
    if (exp instanceof QuoteExp) 
      return;
    if (exp.getLineNumber() <= 0)
        exp.setLocation(this);
  }

  /** Extract a type from the car of a pair. */
  public Type exp2Type(Pair typeSpecPair)
  {
    return exp2Type(typeSpecPair, null, null);
  }

  public Type exp2Type(Pair typeSpecPair, Declaration decl, SyntaxForm syntax)
  {
    Object saved = pushPositionOf(typeSpecPair);
    try
      {
	Expression texp = rewrite_car(typeSpecPair, syntax);
	if (texp instanceof ErrorExp)
	  return null;
        Type type = getLanguage().getTypeFor(texp);
        if (type == null)
          {
            try
              {
                Object t = texp.eval(env);
                if (t instanceof Class)
                  type = Type.make((Class) t);
                else if (t instanceof Type)
                  type = (Type) t;
              }
            catch (Error ex)
              {
                throw ex;
              }
            catch (Throwable ex)
              {
              }
          }
        if (type == null)
	   {
	     if (texp instanceof ReferenceExp)
	       error('e', "unknown type name '"
		     + ((ReferenceExp) texp).getName() + '\'');
	     else
               error('e', "invalid type spec");
             type = Type.errorType;
	   }
        if (decl != null)
          decl.setType(texp, type);
        return type;
      }
    finally
      {
	popPositionOf(saved);
      }
  }

  public static Object wrapSyntax (Object form, SyntaxForm syntax)
  {
    if (syntax == null || form instanceof Expression)
      return form;
    else
      return SyntaxForms.fromDatumIfNeeded(form, syntax);
  }

  /** Pop from formStack all forms that come after beforeFirst.
   */
  public Values popForms(Pair beforeFirst)
  {
    Object tail = formStack.popTail(beforeFirst);
    if (tail == LList.Empty)
      return Values.empty;
    return new ValuesFromLList((LList) tail);
  }

  public void scanForm (Object st, ScopeExp defs)
  {
    if (st instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) st;
	ScopeExp save_scope = setPushCurrentScope(sf.getScope());
	try
	  {
	    Pair beforeFirst = formStack.last;
	    scanForm(sf.getDatum(), defs);
	    pushForm(wrapSyntax(popForms(beforeFirst), sf));
	    return;
	  }
	finally
	  {
	    setPopCurrentScope(save_scope);
	  }
      }
    if (st instanceof Values)
      {
	if (st == Values.empty)
	  st = QuoteExp.voidExp; // From #!void
        else if (st instanceof ValuesFromLList)
        {
          for (Object vs = ((ValuesFromLList) st).values;
               vs != LList.Empty; )
            {
              Pair p = (Pair) vs;
              Object save = pushPositionOf(p);
              scanForm(p.getCar(), defs);
              popPositionOf(save);
              vs = p.getCdr();
            }
        }
	else
	  {
	    Object[] vals = ((Values) st).getValues();
	    for (int i = 0;  i < vals.length;  i++)
	      scanForm(vals[i], defs);
	    return;
	  }
      }
    if (st instanceof Pair)
      {
        Pair st_pair = (Pair) st;
        Declaration saveContext = macroContext;
        Syntax syntax = null;
        ScopeExp savedScope = current_scope;
        Object savedPosition = pushPositionOf(st);
        if (st instanceof SourceLocator && defs.getLineNumber() < 0)
          defs.setLocation((SourceLocator) st);
        try
          {
            Object obj = st_pair.getCar();
            if (obj instanceof SyntaxForm)
              {
                SyntaxForm sf = (SyntaxForm) st_pair.getCar();
                savedScope = setPushCurrentScope(sf.getScope());
                obj = sf.getDatum();
              }
            Pair p;
            if (obj instanceof Pair
                && (p = (Pair) obj).getCar() == LispLanguage.lookup_sym
                && p.getCdr() instanceof Pair
                && (p = (Pair) p.getCdr()).getCdr() instanceof Pair)
              {
                Expression part1 = rewrite(p.getCar(), 'R');
                Expression part2 = rewrite_car_for_lookup((Pair) p.getCdr());
                Object value1 = part1.valueIfConstant();
                Object value2 = part2.valueIfConstant();
                if (value1 instanceof Class && value2 instanceof Symbol)
                  {
                    try
                      {
                        obj = GetNamedPart.getNamedPart(value1, (Symbol)value2);
                        if (obj instanceof Syntax)
                          syntax = (Syntax) obj;
                      }
                    catch (Exception ex)
                      {
                        obj = null;
                      }
                  }
                else
                  obj = namespaceResolve(part1, part2);
              }
            if (obj instanceof Symbol && ! selfEvaluatingSymbol(obj))
              {
                Expression func = rewrite(obj, 'M');
                if (func instanceof ReferenceExp)
                  {
                    Declaration decl = ((ReferenceExp) func).getBinding();
                    if (decl != null)
                      syntax = check_if_Syntax(decl);
                    else
                      {
                        obj = resolve(obj, true);
                        if (obj instanceof Syntax)
                          syntax = (Syntax) obj;
                      }
                  }
              }
            // Recognize deferred begin created in scanBody for pendingForms.
            // A seemingly-cleaner (obj instanceof Syntax) causes problems
            // with some Syntax forms, such as define.
            else if (obj == kawa.standard.begin.begin
                     || obj == kawa.standard.define_library.define_library_scan)
              syntax = (Syntax) obj;
          }
        finally
          {
              if (savedScope != current_scope)
                setPopCurrentScope(savedScope);
            popPositionOf(savedPosition);
          }
	if (syntax != null)
	  {
	    String save_filename = getFileName();
	    try
	      {
		syntax.scanForm(st_pair, defs, this);
		return;
	      }
	    finally
	      {
                macroContext = saveContext;
	      }
	  }
      }
    pushForm(st);
  }

  /** Recursive helper method for rewrite_body.
   * Scan body for definitions, adding partially macro-expanded
   * expressions into the <code>formStack</code>.
   * @param makeList if true, return a list representation of the scanned
   *   forms (not including declarations); else forms are push on formStack
   * @return a list of forms if <code>makeList</code> (possibly wrapped
   * in a <code>SyntaxForm</code>); otherwise <code>null</code>.
   */

  public LList scanBody (Object body, ScopeExp defs, boolean makeList)
  {
    LList list = makeList ? LList.Empty : null;
    Pair lastPair = null;
    while (body != LList.Empty)
      {
	if (body instanceof SyntaxForm)
	  {
	    SyntaxForm sf = (SyntaxForm) body;
	    ScopeExp save_scope = setPushCurrentScope(sf.getScope());
	    try
	      {
		Pair first = formStack.last;

		LList f = scanBody(sf.getDatum(), defs, makeList);
                if (makeList)
                  {
                    f = (LList) SyntaxForms.fromDatumIfNeeded(f, sf);
                    if (lastPair == null)
		      return f;
                    lastPair.setCdrBackdoor(f);
                    return list;
                  }
		pushForm(wrapSyntax(popForms(first), sf));
		return null;
	      }
	    finally
	      {
		setPopCurrentScope(save_scope);
	      }
	  }
	else if (body instanceof Pair)
	  {
	    Pair pair = (Pair) body;
	    Pair first = formStack.last;
            Object savePos = pushPositionOf(pair);
	    scanForm(pair.getCar(), defs);
            popPositionOf(savePos);
            if (getState() == Compilation.PROLOG_PARSED && pendingForm != null)
              {
                // We've seen a require form during the initial pass when
                // we're looking module names.  Defer the require and any
                // following forms in this body.
                if (pair.getCar() != pendingForm)
                  pair = makePair(pair, pendingForm, pair.getCdr());
                pendingForm = new Pair(kawa.standard.begin.begin, pair);
                if (makeList)
                  formStack.pushAll(list);
                return LList.Empty;
              }
	    if (makeList)
	      {
                Pair last = formStack.lastPair();
                LList nlist = (LList) formStack.popTail(first);
                if (lastPair == null)
		  list = nlist;
		else
		  lastPair.setCdrBackdoor(nlist);
                if (last != first)
                  lastPair = last;
	      }
	    body = pair.getCdr();
	  }
	else
	  {
	    pushForm(syntaxError("body is not a proper list"));
	    break;
	  }
      }
    return list;
  }

  public static Pair makePair(Pair pair, Object car, Object cdr)
  {
    if (pair instanceof PairWithPosition)
      return new PairWithPosition((PairWithPosition) pair, car, cdr);
    return new Pair(car, cdr);
  }

  /**
   * Re-write a Scheme 'body' in S-expression format into internal form.
   */

  public Expression rewrite_body (Object exp)
  {
    // NOTE we have both a rewrite_body and a rewriteBody.
    // This is confusing, at the least.  FIXME.
    Object saved = pushPositionOf(exp);
    LetExp defs = new LetExp();
    defs.setFlag(LetExp.IS_BODY_SCOPE);
    int renamedAliasOldSize = renamedAliasCount();
    Pair first = formStack.last;
    defs.setOuter(current_scope);
    current_scope = defs;
    try
      {
        LList list = scanBody(exp, defs, true);
	if (list.isEmpty())
	  pushForm(syntaxError("body with no expressions"));
        int ndecls = 0;
        for (Declaration decl = defs.firstDecl(); decl != null; decl = decl.nextDecl())
          {
            if (! decl.getFlag(Declaration.IS_DYNAMIC))
              {
                ndecls++;
                decl.setInitValue(QuoteExp.undefined_exp);
              }
          }
        rewriteBody(list);
        int renamedAliasNewSize = renamedAliasCount();
        popRenamedAlias(renamedAliasNewSize - renamedAliasOldSize);
	Expression body = makeBody(first, null);
	setLineOf(body);
	if (ndecls == 0)
	  return body;
	defs.setBody(body);
	setLineOf(defs);
	return defs;
      }
    finally
      {
	pop(defs);
	popPositionOf(saved);
      }
  }
  
  protected void rewriteBody (LList forms)
  {
    while (forms != LList.Empty)
      {
        Pair pair = (Pair) forms;
        Object saved = pushPositionOf(pair);
        try
          {
            rewriteInBody(pair.getCar());
          }
        finally
          {
            popPositionOf(saved);
          }
        forms = (LList) pair.getCdr();
      }
  }

  /** Combine a list of zero or more expression forms into a "body". */
  protected Expression makeBody(Pair head, ScopeExp scope)
  {
    Object tail = formStack.popTail(head);
    int nforms = LList.length(tail);
    if (nforms == 0)
      return QuoteExp.voidExp;
    Pair first = (Pair) tail;
     if (nforms == 1)
      {
	return (Expression) first.getCar();
      }
    else
      {
	Expression[] exps = new Expression[nforms];
        first.toArray(exps);
        if (scope instanceof ModuleExp)
	  return new ApplyExp(gnu.kawa.functions.AppendValues.appendValues,
			      exps);
	else
	  return makeBody(exps);
      }
  }

  public boolean appendBodyValues () { return false; }

  /** Combine a 'body' consisting of a list of expression. */
  public Expression makeBody(Expression[] exps)
  {
    if (appendBodyValues())
      return new ApplyExp(gnu.kawa.functions.AppendValues.appendValues, exps);
    else
      return new BeginExp (exps);
  }

  /** Storage used by noteAccess and processAccesses. */
  ArrayList notedAccess;

  /** Note that we reference name in a given scope.
   * This may be called when defining a macro, at scan-time,
   * and the name may be bound to a declaration we haven't seen yet. */
  public void noteAccess (Object name, ScopeExp scope)
  {
    if (notedAccess == null)
      notedAccess = new ArrayList();
    notedAccess.add(name);
    notedAccess.add(scope);
  }

  /** Check references recorded by noteAccess.
   * Resolve now to a Declaration, and note the access.
   * This is needed in case an exported macro references a private Declaration.
   */
  public void processAccesses ()
  {
    if (notedAccess == null)
      return;
    int sz = notedAccess.size();
    ScopeExp saveScope = current_scope;
    for (int i = 0;  i < sz;  i += 2)
      {
	Object name = notedAccess.get(i);
	ScopeExp scope = (ScopeExp) notedAccess.get(i+1);
	if (current_scope != scope)
          {
            // I.e. first time do equivalent of setPushCurrentScope
            if (current_scope == saveScope)
              lexical.pushSaveTopLevelRedefs();
	    setCurrentScope(scope);
          }
	Declaration decl =  (Declaration) lexical.lookup(name, -1);
	if (decl != null && ! decl.getFlag(Declaration.IS_UNKNOWN))
	  {
            decl.getContext().currentLambda().capture(decl);
	    decl.setCanRead(true);
            decl.setSimple(false);
	    decl.setFlag(Declaration.EXTERNAL_ACCESS);
	  }
      }
    if (current_scope != saveScope)
      setPopCurrentScope(saveScope);
  }

  public void finishModule(ModuleExp mexp)
  {
    boolean moduleStatic = mexp.isStatic();
    for (Declaration decl = mexp.firstDecl();
	 decl != null;  decl = decl.nextDecl())
      {
	if (decl.getFlag(Declaration.NOT_DEFINING))
	  {
	    String msg1 = "'";
	    String msg2
	      = (decl.getFlag(Declaration.EXPORT_SPECIFIED)
		 ? "' exported but never defined"
		 : decl.getFlag(Declaration.STATIC_SPECIFIED)
		 ? "' declared static but never defined"
		 : "' declared but never defined");
	    error('e', decl, msg1, msg2);
	  }
	if (mexp.getFlag(ModuleExp.EXPORT_SPECIFIED)
            || (generateMainMethod() && ! immediate))
	  {
	    if (decl.getFlag(Declaration.EXPORT_SPECIFIED))
	      {
		if (decl.isPrivate())
		  {
		    if (decl.getFlag(Declaration.PRIVATE_SPECIFIED))
		      error('e', decl,
			    "'", "' is declared both private and exported");
		    decl.setPrivate(false);
		  }
	      }
	    else if (! kawa.standard.IfFeature.isProvide(decl))
	      decl.setPrivate(true);
	  }
	if (moduleStatic)
	  decl.setFlag(Declaration.STATIC_SPECIFIED);
	else if ((mexp.getFlag(ModuleExp.NONSTATIC_SPECIFIED)
		  && ! decl.getFlag(Declaration.STATIC_SPECIFIED))
		 || gnu.expr.Compilation.moduleStatic < 0
		 || mexp.getFlag(ModuleExp.SUPERTYPE_SPECIFIED))
	  decl.setFlag(Declaration.NONSTATIC_SPECIFIED);
      }
    if (mexp.getFlag(ModuleExp.SUPERTYPE_SPECIFIED))
        mexp.setFlag(false, ModuleExp.USE_DEFINED_CLASS);
  }

  public void resolveModule(ModuleExp mexp)
  {
    Expression savePos = new ReferenceExp((Object) null);
    int numPending = pendingImports == null ? 0 : pendingImports.size();
    for (int i = 0;  i < numPending;  )
      {
        ModuleInfo info = (ModuleInfo) pendingImports.elementAt(i++);
        ScopeExp defs = (ScopeExp) pendingImports.elementAt(i++);
        Expression posExp = (Expression) pendingImports.elementAt(i++);
        Pair beforeGoal = (Pair) pendingImports.elementAt(i++);
        DeclSetMapper mapper = (DeclSetMapper) pendingImports.elementAt(i++);
        if (mexp == defs)
          {
            // process(BODY_PARSED);
            savePos.setLine(this);
            setLine(posExp);
            Pair beforeImports = formStack.last;
            kawa.standard.require.importDefinitions(null, info, mapper,
                                                    formStack, defs, this);
            if (beforeGoal != beforeImports
                && beforeImports != formStack.last)
              {
                // Move forms derived from the import forwards in the list,
                // just following beforeGoal.
                Object firstGoal = beforeGoal.getCdr();
                Object firstImports = beforeImports.getCdr();
                beforeGoal.setCdrBackdoor(firstImports);
                formStack.last.setCdrBackdoor(firstGoal);
                beforeImports.setCdrBackdoor(LList.Empty);
                formStack.last = beforeImports;
              }
            setLine(savePos);
          }
      }
    pendingImports = null;
    setModule(mexp);

    savePos.setLine(this);
    setLine(null, -1, -1);
    Compilation save_comp = Compilation.setSaveCurrent(this);
    try
      {
        Pair firstForm = formStack.getHead();
        rewriteBody((LList) formStack.popTail(firstForm));
	mexp.body = makeBody(firstForm, mexp);

        processAccesses();

        // In immediate mode need to preserve Declaration for current "session".
        if (! immediate)
	  lexical.pop(mexp);

        // Patch up renamed exports - see export.
        for (Declaration decl = mexp.firstDecl();  decl != null;
             decl = decl.nextDecl()) {
            if (decl.getSymbol() == null
                && decl.getFlag(Declaration.EXPORT_SPECIFIED)) {
                decl.patchSymbolFromSet();
            }
        }
      }
    finally
      {
	Compilation.restoreCurrent(save_comp);
        setLine(savePos);
      }

    /* DEBUGGING:
    OutPort err = OutPort.errDefault ();
    err.print ("[Re-written expression for load/compile: ");
    mexp.print (err);
    //err.print ("\nbefore load<"+mod.getClass().getName()+">");
    err.println();
    err.flush();
    */
  }

  public Declaration makeRenamedAlias (Declaration decl,
				       ScopeExp templateScope)
  {
    if (templateScope == null)
      return decl; // ???
    return makeRenamedAlias(decl.getSymbol(), decl, templateScope);
  }

  public Declaration makeRenamedAlias (Object name,
				       Declaration decl,
				       ScopeExp templateScope)
  {
    Declaration alias = new Declaration(name);
    alias.setAlias(true);
    alias.setPrivate(true);
    alias.context = templateScope;
    ReferenceExp ref = new ReferenceExp(decl);
    ref.setDontDereference(true);
    alias.noteValue(ref);
    return alias;
  }

  /** Push an alias for a declaration in a scope.
   * If the name of {@code decl} came from a syntax template
   * whose immediate scope is {@code templateScope},
   * then the same syntax template may contain local variable references
   * that are also in the same {@code templateScope}.
   * Such variable references will <em>not</em> look in the current
   * "physical" scope, where we just created {@code decl}, but
   * will instead search the "lexical" {@code templateScope}.
   * So that such references can resolve to {@code decl}, we
   * create an alias in {@code templateScope} that points
   * to {@code decl}.  We record that we did this in the
   * {@code renamedAliasStack}, so we can remove the alias later.
   */
  public void pushRenamedAlias (Declaration alias)
  {
    Declaration decl = getOriginalRef(alias).getBinding();
    ScopeExp templateScope = alias.context;
    decl.setSymbol(null);
    Declaration old = templateScope.lookup(alias.getSymbol());
    if (old != null)
      templateScope.remove(old);
    templateScope.addDeclaration(alias);
    if (renamedAliasStack == null)
      renamedAliasStack = new Stack<Declaration>();
    renamedAliasStack.push(old);
    renamedAliasStack.push(alias);
  }

    public int renamedAliasCount() {
        return renamedAliasStack == null ? 0 : renamedAliasStack.size() >> 1;
    }

  /** Remove one or more aliases created by <code>pushRenamedAlias</code>. */
  public void popRenamedAlias (int count)
  {
    while (--count >= 0)
      {
	Declaration alias = (Declaration) renamedAliasStack.pop();
        ScopeExp templateScope = alias.getContext();
	Declaration decl = getOriginalRef(alias).getBinding();
	decl.setSymbol(alias.getSymbol());
	templateScope.remove(alias);
	Declaration old = renamedAliasStack.pop();
	if (old != null)
	  templateScope.addDeclaration(old);
      }
  }

    public Declaration define(Object name, ScopeExp defs) {
        return define(name, (TemplateScope) null, defs);
    }

    public Declaration define(Object name, SyntaxForm nameSyntax,
                              ScopeExp defs) {
        return define(name, nameSyntax == null ? null : nameSyntax.getScope(),
                      defs);
    }

    public Declaration define(Object name, TemplateScope templateScope,
                              ScopeExp defs) {
        ScopeExp scope = templateScope != null ? templateScope
            : currentScope();
        boolean aliasNeeded = scope != defs;
        Object declName = aliasNeeded
            ? Symbol.makeUninterned(name.toString())
            : name;
        Declaration decl = defs.getDefine(declName, this);
        if (aliasNeeded) {
            Declaration alias = makeRenamedAlias(name, decl, scope);
            if (defs instanceof LetExp)
                pushRenamedAlias(alias);
            else
                scope.addDeclaration(alias);
        }
        push(decl);
        return decl;
    }

  static boolean isObjectSyntax (ClassType declaringClass, String fieldName)
  {
    return "objectSyntax".equals(fieldName)
      &&  "kawa.standard.object".equals(declaringClass.getName());
  }

    public FormStack formStack = new FormStack(this);
    public void pushForm(Object value) { formStack.push(value); }
  
    /** A list of "forms" to be further processed.
     * It is implemented as an LList so we can save position information.
     */
    public static class FormStack extends Pair {
        private Pair last = this;
        SourceLocator sloc;
    
        public FormStack(SourceLocator sloc) {
            this.sloc = sloc;
            this.cdr = LList.Empty;
        }

        /** Return the "head" of the list.
         * The cdr of the head is the first element.
         */
        public Pair getHead() { return this; }
        public Object getFirst() { return cdr; }
        /** The Pair whose car is the last form in the list.
         * If the list is empty, this returns the list head.
         */
        @Override
        public Pair lastPair() { return last; }

        /* DEBUGGING:
        public void dump() {
            int i=0;
            System.err.println("formStack len:"+LList.length(getFirst()));
            for(Object x = getFirst(); x instanceof Pair; i++) {
                Pair p = (Pair) x;
                //if (! (p.getCar() instanceof SetExp))
                    System.err.println("- #"+i+": "+p.getCar());
                x = p.getCdr();
            }
        }
        */

        public Object popTail(Pair oldTail) {
            Object r = oldTail.getCdr();
            oldTail.setCdrBackdoor(LList.Empty);
            last = oldTail;
            return r;
        }
    
        public void push(Object value) {
            PairWithPosition pair = new PairWithPosition(sloc, value, LList.Empty);
            last.setCdrBackdoor(pair);
            last = pair;
        }

        public void pushAll(LList values) {
            if (values == LList.Empty)
                return;
            last.setCdrBackdoor(values);
            last = ((Pair) values).lastPair();
        }
    
        public void pushAll(LList values, Pair valuesLast) {
            if (values == LList.Empty)
                return;
            last.setCdrBackdoor(values);
            last = valuesLast;
        }
    
        public void pushAfter(Object value, Pair position) {
            Pair pair = new PairWithPosition(sloc, value, position.getCdr());
            position.setCdrBackdoor(pair);
            if (last == position)
                last = pair;
        }
    }

    /** An implementationof Values using a linked list.
     */
    public static class ValuesFromLList extends Values.FromList<Object> {
        public LList values;

        public ValuesFromLList(LList values) {
            super(values);
            this.values = values;
        }
    }

    Stack<ScanContext> scanContextStack = new Stack<ScanContext>();

    public ScanContext getScanContext() { return scanContextStack.peek(); }
    public int curScanNesting() { return scanContextStack.size(); }
    public Stack<ScanContext> getScanContextStack() { return scanContextStack; }

    public void pushScanContext (LambdaExp lambda) {
        ScanContext newContext = new ScanContext();
        newContext.lambda = lambda;
        scanContextStack.push(newContext);
    }
    public void popScanContext() {
        scanContextStack.pop();
    }
    public static class ScanContext {
        LinkedHashMap<Declaration,Declaration> decls
            = new LinkedHashMap<Declaration,Declaration>();
        ArrayList<Expression> scanExpressions = null;
        LambdaExp lambda;

        public LambdaExp getLambda() { return lambda; }

        public Declaration addSeqDecl(Declaration scanVar) {
            Declaration param = decls.get(scanVar);
            if (param == null) {
                param = lambda.addParameter(null);
                decls.put(scanVar, param);
            }
            return param;
        }
        public void addSeqExpression(Expression scanExp) {
            if (scanExpressions == null)
                scanExpressions = new ArrayList<Expression>();
            scanExpressions.add(scanExp);
        }
    }
}
