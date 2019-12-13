package kawa.lang;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.text.Char;
import java.util.Stack;
import java.util.Vector;
import gnu.mapping.Environment;
import gnu.mapping.Symbol;
import gnu.kawa.functions.Convert;
import gnu.kawa.lispexpr.SeqSizeType;
import gnu.kawa.lispexpr.LangObjType;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.kawa.lispexpr.LispLanguage;
import gnu.kawa.reflect.MappedArrayType;
import gnu.mapping.Procedure;
import kawa.standard.Scheme;

/** Methods for parsing patterns. */

public class BindDecls {
    public static final BindDecls instance = new BindDecls();

    public boolean allowShadowing = false;

    public boolean makeConstant = true;

    public Object ifKeyword = Special.ifk;
    public Procedure compareEquals = kawa.standard.Scheme.isEqual;
    public Type booleanType = kawa.standard.Scheme.booleanType;

    static final Symbol underScoreSymbol = Symbol.valueOf("_");

    public Declaration define(Symbol name, TemplateScope templateScope,
                              ScopeExp scope, Translator comp) {
        Declaration oldDecl = comp.lexical.lookup(name, false);
        Declaration decl = comp.define(name, templateScope, scope);
        if (! allowShadowing
            && oldDecl != null
            && oldDecl.context != scope
            && ! (oldDecl.context instanceof ModuleExp)) {
            comp.error('w', decl, "new declaration '", "' shadows old declaration");
            comp.error('w', oldDecl, "(this is the previous declaration of '", "')");
        }
        return decl;
    }

    public Object parsePatternNext(Pair patList, Translator comp) {
        Object next = patList.getCdr();
        if (next instanceof Pair) {
            Pair nextPair = (Pair) next;
            if (comp.matches(nextPair.getCar(), "::")) {
                Object nextCdr = nextPair.getCdr();
                if (nextCdr instanceof Pair) {
                    next = ((Pair) nextCdr).getCdr();
                }
                else { // Error
                    next = nextCdr;
                }
            }
        }
        return next;
    }

    /** Parse a declaration or more generally a pattern.
     * The actual pattern is an initial sublist (using just the initial
     * car) of the patList.
     * @return A 2-element array, where element 0 is the unused remainder
     *   of patList, while element 1 is a Declaration for that pattern.
     */
    public Object[] parsePatternCar(Pair patList, int scanNesting,
                                    ScopeExp scope,
                                    Translator comp) {
        return parsePatternCar(patList, null, null, scanNesting, scope, comp);
    }
    public Object[] parsePatternCar(Pair patList, Expression init,
                                    TemplateScope templateScope,
                                    int scanNesting, ScopeExp scope,
                                    Translator comp) {
        Object next = patList.getCdr();
        Type type = null;
        if (next instanceof Pair) {
            Pair nextPair = (Pair) next;
            if (comp.matches(nextPair.getCar(), "::")) {
                Object nextCdr = nextPair.getCdr();
                if (nextCdr instanceof Pair) {
                    Pair nextCdrPair = (Pair) nextCdr;
                    type = comp.exp2Type(nextCdrPair);
                    next = nextCdrPair.getCdr();
                }
                else {
                    Object saveLoc = comp.pushPositionOf(nextPair);
                    comp.error('e', "missing type after '::'");
                    comp.popPositionOf(saveLoc);
                    next = nextCdr;
                }
            }
        }
        Object pattern = patList.getCar();
        Object saveLoc = comp.pushPositionOf(patList);

        Object patval = pattern;
        while (patval instanceof SyntaxForm) {
            SyntaxForm patSyntax = (SyntaxForm) patval;
            templateScope = patSyntax.getScope();
            patval = patSyntax.getDatum();
        }
        patval = comp.namespaceResolve(patval);
        Declaration decl = null;

        QuoteExp literal = literalPattern(patval, comp);
        if (literal != null) {
            decl = scope.addDeclaration((Object) null);
            addCondition(scope, compareLiteral(decl, literal));
        } else if (patval instanceof Symbol) {
            if (patval == underScoreSymbol) {
                decl = scope.addDeclaration((Object) null);
            } else {
                decl = define((Symbol) patval, templateScope, scope, comp);
                Translator.setLine(decl, patList);
            }
            if (init != null)
                setInitializer(decl, init, scope, comp);
            if (scope instanceof ModuleExp
                && (patval == underScoreSymbol
                    || ! (scope.getFlag(ModuleExp.INTERACTIVE)
                          || comp.sharedModuleDefs())))
                decl.setPrivate(true);
            if (makeConstant)
                decl.setFlag(Declaration.IS_CONSTANT);
            decl.setFlag(Declaration.IS_SINGLE_VALUE);
        } else if (patval == ifKeyword) {
            if (next instanceof Pair) {
                Pair nextPair = (Pair) next;
                decl = addCondition(scope, nextPair.getCar());
                next = nextPair.getCdr();
            } else {
                comp.error('e', "missing expression after "+ifKeyword);
            }
        } else if (pattern instanceof Pair) {
            Pair patpair = (Pair) pattern;
            Object patcar = patpair.getCar();
            if (patcar == LispLanguage.bracket_list_sym) {
                decl = scope.addDeclaration((Object) null);
                if (init != null)
                    setInitializer(decl, init, scope, comp);
                if (type != null)
                    ; // FIXME
                decl.setPrivate(true);
                decl.setFlag(Declaration.IS_CONSTANT
                             |Declaration.SKIP_FOR_METHOD_PARAMETER
                             |Declaration.IS_SINGLE_VALUE);
                // FIXME pass templateScope?
                parseBracketListPattern(patpair, scanNesting, scope, decl, comp);
            }
            /*else if (patcar == LispLanguage.bracket_quote_sym)
              ....
            */
            else if (patcar == LispLanguage.splice_sym
                     || patcar == LispLanguage.splice_colon_sym) {
                Object patcdr = patpair.getCdr();
                if (Translator.listLength(patcdr) != 1)
                    comp.syntaxError("bad syntax for splice pattern cdr:"+patcdr);
                else {
                    Object[] r = parsePatternCar((Pair) patcdr, null,
                                                 templateScope,
                                                 scanNesting, scope, comp);
                    decl = (Declaration) r[1];
                    decl.setFlag(Declaration.IS_REST_PARAMETER);
                    boolean keywordsOk =
                        patcar == LispLanguage.splice_colon_sym;
                    if (keywordsOk) {
                        decl.setFlag(Declaration.KEYWORDS_OK);
                        if (scope instanceof LambdaExp)
                            scope.setFlag(LambdaExp.ALLOW_OTHER_KEYWORDS);
                    }
                    if (! decl.getFlag(Declaration.TYPE_SPECIFIED)) {
                        decl.setType(keywordsOk ? LangObjType.argVectorType
                                     : ArrayType.make(Type.objectType));
                    }
                }
            } else
                comp.syntaxError("unrecognized pattern operator "+patcar);
        }
        else
            comp.error('e', "unrecognized pattern "+pattern);
        if (decl != null) {
            decl.setScanNesting(scanNesting);
            if (type != null) {
                decl.setType(MappedArrayType.maybe(type, scanNesting));
                decl.setFlag(Declaration.TYPE_SPECIFIED);
            }
        }
        comp.popPositionOf(saveLoc);
        return new Object[]{next,decl};
    }

    /** Handle patterns of the form {@code [pat1 ... patN]}.
     */
    public void parseBracketListPattern
        (Pair patpair, int scanNesting, ScopeExp scope, Declaration decl, Translator comp) {
        ClassType listType = ClassType.make("java.util.List");
        decl.setFlag(Declaration.SKIP_FOR_METHOD_PARAMETER);
        if (decl.getTypeExpRaw() != null) {
            Declaration d = scope.addDeclaration((Object) null);
            d.setFlag(Declaration.PATTERN_NESTED|Declaration.SKIP_FOR_METHOD_PARAMETER);
            d.setScanNesting(scanNesting);
            setInitializer(d, new ReferenceExp(decl), scope, comp);
            decl = d;
        }
        int count = 0;
        Object cdr = patpair.getCdr();
        int ellipsisCount = 0;
        int spliceCount = 0;
        for (;; count++) {
            if (cdr == LList.Empty)
                break;
            if (! (cdr instanceof Pair))
                break;  // FIXME ERROR - or handle "rest" pattern
            patpair = (Pair) cdr;
            boolean sawEllipsis = false;
            boolean sawSplice = false;
            int curScanNesting = scanNesting;
            cdr = parsePatternNext(patpair, comp);
            if (cdr instanceof Pair) {
                Object nextCar = ((Pair) cdr).getCar();
                Object ellipsis = SyntaxRule.dots3Symbol;
                if (SyntaxPattern.literalIdentifierEq(nextCar, null/*FIXME*/, ellipsis, null)) {
                    sawEllipsis = true;
                    curScanNesting++;
                    ellipsisCount++;
                    cdr = ((Pair) cdr).getCdr();
                }
            }
            Object curCar = patpair.getCar();
            if (Translator.listLength(curCar) == 2) {
                Object nextCaar = ((Pair) curCar).getCar();
                if (nextCaar == LispLanguage.splice_sym
                    || nextCaar == LispLanguage.splice_colon_sym) {
                    sawSplice = true;
                    spliceCount++;
                    patpair = (Pair) ((Pair) curCar).getCdr();
                }
            }
            Expression init;
            if (sawEllipsis || sawSplice) {
                // FIXME restCount mishandles 'ID :: TYPE', for example.
                int restCount = Translator.listLength(cdr);
                Method dropMethod = ClassType.make("gnu.lists.Sequences")
                    .getDeclaredMethod("drop", restCount==0 ? 2 : 3);
                Expression[] args = new Expression[restCount==0 ? 2 : 3];
                args[0] = new ReferenceExp(decl);
                args[1] = new QuoteExp(count, Type.intType);
                if (restCount != 0)
                    args[2] = new QuoteExp(restCount, Type.intType);
                init = new ApplyExp(dropMethod, args);
            } else {
                // FIXME Probably better to use an Iterator or "position indexes"
                Method indexMethod;
                int index;
                if (ellipsisCount + spliceCount > 0) {
                    index = -1 - Translator.listLength(cdr);
                    indexMethod = ConsumerTarget.typeSequences
                        .getDeclaredMethod("getAt", 2);
                } else {
                    index = count;
                    indexMethod = listType
                        .getMethod("get", new Type[] { Type.intType  });
                }
                init = new ApplyExp(indexMethod, new Expression[] {
                        new ReferenceExp(decl),
                        new QuoteExp(index, Type.intType) });
            }
            if (scanNesting > 0)
                init = mapInit(init, decl);
            Object[] r = parsePatternCar(patpair, init, null, curScanNesting,
                                         scope, comp);
            //r[0] is ingnored, instead we use parsePatternNext
            Declaration d = (Declaration) r[1];
            d.setScanNesting(curScanNesting);
            d.setFlag(Declaration.PATTERN_NESTED);
            if (sawEllipsis)
                d.setFlag(Declaration.SCAN_OWNER);
        }
        if (ellipsisCount+spliceCount > 1)
            comp.error('e', "more than one '...' or '@' in a pattern not supported");
        Type seqType = new SeqSizeType(count-ellipsisCount-spliceCount,
                                       ellipsisCount+spliceCount==0);
        decl.setType(MappedArrayType.maybe(seqType, scanNesting));
    }

    public static void setInitializer(Declaration decl, Expression init, ScopeExp scope, Translator comp) {
        if ((scope instanceof ModuleExp)
            || (scope instanceof LetExp
                && scope.getFlag(LetExp.IS_BODY_SCOPE))) {
            SetExp sexp = new SetExp(decl, init);
            comp.pushForm(sexp);
            decl.noteValueFromSet(sexp);
        }
        else {
            decl.setInitValue(init);
            decl.noteValueFromLet(scope);
        }
    }

    static class ReplaceDecl extends ExpExpVisitor<Void> {
        Declaration oldDecl;
        Declaration newDecl;
        protected Expression visitReferenceExp(ReferenceExp exp, Void ignored) {
            if (exp.getBinding() == oldDecl)
                exp.setBinding(newDecl);
            return exp;
        }
    }

    static Expression mapInit(Expression init, Declaration decl) {
        LambdaExp lambda = new LambdaExp();
        Declaration param = lambda.addParameter(null);
        ReplaceDecl v = new ReplaceDecl();
        v.oldDecl = decl;
        v.newDecl = param;
        v.visit(init, null);
        lambda.body = init;
        return new ApplyExp(Scheme.map,
                            lambda,
                            new ReferenceExp(decl));
    }

    Declaration addCondition(ScopeExp scope, Object condition) {
        Declaration decl = scope.addDeclaration((Object) null);
        Expression cond;
        if (condition instanceof Expression)
            cond = Compilation.makeCoercion((Expression) condition, booleanType);
        else
            cond = new LangExp(LList.list3(new QuoteExp(Convert.cast),
                                           new QuoteExp(booleanType),
                                           condition));
        decl.setInitValue(cond);
        decl.setFlag(Declaration.PATTERN_NESTED|Declaration.SKIP_FOR_METHOD_PARAMETER);
        decl.setType(QuoteExp.isTrueTypeExp, LangPrimType.isTrueType);
        return decl;
    }

    public QuoteExp literalPattern(Object patval, Translator comp) {
        if (patval instanceof Number
            || patval == null
            || patval instanceof Character
            || patval instanceof Char
            || patval instanceof Boolean
            || patval instanceof CharSequence)
            return QuoteExp.getInstance(patval);
        if (patval instanceof Pair) {
            Pair p1 = (Pair) patval;
            Object p1cdr = p1.getCdr();
            if (comp.matches(p1.getCar(), "quote")
                && p1cdr instanceof Pair) {
                Pair p2 =  (Pair) p1cdr;
                if (p2.getCdr() == LList.Empty)
                    return QuoteExp.getInstance(p2.getCar());
            }
        }
        return null;
    }
    public Expression compareLiteral(Declaration param, QuoteExp literal) {
        return new ApplyExp(compareEquals,
                            new ReferenceExp(param), literal);
    }
}
