// Copyright (c) 2001, 2004, 2005  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.text.*;
import gnu.lists.*;
import gnu.mapping.EnvironmentKey;
import gnu.kawa.reflect.StaticFieldLocation;
import kawa.lang.Translator; // FIXME

/** Language sub-class for Lisp-like languages (including Scheme). */

public abstract class LispLanguage extends Language
{
  static public final String quote_sym = "quote";
  static public final String unquote_sym = "unquote";
  static public final String unquotesplicing_sym = "unquote-splicing";
  static public final String quasiquote_sym = "quasiquote";

  /** The default <code>ReadTable</code> for this language. */
  public ReadTable defaultReadTable = createReadTable();

  /** Create a fresh <code>ReadTable</code> appropriate for this language. */
  public abstract ReadTable createReadTable ();

  public Compilation parse(Lexer lexer, int options)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    SourceMessages messages = lexer.getMessages();
    kawa.lang.Translator tr = new Translator (this, messages);
    tr.immediate = (options & PARSE_IMMEDIATE) != 0;
    ModuleExp mexp = new ModuleExp();
    if (Compilation.generateAppletDefault)
      mexp.setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
    String fname = lexer.getName();
    mexp.setFile(fname);
    Values forms = new Values();
    tr.push(mexp);
    int first = tr.formStack.size();
    LispReader reader = (LispReader) lexer;
    Compilation save_comp = Compilation.getCurrent();
    try
      {
        Compilation.setCurrent(tr);
        for (;;)
          {
            Object sexp = reader.readCommand();
            if (sexp == Sequence.eofValue)
              {
                if ((options & PARSE_ONE_LINE) != 0)
                  return null;  // FIXME
                break;
              }
            tr.scanForm(sexp, mexp);
            if ((options & PARSE_ONE_LINE) != 0)
              break;
          }
        if (lexer.peek() == ')')
          lexer.fatal("An unexpected close paren was read.");
        tr.finishModule(mexp, first);
      }
    finally
      {
        Compilation.setCurrent(save_comp);
      }
    return tr;
  }

  /** Declare in the current Environment a Syntax bound to a static field.
   * @param name the procedure's source-level name.
   * @param cname the name of the class containing the field.
   * @param fname the name of the field, which should be a static
   *   final field whose type extends kawa.lang.Syntax.
   */
  protected void defSntxStFld(String name, String cname, String fname)
  {
    Object property
      = hasSeparateFunctionNamespace() ? EnvironmentKey.FUNCTION : null;
    StaticFieldLocation loc = 
      StaticFieldLocation.define(environ, environ.getSymbol(name), property,
				 cname, fname);
    loc.setSyntax();
  }

  protected void defSntxStFld(String name, String cname)
  {
    defSntxStFld(name, cname, Compilation.mangleNameIfNeeded(name));
  }

  /** Combine a <body> consisting of a list of expression. */
  public Expression makeBody(Expression[] exps)
  {
    return new BeginExp (exps);
  }

  public Expression makeApply (Expression func, Expression[] args)
  {
    return new ApplyExp(func, args);
  }

  public boolean selfEvaluatingSymbol (Object obj)
  {
    return obj instanceof Keyword;
  }

  /** Convert the Language's idea of a symbol to a gnu.mapping.Symbol. */
  public static Symbol langSymbolToSymbol (Object sym)
  {
    return ((LispLanguage) Language.getDefaultLanguage()).fromLangSymbol(sym);
  }

  protected Symbol fromLangSymbol (Object sym)
  {
    if (sym instanceof String)
      return getSymbol((String) sym);
    return (Symbol) sym;
  }
}
