// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.*;
import gnu.lists.*;

/** Interpreter sub-class for Lisp-like languages (including Scheme). */

public abstract class LispInterpreter extends Interpreter
{
  public ModuleExp parse(Environment env, Lexer lexer)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    gnu.text.SourceMessages messages = lexer.getMessages();
    kawa.lang.Translator tr = new kawa.lang.Translator(env, messages);
    tr.immediate = true;
    lexer.clearErrors();
    PairWithPosition body
      = PairWithPosition.make(null, LList.Empty,
			      lexer.getName(),
			      lexer.getLineNumber() + 1,
			      lexer.getColumnNumber() + 1);
    Object sexp = ((LispReader) lexer).readObject(); // FIXME
    if (sexp == Sequence.eofValue)
      return null; // FIXME
    
    body.car = sexp;
    /* If the errors were minor, we could perhaps try to
       do Translation (to check for more errors)  .  ??? */
    return kawa.standard.Scheme.makeModuleExp(body, tr);
  }

  public ModuleExp parseFile (InPort port, gnu.text.SourceMessages messages)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    kawa.lang.Translator tr = new  kawa.lang.Translator (environ, messages);
    ModuleExp mexp = new ModuleExp();
    if (Compilation.generateAppletDefault)
      mexp.setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
    mexp.setFile(port.getName());
    java.util.Vector forms = new java.util.Vector(20);
    tr.push(mexp);
    LispReader lexer = (LispReader) getLexer(port, messages);
    for (;;)
      {
	Object sexp = lexer.readObject(); // FIXME
	if (sexp == Sequence.eofValue)
	  break;
	if (! tr.scan_form (sexp, forms, mexp))
	  break;
      }
    if (port.peek() == ')')
      lexer.fatal("An unexpected close paren was read.");
    tr.finishModule(mexp, forms);
    return mexp;
  }

  /** Combine a <body> consisting of a list of expression. */
  public Expression makeBody(Expression[] exps)
  {
    return new BeginExp (exps);
  }

}
