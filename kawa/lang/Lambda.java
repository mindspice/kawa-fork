package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes the lambda builtin.
 * @author	Per Bothner
 */

public class Lambda extends Syntax implements Printable
{
  static private Pattern pattern = new VarListPat (1);

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return tr.syntaxError ("missing formals in lambda");
    int old_errors = tr.errors;
    LambdaExp lexp = new LambdaExp();
    rewrite(lexp, match[0], match[1], tr);
    if (tr.errors > old_errors)
      return new ErrorExp("bad lambda expression");
    return lexp;
  }

  /**
   * Higher-level constructor, that does the re-writing.
   * @param formals the formal parameter list (or symbol)
   * @param body the body of the procedure
   * @param tr the (Scheme) Translator
   */
  // FIXME make method of Translator
  public static void rewrite(LambdaExp lexp, Object formals, Object body, Translator tr)
  {
    /* Count formals, while checking that the syntax is OK. */
    Object bindings = formals;
    int opt_args = -1;
    int rest_args = -1;
    int key_args = -1;
    Pair pair;
    for (; bindings instanceof Pair;  bindings = pair.cdr)
      {
	pair = (Pair) bindings;
	if (pair.car == Special.optional)
	  {
	    if (opt_args >= 0)
	      {
		tr.syntaxError ("multiple #!optional in parameter list");
		return;
	      }
	    else if (rest_args >= 0 || key_args >= 0)
	      {
		tr.syntaxError ("#!optional after #!rest or #!key");
		return;
	      }
	    opt_args = 0;
	  }
	else if (pair.car == Special.rest)
	  {
	    if (rest_args >= 0)
	      {
		tr.syntaxError ("multiple #!rest in parameter list");
		return;
	      }
	    else if (key_args >= 0)
	      {
		tr.syntaxError ("#!rest after #!key");
		return;
	      }
	    rest_args = 0;
	  }
	else if (pair.car == Special.key)
	  {
	    if (key_args >= 0)
	      {
		tr.syntaxError ("multiple #!key in parameter list");
		return;
	      }
	    key_args = 0;
	  }
	else if (key_args >= 0)
	  key_args++;
	else if (rest_args >= 0)
	  rest_args++;
	else if (opt_args >= 0)
	  opt_args++;
	else
	  lexp.min_args++;
	bindings = pair.cdr;
      }
    if (bindings instanceof String)
      {
	if (opt_args >= 0 || key_args >= 0 || rest_args >= 0)
	  {
	    tr.syntaxError ("dotted rest-arg after #!optional, #!rest, or #!key");
	    return;
	  }
	rest_args = 1;
      }
    else if (bindings != List.Empty)
      {
	tr.syntaxError ("misformed formals in lambda");
	return;
      }
    if (rest_args > 1)
      {
	tr.syntaxError ("multiple #!rest parameters");
        return;
      }
    if (opt_args < 0)
      opt_args = 0;
    if (rest_args < 0)
      rest_args = 0;
    if (key_args < 0)
      key_args = 0;
    if (rest_args > 0)
      lexp.max_args = -1;
    else   // Is this useful?
      lexp.max_args = lexp.min_args + opt_args + 2 * key_args;
    if (opt_args + key_args > 0)
      lexp.defaultArgs = new Expression[opt_args + key_args];
    if (key_args > 0)
      lexp.keywords = new Keyword[key_args];

    tr.push(lexp);
    bindings = formals;
    int i = 0;
    opt_args = 0;
    key_args = 0;
    Object mode = null;
    for (; bindings instanceof Pair;  bindings = pair.cdr)
      {
	pair = (Pair) bindings;
	if (pair.car == Special.optional
	    || pair.car == Special.rest || pair.car == Special.key)
	  {
	    mode = pair.car;
	    continue;
	  }
	String name;
	Object defaultValue = QuoteExp.falseExp;
	Object typeSpec = null;
	if (pair.car instanceof String)
	  name = (String) pair.car;
	else if (pair.car instanceof Pair
		 && ((Pair) pair.car).car instanceof String
		 && ((Pair) pair.car).cdr instanceof Pair)
	  {
	    Pair pair_car = (Pair) pair.car;
	    name = (String) pair_car.car;
	    if (mode == null)
	      typeSpec = ((Pair) pair_car.cdr).car;
	    else if (mode != Special.rest)
	      {
		Pair defaultPair = (Pair) pair_car.cdr;
		defaultValue = defaultPair.car;
		if (defaultPair.cdr instanceof Pair)
		  typeSpec = ((Pair) defaultPair.cdr).car;
	      }
	    else
	      {
		tr.syntaxError ("default value for #!rest parameter");
		
		return;
	      }
	  }
	else
	  {
	    tr.syntaxError ("parameter is neither name nor (name default)");
	    return;
	  }
	if (mode == Special.optional || mode == Special.key)
	  lexp.defaultArgs[opt_args++] = tr.rewrite(defaultValue);
	if (mode == Special.key)
	  lexp.keywords[key_args++] = Keyword.make(name.toString());
	Declaration decl = lexp.addDeclaration (name);
	decl.setParameter(true);
	if (typeSpec != null)
	  decl.setType(kawa.standard.prim_method.exp2Type(typeSpec, tr));
	decl.noteValue(null);  // Does not have a known value.
	tr.push(decl);
      }
    if (bindings instanceof String)
      {
	Declaration decl = lexp.addDeclaration ((String) bindings);
	decl.setParameter (true);
	decl.noteValue (null);  // Does not have a known value.
	tr.push(decl);
      }
    if (body instanceof PairWithPosition)
      lexp.setFile(((PairWithPosition) body).getFile());

    // Syntatic sugar:  <TYPE> BODY --> (as <TYPE> BODY)
    lexp.body = tr.rewrite_body (body);
    if (lexp.body instanceof BeginExp)
      {
	BeginExp bexp = (BeginExp) lexp.body;
	Expression[] exps = bexp.getExpressions();
	int len = exps.length;
	if (len > 1)
	  {
	    Expression rexp = exps[0];
	    gnu.bytecode.Type rtype
	      = kawa.standard.Scheme.getTypeValue(rexp);
	    if (rtype != null)
	      {
		if (len > 2)
		  {
		    len--;
		    Expression[] new_body = new Expression[len];
		    System.arraycopy(exps, 1, new_body, 0, len);
		    exps = new Expression[2];
		    exps[0] = rexp;
		    exps[1] = new BeginExp(new_body);
		  }
		QuoteExp c = new QuoteExp(kawa.standard.convert.getInstance());
		lexp.body = new ApplyExp(c, exps);
	      }
	  }
      }
    tr.pop(lexp);
  }


  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<builtin lambda>");
  }
}
