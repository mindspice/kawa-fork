package kawa.standard;
import kawa.lang.*;

/** Implement the Scheme standard functions "map" and "for-each".
 * @author Per Bothner
 */

public class map  extends ProcedureN
{
  /** True if we should collect the result into a list. */
  boolean collect;

  public map (boolean collect)
  {
    super (collect ? "map" : "for-each");
    this.collect = collect;
  }

  /** An optimized single-list version of map. */
  static public Object map1 (Procedure proc, Object list)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Object result = List.Empty;
    Pair last = null;
    while (list != List.Empty)
      {
	Pair pair = (Pair) list;
	Pair new_pair = new Pair (proc.apply1 (pair.car), List.Empty);
	if (last == null)
	  result = new_pair;
	else
	  last.cdr = new_pair;
	last = new_pair;
	list = pair.cdr;
      }
    return result;
  }

  /** An optimized single-list version of for-each. */
  static public void forEach1 (Procedure proc, Object list)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    while (list != List.Empty)
      {
	Pair pair = (Pair) list;
	proc.apply1 (pair.car);
	list = pair.cdr;
      }
  }

  public Object apply2 (Object arg1, Object arg2)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Procedure proc = (Procedure) arg1;
    if (collect)
      return map1 (proc, arg2);
    forEach1 (proc, arg2);
    return Interpreter.voidObject;
  }

  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Procedure proc = (Procedure) (args[0]);
    int arity = args.length - 1;
    if (arity == 1)
      return map1 (proc, args[1]);
    Object result;
    Pair last = null;
    if (collect)
      result = List.Empty;
    else
      result = Interpreter.voidObject;
    Object[] rest = new Object [arity];
    System.arraycopy (args, 1, rest, 0, arity);
    Object[] each_args = new Object [arity];
    for (;;)
      {
	for (int i = 0;  i < arity;  i++)
	  {
	    Object list = rest[i];
	    if (list == List.Empty)
	      return result;
	    Pair pair = (Pair) list;
	    each_args[i] = pair.car;
	    rest[i] = pair.cdr;
	  }
	Object value = proc.applyN (each_args);
	if (collect)
	  {
	    Pair new_pair = new Pair (value, List.Empty);
	    if (last == null)
	      result = new_pair;
	    else
	      last.cdr = new_pair;
	    last = new_pair;
	  }
      }
  }

}
