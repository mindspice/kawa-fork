package kawa.standard;
import gnu.mapping.*;
import gnu.lists.*;

/**
 * Implement the Scheme standard function "append".
 * @author Per Bothner
 */

public class append extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    return append$V(args);
  }

  public static Object append$V (Object[] args)
  {
    int count = args.length;
    if (count == 0)
      return LList.Empty;
    Object result = args[count - 1];
    for (int i = count - 1; --i >= 0; )
      {
	Object list = args[i];
	Object copy = null;
	Pair last = null;
	while (list instanceof Pair)
	  {
	    Pair list_pair = (Pair) list;
	    Pair new_pair = new Pair (list_pair.car, null);
	    if (last == null)
	      copy = new_pair;
	    else
	      last.cdr = new_pair;
	    last = new_pair;
	    list = list_pair.cdr;
	  }
	if (list != LList.Empty)
	  throw new WrongType("append", 2, "list");
	if (last != null)
	  {
	    last.cdr = result;
	    result = copy;
	  }
      }
    return result;
  }
}
