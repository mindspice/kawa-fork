package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "expt". */

public class expt extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    if (arg2 instanceof IntNum)
      return ((Numeric) arg1).power((IntNum) arg2);
    return new DFloNum (Math.pow (((RealNum)arg1).doubleValue (),
				  ((RealNum)arg2).doubleValue ()));
  }
}
