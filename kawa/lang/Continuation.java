package kawa.lang;
import gnu.mapping.*;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

/**
 * A Continuation "represents an entire (default) future for the computation.
 * This implemementation is based on Java exceptions, and is restricted
 * to "upward" (?) continuation (i.e. catch/throw-type uses).
 * @author	Per Bothner
 */

public class Continuation extends MethodProc
{
  public boolean invoked;
  static int counter;
  int id;

  public Continuation (CallContext ctx)
  {
      super(true, applyMethodCont);
  }
    public static final MethodHandle applyMethodCont =
        lookupApplyHandle(Continuation.class, "applyMethodCont");

    public static Object applyMethodCont(Procedure proc, CallContext ctx) throws Throwable {
        Continuation cont = (Continuation) proc;
        if (cont.invoked)
            throw new GenericError
                ("implementation restriction: continuation can only be used once");
        throw new CalledContinuation (ctx.getRestArgsArray(), cont, ctx);
    }

  public static void handleException$X (Throwable ex, Continuation cont,
                                        CallContext ctx)
    throws Throwable
  {
    CalledContinuation cex;
    if (! (ex instanceof CalledContinuation)
        || (cex = (CalledContinuation) ex).continuation != cont)
      throw ex;
    cont.invoked = true;
    Object[] values = cex.values;
    int nvalues = values.length;
    for (int i = 0;  i < nvalues;  i++)
      ctx.consumer.writeObject(values[i]);
  }

  public static Object handleException (Throwable ex, Continuation cont)
    throws Throwable
  {
    CalledContinuation cex;
    if (! (ex instanceof CalledContinuation)
        || (cex = (CalledContinuation) ex).continuation != cont)
      throw ex;
    cont.invoked = true;
    return Values.make(cex.values);
  }

  public final String toString()
  {
    return "#<continuation " + id + (invoked ? " (invoked)>" : ">");
  }
}

