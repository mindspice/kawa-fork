package gnu.expr.continuations;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.expr.continuations.Helpers.ExitException;
import gnu.expr.continuations.Helpers.ContinuationException;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure1;

/**
 * A Procedure to run top level expressions in an Exception handler,
 * that resumes captured contiunations or re-establish previously
 * saved continuations.
 *
 * @author Andrea Bernardini <andrebask@gmail.com>
 */
public class TopLevelHandler extends Procedure1 {

    public static final TopLevelHandler topLevelHandler = new TopLevelHandler();

    public Object apply1(Object arg1) throws Throwable {
        return runInTopLevelHandler((Procedure) arg1);
    }

    public void compile(ApplyExp exp, Compilation comp, Target target) {
        CodeAttr code = comp.getCode();
        Method initMethod = ClassType.make("gnu.expr.continuations.TopLevelHandler")
            .getDeclaredStaticMethod("runInTopLevelHandler", 1);

        exp.getArg(1).compile(comp, ClassType.make("gnu.mapping.Procedure"));
        code.emitInvokeStatic(initMethod);
    }

    /**
     * Runs inside an exception handler the first computation of a top
     * level expression, managing the capture of continuations and the
     * invocation of saved continuation.
     */
    public static Object runInTopLevelHandler(Procedure initialFrame) throws Throwable {
        // runs the received frames until one returns.
        while (true) {
            try {
                return invokeFrame(initialFrame);
            } catch (ExitException rce) {
                // a capture or a continaution invocation
                // exited the previous execution context
                // and requires the top level handler to
                // run a thunk that re-establish an other
                // continuation. At the next cycle of the
                // loop we will run the received thunk.

                initialFrame = rce.thunk;
            }
        }
    }

    private static Object invokeFrame(final Procedure initialFrame) throws Throwable {
        try {
            // invoke the first computation of a top level expression.
            return initialFrame.apply1(null);
        } catch (ContinuationException sce) {
            // assemble the list of frames in a Continution Object.
            final Continuation k = sce.toContinuation();

            // send to the top level handler a computation that will
            // resume the captured continuation.
            Procedure f = new Procedure1() {

                public Object apply1(Object arg) throws Throwable {
                    return k.resume(k);
                }
            };

            throw new ExitException(f);
        }
    }
}

