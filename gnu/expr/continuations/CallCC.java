package gnu.expr.continuations;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.expr.continuations.Helpers.*;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure1;

/**
 * Implementation of the call-with-current-continuation function.
 *
 * @author Andrea Bernardini <andrebask@gmail.com>
 */
public class CallCC extends Procedure1 {

    public static final CallCC callcc = new CallCC();

    public Object apply1(Object arg1) throws Throwable {
        return call_cc((Procedure) arg1);
    }

    public void compile(ApplyExp exp, Compilation comp, Target target) {
        CodeAttr code = comp.getCode();
        Method callccMethod = ClassType.make("gnu.expr.continuations.CallCC")
            .getDeclaredStaticMethod("call_cc", 1);

        exp.getArg(1).compile(comp, ClassType.make("gnu.mapping.Procedure"));
        code.emitInvokeStatic(callccMethod);
    }

    /**
     * Throws a ContinuationException starting to unwind the stack.
     * A call to the first computation step of the lambda function passed
     * to the call/cc is enclosed in a ContinuationFrame, that is stored into
     * the frames list of the ContinuationException.
     */
    public static Object call_cc(final Procedure receiver) throws ContinuationException {
        try {
            // begin unwind the stack
            throw new ContinuationException();
        } catch (ContinuationException sce) {
            sce.extend(new ContinuationFrame(receiver));
            throw sce;
        }
    }
}
