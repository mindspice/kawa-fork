package gnu.expr.continuations;

import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.mapping.Procedure0or1;
import gnu.mapping.Values;
import gnu.expr.continuations.Helpers.ContinuationFrame;
import gnu.expr.continuations.Helpers.ContinuationException;
import gnu.expr.continuations.Helpers.ExitException;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure1;
import java.util.ArrayList;

/**
 * A Procedure that implements an invokable reified Continuation.
 *
 * @author Andrea Bernardini <andrebask@gmail.com>
 */
public class Continuation extends Procedure0or1 {

    // The type of a continuation
    public static Type typeContinuation = ClassType.make(Continuation.class);

    // Holds the list of frames that form the continuation
    ArrayList<ContinuationFrame> frames;

    /**
     * Assembles the new frames and the already assembled frames
     * into a Continuation Object.
     */
    public Continuation(ArrayList<ContinuationFrame> newFrames,
                        ArrayList<ContinuationFrame> oldFrames) {

        frames = (oldFrames != null) ? new ArrayList<ContinuationFrame>(oldFrames)
                                     : new ArrayList<ContinuationFrame>();

        // The new frames are appended one by one to the old_frames
        // while setting their continuation to the list of frames below.
        for(int i = newFrames.size()-1; i >= 0; i--) {
            ContinuationFrame newFrame = newFrames.get(i);
            if (newFrame.continuation != null) {
                throw new Error("Continuation should be empty here");
            }
            newFrame.continuation = new ArrayList<ContinuationFrame>(frames);
            frames.add(newFrame);
        }
    }

    public Object apply0() throws Throwable {
        return apply1(Values.empty);
    }

    public Object apply1(final Object val) throws Throwable {

        // When invoked, a continuation does not returns
        // (Actually it is not a function). The call has
        // the effect of exiting from the current execution
        // context and re-establishing the continuation stored
        // in the Continuation Object (It replaces the stack).

        Procedure t = new Procedure1() {

            public Object apply1(Object ignored) throws Throwable {
                // reload the continuation.
                return reloadFrames(frames.size()-2, val);
            }
        };

        // We use an Exception to exit.
        throw new ExitException(t);
    }

    /**
     * Resumes the current continuation.
     * When the call/cc is called, the stack is unwinded and saved
     * on the heap. Immediately after the call/cc call, the current
     * continuation is resumed using this function. see also {@see
     * gnu.expr.continuations.TopLevelHandler}.
     */
    Object resume(final Object restartValue) throws Throwable {
        return reloadFrames(frames.size()-1, restartValue);
    }


    /**
     * Performs the actual reloading.
     * Iterates over the list of frames in reverse order to re-establish
     * the saved continuation with the passed value.
     */
    Object reloadFrames(int endIndex, Object restartValue) throws Throwable {
        Object continueValue = restartValue;
        for (int i = endIndex; i >= 0; i -= 1) {
            ContinuationFrame frame = frames.get(i);
            try {
                continueValue = frame.computation.apply1(continueValue);
            } catch (ContinuationException sce) {
                sce.append(frame.continuation);
                throw sce;
            }
        }
        return continueValue;
    }

    public String toString () {
        return "#<continuation" + /*this.hashCode() +*/ ">";
    }
}
