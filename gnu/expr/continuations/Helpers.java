package gnu.expr.continuations;

import gnu.mapping.Procedure;
import java.util.ArrayList;

/**
 * Support code for the call/cc implementation.
 *
 * @author Andrea Bernardini <andrebask@gmail.com>
 */
public class Helpers {

    /**
     * A computation step with its continuation
     * (as a list of ContinuationFrames).
     */
    public static class ContinuationFrame {

        Procedure computation;
        ArrayList<ContinuationFrame> continuation;

        public ContinuationFrame(Procedure frame) {
            computation = frame;
        }
    }

    static class FastException extends Exception {
        @Override
        public Throwable fillInStackTrace() {
            return this;
        }
    }

    /**
     * An Exception used to exit the current execution context
     * and containing a thunk that reloads a continuation.
     */
    public static class ExitException extends FastException {

        Procedure thunk;

        public ExitException(Procedure thunk) {
            this.thunk = thunk;
        }
    }

    /**
     * An Exception used to store in order the computation steps, to be
     * later stored in a Continuation Object.
     */
    public static class ContinuationException extends FastException {

        // A list of frames that have not yet been assembled into the
        // continuation.
        ArrayList<ContinuationFrame> newCapturedFrames = new ArrayList<ContinuationFrame>();

        // A list of frames that already have been assembled into the continuation.
        // When unloading the stack, we don't need to unload these frames.
        ArrayList<ContinuationFrame> reloadedFrames;

        /**
         * Push a newly created Frame onto the list of frames that need
         * to be assembled into the continuation.  This will be done in the
         * top level exception handler.
         */
        public void extend(ContinuationFrame extension) {
            newCapturedFrames.add(extension);
        }

        /**
         * Append the tail of the current continuation to the exception
         * Object so that the handler can assemble the new frames onto it.
         */
        public void append(ArrayList<ContinuationFrame> oldFrames) {
            reloadedFrames = oldFrames;
        }

        /**
         * Assemble and return the continuation.
         */
        public Continuation toContinuation() throws Exception {
            return new Continuation(newCapturedFrames, reloadedFrames);
        }
    }

    public static void extend(ContinuationException c, ContinuationFrame extension){
        c.extend(extension);
    }

}
