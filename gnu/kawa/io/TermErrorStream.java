package gnu.kawa.io;
// Copy of org.domterm.util.DomTermErrorStream. FIXME

import java.io.*;

/** The standard error stream, when running under DomTerm or an ANSI terminal.
 * This forwards to another stream (normally System.out),
 * but surrounds each write with special escape sequences.
 * These cause DomTerm to place the error output inside
 * a {@code <span std="err">} element, which by default
 * is colored red.
 */

public class TermErrorStream extends PrintStream {
    public static final byte[] DOMTERM_START_ERR_MARKER = {
        19, // urgent-begin
        21, // urgent-counted
        27, // escape
        (byte) '[',
        (byte) '1',
        (byte) '2',
        (byte) 'u',
        20 // urgent-end
    };
    public static final byte[] DOMTERM_END_ERR_MARKER = {
        19, // urgent-begin
        21, // urgent-counted
        27, // escape
        (byte) '[',
        (byte) '1',
        (byte) '1',
        (byte) 'u',
        20 // urgent-end
    };
    public static final byte[] ANSI_START_ERR_MARKER = {
        27 /* escape */,
        (byte) '[',
        (byte) '3',
        (byte) '1',
        (byte) 'm'
    };
    public static final byte[] ANSI_END_ERR_MARKER = {
        27 /* escape */,
        (byte) '[',
        (byte) '3',
        (byte) '9',
        (byte) 'm'
    };
    byte[] startErrMarker;
    byte[] endErrMarker;
    int mextra; // max(startErrMarker.length,endErrMarker.length)+1
    byte[] buffer;
    int bsize = 0;
    int blimit = 0;
    boolean inError;
    private static int INIT_BUFSIZE = 1024;
    private static int SHRINK_BUFSIZE = 5000;

    private PrintStream out;

    public TermErrorStream(PrintStream out, boolean ansi) {
        super(out, true);
        this.out = out;
        if (ansi) {
            startErrMarker = ANSI_START_ERR_MARKER;
            endErrMarker = ANSI_END_ERR_MARKER;
        } else {
            startErrMarker = DOMTERM_START_ERR_MARKER;
            endErrMarker = DOMTERM_END_ERR_MARKER;
        }
        int slen = startErrMarker.length;
        int elen = endErrMarker.length;
        mextra = (slen > elen ? slen : elen) + 1;
    }
    public boolean isDomTerm() {
        return startErrMarker == DOMTERM_START_ERR_MARKER;
    }

    public static void setSystemErr(boolean ansi) {
        // KLUDGE because our DomTermErrorStream is a copy of the
        // one in domterm.jar (and currently in a different package),
        // so this handles either version.  FIXME.
        if (System.err.getClass().getName().indexOf("DomTermErrorStream") < 0)
        //if (! (System.err instanceof DomTermErrorStream))
            System.setErr(new TermErrorStream(System.out, ansi));
    }

    private void writeRaw(byte b) {
        if (bsize >= blimit) {
            byte[] nbuffer = new byte[buffer == null ? INIT_BUFSIZE
                                      : (3 * buffer.length) >> 1];
            if (bsize > 0)
                System.arraycopy(buffer, 0, nbuffer, 0, bsize);
            blimit = nbuffer.length - mextra;
            buffer = nbuffer;
        }
        if (b == '\r' || b == '\n') {
            if (inError) {
                int elen = endErrMarker.length;
                System.arraycopy(endErrMarker, 0, buffer, bsize, elen);
                bsize += elen;
                inError = false;
            }
            buffer[bsize++] = b;
            if (b == '\n') {
                out.write(buffer, 0, bsize);
                bsize = 0;
                if (buffer.length > SHRINK_BUFSIZE) {
                    buffer = new byte[INIT_BUFSIZE];
                    blimit = INIT_BUFSIZE - mextra;
                }
            }
        } else {
            if (! inError) {
                int slen = startErrMarker.length;
                System.arraycopy(startErrMarker, 0, buffer, bsize, slen);
                bsize += slen;
                inError = true;
            }
            buffer[bsize++] = b;
        }
    }

    @Override
    public void write(int b) {
        synchronized (out) {
            writeRaw((byte) b);
        }
    }

    @Override
    public void write(byte buf[], int off, int len) {
        synchronized (out) {
            while (--len >= 0) {
                writeRaw(buf[off++]);
            }
        }
    }

    public void flush() {
        if (inError) {
            int elen = endErrMarker.length;
            System.arraycopy(endErrMarker, 0, buffer, bsize, elen);
            bsize += elen;
        }
        if (bsize > 0)
            out.write(buffer, 0, bsize);
        out.flush();
        bsize = 0;
        if (blimit > SHRINK_BUFSIZE) {
            buffer = null;
            blimit = 0;
        }
    }

    public void close() {
        super.close();
        buffer = null;
    }
}
