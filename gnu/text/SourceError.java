package gnu.text;
import gnu.kawa.io.InPort;
import java.io.File;

/** Represents an error message from processing a "source" file.
 */

public class SourceError extends SourceLocator.Simple
// FIXME: If JAVA6, should implement: javax.tools.Diagnostic<Path>
{
    /** Used to chain to the "next" message. */
    public SourceError next;

    /** The seriousness of the error - one of 'i' (for informational),
     * 'w' (for warning), 'e' (for error), or 'f' (for fatal error). */
    public char severity;

    /** If non-null, an error code, as might be specified by a standard. */
    public String code;

    /** The actual error message.
     * This is post-localization and -formatting.
     * It can contain multiple lines, separated by '\n'.*/
    public String message;

    /** Provides optional stack trace.
     * Filled when --debug-error-prints-stack-trace or
     * --debug-warning-prints-stack-trace option is used.*/
    public Throwable fakeException;

    public SourceError(char severity, String filename, int line, int column, 
                       String message) {
        this.severity = severity;
        this.filename = filename;
        this.position = SourceMapper.simpleEncode(line, column);
        this.message = message;
    }

    public SourceError(char severity, SourceLocator location, String message) {
        this.severity = severity;
        this.filename = location.getFileName();
        this.position = SourceMapper.simpleEncode(location);
        this.message = message;
    }

    /** Create a new SourceError using the current line/column from
     * a <code>InPort</code>. */
    public SourceError(InPort port, char severity, String message) {
        this(severity, port.getName(),
             adjustFromPort(port.getLineNumber()),
             adjustFromPort(port.getColumnNumber()),
             message);
    }
    private static int adjustFromPort(int portPosition) {
        return portPosition >= 0 ? portPosition + 1 : portPosition;
    }

    /** Convert the error to a String.
     * The String starts with filename, line and option column,
     * followed by the message.  Warning messages are indicated as such. */
    public String toString() {
        return toString(false);
    }

    /** Convert the error to a String.
     * The String starts with filename, line and option column,
     * followed by the message.  Warning messages are indicated as such. */
    public String toString(boolean stripDirectories) {
        StringBuilder buffer = new StringBuilder();
        appendTo(buffer, stripDirectories, null);
        return buffer.toString ();
    }

    public static void appendEscaped(Appendable out, CharSequence str)
        throws java.io.IOException {
        int len = str.length();
        for (int i = 0; i < len; i++) {
            char ch = str.charAt(i);
            if (ch == '<' || ch == '>' || ch == '&') {
                out.append(ch == '<' ? "&lt;"
                           : ch == '>' ? "&gt;"
                           : "&amp;");
            } else
                out.append(ch);
        }
    }
   
    public void appendTo(Appendable out, boolean stripDirectories,
                         String newLine) {
        boolean isDomTerm = gnu.kawa.io.CheckConsole.forDomTerm(out);
        try {
            if (isDomTerm)
                out.append("\033]44;"+(isRepl() ? "repl: true" : "")+"\007");
            if (isDomTerm)
                out.append("\033[44;0u");
            String fname;
            if (filename == null)
                fname = "<unknown>";
            else {
                fname = filename;
                if (stripDirectories)
                    fname = new File(fname).getName();
            }
            StringBuilder position = new StringBuilder();
            String endpos = "";
            int line = getStartLine();
            int column = getStartColumn();
            if (line > 0 || column > 0) {
                position.append(Integer.toString(line));
                if (column > 0) {
                    position.append(':');
                    position.append(Integer.toString(column));
                }
                int eline = getEndLine();
                int ecolumn = getEndColumn();
                if (eline > 0 && ecolumn > 0 && column > 0) {
                    if (line == eline && ecolumn > column) {
                        endpos = "-" + ecolumn;
                    } else if (eline > line) {
                        endpos = "-" + eline + ":" + ecolumn;
                    }
                }
            }
            if (isDomTerm) {
                out.append("\033]72;");
                out.append("<a class='subtle' href='");
                out.append(new File(fname).toURI().toString());
                if (position.length() > 0) {
                    out.append("#position=");
                    out.append(position);
                    out.append(endpos);
                }
                out.append("'>");
                appendEscaped(out, fname);
            }
            else
                out.append(fname);
            if (position.length() > 0) {
                out.append(':');
                out.append(position);
            }
            if (isDomTerm) {
                out.append("</a>\007");
            }
            out.append(": ");
            if (severity == 'w')
                out.append("warning - ");
            else if (severity == 'i')
                out.append("note - ");
            out.append(message);
            if (code != null) {
                out.append(" [");
                out.append(code);
                out.append("]");
            }

            if (fakeException != null) {
                StackTraceElement[] stackTrace = fakeException.getStackTrace();
                for (int i = 0; i < stackTrace.length; i++) {
                    out.append(newLine != null ? newLine : "\n");
                    out.append("    ");
                    out.append(stackTrace[i].toString());
                }
            }
            if (isDomTerm)
                out.append("\033[44;0u");
            if (newLine != null)
                out.append(newLine);
        } catch (java.io.IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public void print(Appendable out) {
        appendTo(out, false, null);
    }

    public void println(Appendable out, boolean stripDirectories) {
        appendTo(out, stripDirectories,
                 System.getProperty("line.separator", "\n"));
    }
}
