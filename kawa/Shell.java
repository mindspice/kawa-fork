package kawa;

import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;
import gnu.lists.*;
import gnu.bytecode.ArrayClassLoader;
import gnu.bytecode.ZipLoader;
import gnu.kawa.format.AbstractFormat;
import gnu.kawa.io.BinaryInPort;
import gnu.kawa.io.FilePath;
import gnu.kawa.io.InPort;
import gnu.kawa.io.NBufferedInputStream;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.kawa.io.TtyInPort;
import gnu.kawa.util.ExitCalled;
import gnu.kawa.util.Signals;
import gnu.text.Lexer;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import java.net.URL;

/** Utility functions (static methods) for kawa.repl.
 * It also contains methods for file loading (source and compiled).
 */

public class Shell
{
  /* #ifdef JAVA2 */
  public static ThreadLocal currentLoadPath = new ThreadLocal();
  /* #else */
  // public static Location currentLoadPath =
  //   Location.make(null, "load-path");
  /* #endif */

  private static Class[] noClasses = { };
  private static  Class[] boolClasses = { Boolean.TYPE };
  private static  Class[] lispPushClasses = { OutPort.class, Character.TYPE, Boolean.TYPE };
  private static  Class[] xmlPrinterClasses = {Consumer.class, Object.class };
  private static  Class[] httpPrinterClasses = {OutPort.class };
  private static Object consumerArg = "(consumer)";

  /** A table of names of known output formats.
   * For each entry, the first Object is the format name.
   * The next entries are a class name, the name of a static method in that
   * class, and the parameter types (as a Class[] suitable for getMethod).
   * The remain values are arguments (passed to invoke), except that if an
   * argument is the spacial value consumerArg, it is replaced by the
   * destination Consumer. */
   
    static Object[][] formats = {
      { "scheme", "gnu.kawa.functions.DisplayFormat",
	"getSchemeFormat", boolClasses,
	Boolean.FALSE },
      { "readable-scheme", "gnu.kawa.functions.DisplayFormat", 
	"getSchemeFormat", boolClasses,
	Boolean.TRUE },
      { "elisp", "gnu.kawa.functions.DisplayFormat",
	"getEmacsLispFormat", boolClasses,
	Boolean.FALSE },
      { "readable-elisp", "gnu.kawa.functions.DisplayFormat",
	"getEmacsLispFormat", boolClasses,
	Boolean.TRUE },
      { "clisp", "gnu.kawa.functions.DisplayFormat",
	"getCommonLispFormat", boolClasses,
	Boolean.FALSE },
      { "readable-clisp", "gnu.kawa.functions.DisplayFormat",
	"getCommonLispFormat", boolClasses,
	Boolean.TRUE },
      { "commonlisp", "gnu.kawa.functions.DisplayFormat",
	"getCommonLispFormat", boolClasses,
	Boolean.FALSE },
      { "readable-commonlisp", "gnu.kawa.functions.DisplayFormat",
	"getCommonLispFormat", boolClasses,
	Boolean.TRUE },
        { "xml", "gnu.xml.XMLPrinter",
          "make", xmlPrinterClasses,
          consumerArg, null },
        { "html", "gnu.xml.XMLPrinter",
          "make", xmlPrinterClasses,
          consumerArg, "html" },
        { "xhtml", "gnu.xml.XMLPrinter",
          "make", xmlPrinterClasses,
          consumerArg, "xhtml" },
        { "cgi", "gnu.kawa.xml.HttpPrinter",
          "make", httpPrinterClasses,
          consumerArg },
        { "ignore", "gnu.lists.VoidConsumer",
          "make", new Class[] { Consumer.class },
          consumerArg },
        { null }
    };

  public static String defaultFormatName;
  public static Object[] defaultFormatInfo;
  public static java.lang.reflect.Method defaultFormatMethod;

  /** Specify the default output format.
   * @param name The name of the format, as an entry in the formats table.
   */
  public static void setDefaultFormat(String name)
  {
    name = name.intern();
    defaultFormatName = name;
    for (int i = 0;  ;  i++)
      {
	Object[] info = formats[i];
	Object iname = info[0];
	if (iname == null)
	  {
	    System.err.println ("kawa: unknown output format '"+name+"'");
	    System.exit (-1);
	  }
	else if (iname == name)
	  {
	    defaultFormatInfo = info;
	    try
	      {
		Class formatClass = Class.forName((String) info[1]);
		defaultFormatMethod
		  = formatClass.getMethod((String) info[2], (Class[]) info[3]);
		
	      }
	    catch (Throwable ex)
	      {
		System.err.println("kawa:  caught "+ex+" while looking for format '"+name+"'");
		System.exit (-1);
	      }
	    break;
	  }
      }
    if (! defaultFormatInfo[1].equals("gnu.lists.VoidConsumer"))
      ModuleBody.setMainPrintValues(true);
  }

  /** Return a Consumer that formats using the appropriate format.
   * The format is chosen depending on specified defaults.
   * @param out The output where formatted output is sent to.
   */
  public static Consumer getOutputConsumer(OutPort out)
  {
    Object[] info = defaultFormatInfo;
    if (out == null)
      return VoidConsumer.getInstance();
    else if (info == null)
      return Language.getDefaultLanguage().getOutputConsumer(out);
    try
      {
	Object args[] = new Object[info.length - 4];
	System.arraycopy(info, 4, args, 0, args.length);
        boolean useConsumer = args[0] == consumerArg;
	for (int i = args.length;  --i >= 0; ) {
	  if (args[i] == consumerArg)
	    args[i] = out;
        }
	Object format = defaultFormatMethod.invoke(null, args);
	if (format instanceof AbstractFormat)
          return ((AbstractFormat) format).makeConsumer(out);
	else
	  return (Consumer) format;
      }
    catch (Exception ex)
      {
	throw new RuntimeException("cannot get output-format '"
				   + defaultFormatName + "' - caught " + ex);
      }
  }

  public static boolean run (Language language, Environment env)
  {
    InPort inp = InPort.inDefault ();
    SourceMessages messages = new SourceMessages();
    OutPort perr;
    if (inp instanceof TtyInPort) // Interactive?
      {
        ((TtyInPort)inp).setPrompter(defaultPrompter);
        perr = OutPort.errDefault();
      }
    else
      perr = null; // Non-interactive.

    Throwable ex = run(language, env, inp,
                       getOutputConsumer(OutPort.outDefault()),
                       perr, null, messages);
    if (ex == null)
      return true;
    printError(ex, messages, OutPort.errDefault());
    return false;
  }

  public static Throwable run (Language language,  Environment env,
                               InPort inp, OutPort pout, OutPort perr,
                               SourceMessages messages)
  {
    Consumer out = getOutputConsumer(pout);
    return run(language, env, inp, out, perr, null, messages);
  }

  public static boolean run (Language language,  Environment env,
                             InPort inp, Consumer out, OutPort perr,
                             URL url)
  {
    SourceMessages messages = new SourceMessages();
    Throwable ex = run(language, env, inp, out, perr, url, messages);
    if (ex != null)
      printError(ex, messages, perr);
    return ex == null;
  }

  public static Throwable run (Language language,  Environment env,
                               InPort inp, Consumer out, OutPort perr,
                               java.net.URL url, SourceMessages messages)
  {
    Language saveLanguage = Language.setSaveCurrent(language);
    Lexer lexer = language.getLexer(inp, messages);
    boolean interactive = inp instanceof TtyInPort;
    lexer.setInteractive(interactive);
    CallContext ctx = CallContext.getInstance();
    Consumer saveConsumer = null;
    if (out != null)
      {
	saveConsumer = ctx.consumer;
	ctx.consumer = out;
      }
    try
      {
        Thread thread = Thread.currentThread();
        ClassLoader parentLoader = thread.getContextClassLoader();
        // Create a "session" ClassLoader.  Use this for remembering classes
        // created in one command (Compilation) through further command,
        // while still allowing the classes to be replaced and collected.
        if (!(parentLoader instanceof ArrayClassLoader))
          thread.setContextClassLoader(new ArrayClassLoader(parentLoader));
      }
    catch (SecurityException ex)
      {
        // Nothing - we'll just lose some minor functionality.
      }
    java.lang.reflect.Method parserMethod = getJLineParserMethod(inp);
    Environment saveEnv = Environment.setSaveCurrent(env);
    try
      {
        SigIntHandler sigIntHandler = null;
        if (interactive) {
          sigIntHandler = new SigIntHandler();
          ((TtyInPort) inp).sigIntHandler = sigIntHandler;
        }
	for (;;)
	  {
            Object oldIntHandler = null;
	    int opts = Language.PARSE_FOR_EVAL|Language.PARSE_ONE_LINE|Language.PARSE_INTERACTIVE_MODULE;
	    try
	      {
                Compilation comp;
                if (interactive)
                    oldIntHandler =
                        Signals.register("INT",
                                         ((TtyInPort) inp).sigIntHandler);
                if (parserMethod != null) {
                    try {
                        comp = (Compilation) parserMethod.invoke(null, language, lexer);
                    } catch (java.lang.reflect.InvocationTargetException ex) {
                        throw ex.getTargetException();
                    }
                } else {
                    for (;;) {
                        try {
                            comp = language.parse(lexer, opts, null);
                            break;
                        } catch (TtyInPort.MoreInputNeeded ex) {
                        }
                    }
                }
		boolean sawError;
                if (interactive) {
                    sawError = messages.checkErrors(perr, Compilation.maxErrors());
                  perr.flush();
                }
                else if (messages.seenErrors())
                  throw new SyntaxException(messages);
                else
                  sawError = false;
		if (comp == null) // ??? end-of-file
		  break;
                ModuleExp mexp = comp.getModule();
		if (sawError) {
                    comp.lexical.pop(mexp);
                    continue;
                }

                // Inline ModuleExp.evalModule for shorter stack trace
                Object inst = ModuleExp.evalModule1(env, comp, url, perr);
                if (perr != null)
                    perr.flush();
                if (inst == null) {
                    comp.pop(mexp);
                    throw new SyntaxException(messages);
                }
                ModuleExp.evalModule2(env, ctx, language, mexp, inst);

                if (out instanceof Writer)
                  ((Writer) out).flush();
		if (inp.eofSeen())
		  break;
	      }
            catch (ThreadDeath e)
              {
                if (! interactive)
                    throw e;
                else if (sigIntHandler == null || sigIntHandler.trace == null)
                    e.printStackTrace(perr);
                else
                    sigIntHandler.trace.printStackTrace(perr);
                Thread.interrupted();
              }
            catch (Error e)
              {
                throw e;
              }
            catch (Throwable e)
	      {
		if (! interactive)
		  return e;
                printError(e, messages, perr);
	      }
            finally
              {
                if (oldIntHandler != null)
                  Signals.unregister("INT", oldIntHandler);
              }
	  }
      }
    finally
      {
        Environment.restoreCurrent(saveEnv);
	if (out != null)
	  ctx.consumer = saveConsumer;
        Language.restoreCurrent(saveLanguage);
      }
    return null;
  }

    static java.lang.reflect.Method getJLineParserMethod(InPort in) {
        Class cls = in.getClass();
        try {
            if (cls.getName().equals("gnu.kawa.io.JLineInPort")) {
                cls = Class.forName("gnu.kawa.io.JLineInPort$KawaParsedLine");
                return cls.getDeclaredMethod("parse",
                                             Language.class, Lexer.class);
            }
        } catch (Throwable ex) {
        }
        return null;
    }

  public static void printError (Throwable ex, SourceMessages messages,
                                 OutPort perr)
  {
    if (ex instanceof WrongArguments)
      {
        WrongArguments e = (WrongArguments) ex;
        messages.printAll(perr, Compilation.maxErrors());
        if (e.usage != null)
          perr.println("usage: "+e.usage);
        e.printStackTrace(perr);
      }
    /*
    else if (ex instanceof java.io.IOException)
      {
        messages.printAll(perr, Compilation.maxErrors());
        String msg = new SourceError(inp, 'e', "").toString();
        msg = msg.substring(0, msg.length() - 2);
        perr.println(msg + " (or later): caught IOException");
        ex.printStackTrace(perr);
      }
    */
    else
      {
        SyntaxException se;
        if (ex instanceof SyntaxException
            && (se = (SyntaxException) ex).getMessages() == messages)
          {
            se.printAll(perr, Compilation.maxErrors());
            se.clear();
          }
        else
          {
            messages.printAll(perr, Compilation.maxErrors());
            ex.printStackTrace(perr);
          }
      }
    perr.flush();
  }

  public final static CompiledModule checkCompiledZip (InputStream fs, Path path, Environment env, Language language)
    throws IOException
  {
    try
      {
        fs.mark(5);
        boolean isZip = (fs.read() == 'P' && fs.read() == 'K'
                         && fs.read() == '\003' && fs.read() == '\004');
        fs.reset();
        if (! isZip)
          return null;
      }
    catch (IOException ex)
      {
        return null;
      }
    fs.close ();
    Environment orig_env = Environment.getCurrent();
    String name = path.toString();
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);
        File zfile = path.toFile();
        if (zfile == null)
          throw new RuntimeException ("load: "+name+" - not a file path");
	if (!zfile.exists ())
	  throw new RuntimeException ("load: "+name+" - not found");
	if (!zfile.canRead ())
	  throw new RuntimeException ("load: "+name+" - not readable");
	ZipLoader loader = new ZipLoader (name);
        Class clas = loader.loadAllClasses();
        return CompiledModule.make(clas, language);
      }
    catch (java.io.IOException ex)
      {
	throw new WrappedException ("load: "+name+" - "+ex.toString (), ex);
      }
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
      }
  }

    static InPort openFile(InputStream fs, Path path) throws IOException {
        Object conv = OutPort.charEncoding.get(null);
        InPort src;
        if (conv == null || conv == Boolean.TRUE)
            return BinaryInPort.openHeuristicFile(fs, path);
        else
            return InPort.openFile(fs, path, conv);
    }


  /** Run a named source file, compiled .zip, or class.
   * We try in order if {@code fname} names a compiled zip file,
   * or names some other file (in which case it is assumed to be source),
   * or is the name of a class in the classpath.
   * @param lineByLine Should we read and evaluate a source file line-by-line
   *   (i.e. read and evaluate each line before reading the next one),
   *   or should be read and compile the whole file as a module before
   *   running it?  Only used when parsing a source file.
   * @param skipLines  If reading a source file, the number of initial
   *   lines to skip before beginning parsing.
   * @return True on success, false on failure.
   */

  public static boolean runFileOrClass (String fname,
                                        boolean lineByLine, int skipLines)
  {
    Language language = Language.getDefaultLanguage();
    try
      {
        Path path;
        InputStream fs;
        if (fname.equals("-"))
          {
            path = Path.valueOf("/dev/stdin");
            fs = System.in;
          }
        else
          {
            path = Path.valueOf(fname);
            fs = path.openInputStream();
          }
        try
          {
            Environment env = Environment.getCurrent();
            return runFile(fs, path, env, lineByLine, skipLines);
          }
        catch (Error e)
          {
            throw e;
          }
        catch (Throwable e)
          {
            e.printStackTrace(System.err);
            return false;
          }
      }
    catch (Error e)
      {
        throw e;
      }
    catch (Throwable e)
      {
        Class clas;
        try
          {
            clas = Class.forName(fname);
          }
        catch (Exception ex)
          {
            System.err.println("Cannot read file "+e.getMessage());
            return false;
          }
        try
          {
            runClass(clas, Environment.getCurrent());
            return true;
          }
        catch (Error ex)
          {
            throw ex;
          }
        catch (Throwable ex)
          {
              //ExitCalled.check(e);
            ex.printStackTrace();
            return false;
          }
      }
  }

    public static void runClass(Class clas, Environment env) throws Throwable {
        CompiledModule cmodule = CompiledModule.make(clas, Language.getDefaultLanguage());
        cmodule.evalModule(env, OutPort.outDefault());
    }

    public static final boolean runFile(InputStream fs, Path path,
                                        Environment env,
                                        boolean lineByLine, int skipLines)
            throws Throwable {
        if (! (fs instanceof BufferedInputStream)
            && ! (fs instanceof NBufferedInputStream))
            fs = new NBufferedInputStream(fs);
        Language language = Language.getDefaultLanguage();
        Path savePath = (Path) currentLoadPath.get();
        try {
            currentLoadPath.set(path);
            CompiledModule cmodule = checkCompiledZip(fs, path, env, language);
            if (cmodule == null) {
                InPort src = openFile(fs, path);
                while (--skipLines >= 0)
                    src.skipRestOfLine();
                try {
                    SourceMessages messages = new SourceMessages();
                    URL url = path.toURL();
                    OutPort perr = OutPort.errDefault();
                    if (lineByLine) {
                        boolean print = ModuleBody.getMainPrintValues();
                        Consumer out
                            = (print ? getOutputConsumer(OutPort.outDefault())
                               : new VoidConsumer());
                        Throwable ex
                            = run(language, env, src, out, perr, url, messages);
                        if (ex instanceof SyntaxException
                            && ((SyntaxException) ex).getMessages() == messages) {
                            messages.printAll(perr, Compilation.maxErrors());
                            perr.flush();
                            return false;
                        }
                        if (ex != null)
                            throw ex;
                    } else {
                        cmodule = compileSource(src, env, url, language,
                                                messages, perr);
                        if (cmodule == null)
                            return false;
                    }
                } finally {
                    src.close();
                }
            }
            if (cmodule != null)
                cmodule.evalModule(env, OutPort.outDefault());
        } finally {
            currentLoadPath.set(savePath);
        }
        return true;
    }

    /** Parse and compile a module from the given port.
     * Return null on error, which gets reported to {@code messages}.
     */
    static CompiledModule compileSource(InPort port, Environment env, URL url,
                                        Language language,
                                        SourceMessages messages, OutPort perr)
            throws SyntaxException, IOException {
        ModuleManager manager = ModuleManager.getInstance();
        ModuleInfo minfo = manager.findWithSourcePath(port.getName());
        Lexer lexer = language.getLexer(port, messages);
        try {
            Compilation comp
                = language.parse(lexer, Language.PARSE_IMMEDIATE, minfo);
            CallContext ctx = CallContext.getInstance();
            //ctx.values = Values.noArgs;
            Object inst = ModuleExp.evalModule1(env, comp, url, null);
            messages.printAll(perr, Compilation.maxErrors());
            perr.flush();
            if (inst == null || messages.seenErrors())
                return null;
            return new CompiledModule(comp.getModule(), inst, language);
        }
        catch (Error ex) {
            throw ex;
        }
        catch (Throwable ex) {
            if (! (ex instanceof SyntaxException)
                || ((SyntaxException) ex).getMessages() != messages) {
                lexer.error('e', "unexpected exception while compiling: "+ex);
                messages.printAll(perr, Compilation.maxErrors());
                ex.printStackTrace(perr);
            }
            else
                messages.printAll(perr, Compilation.maxErrors());
            return null;
        }
    }

    public static final Procedure1 defaultPrompter = new Prompter();
    static class Prompter extends Procedure1 {
        public Object apply1 (Object arg) {
            return ((TtyInPort) arg).defaultPrompt();
        }
    }

    static class SigIntHandler implements Runnable {
        public Thread thread;
        public Error trace;
        public SigIntHandler(Thread thread) { this.thread = thread; }
        public SigIntHandler() { this.thread = Thread.currentThread(); }

        public void run() {
            Error ex = new Error("user interrupt of "+thread);
            ex.setStackTrace(thread.getStackTrace());
            this.trace = ex;
            thread.stop();
        }
    };
}
