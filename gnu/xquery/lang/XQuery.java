// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.Char;
import kawa.standard.Scheme;
import gnu.bytecode.Type;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.xquery.util.*;
import gnu.xml.*;
import gnu.text.Lexer;
import java.util.Vector;
import gnu.kawa.functions.ConstantFunction0;

public class XQuery extends Interpreter
{
  public static final String XQUERY_FUNCTION_NAMESPACE
    = "http://www.w3.org/2002/08/xquery-functions";
  static boolean charIsInt = false;

  public static gnu.math.Numeric asNumber(Object arg)
  {
    if (arg instanceof Char)
      return gnu.math.IntNum.make(((Char) arg).intValue());
    return (gnu.math.Numeric) arg;
  }

  public static char asChar(Object x)
  {
    if (x instanceof Char)
      return ((Char) x).charValue();
    int i;
    if (x instanceof gnu.math.Numeric)
      i = ((gnu.math.Numeric) x).intValue();
    else
      i = -1;
    if (i < 0 || i > 0xffff)
      throw new ClassCastException("not a character value");
    return (char) i;
  }

  public boolean isTrue(Object value)
  {
    return gnu.xquery.util.BooleanValue.booleanValue(value);
  }

  public gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    return new XQParser(inp, messages);
  }

  public Compilation parse(Environment env, Lexer lexer)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    Compilation.usingTailCalls = true;
    gnu.text.SourceMessages messages = lexer.getMessages();
    Compilation tr = new Compilation(messages);
    ModuleExp mexp = new ModuleExp();
    tr.push(mexp);
    tr.mustCompileHere();
    tr.immediate = true;
    lexer.clearErrors();
    Expression sexp = ((XQParser) lexer).parse(tr);
    if (sexp == null)
      return null;
    mexp.body = sexp;
    tr.pop(mexp);
    return tr;
  }

  public Compilation parseFile (InPort port, boolean immediate,
				gnu.text.SourceMessages messages)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    Compilation.usingTailCalls = true;
    Compilation tr = new Compilation(messages);
    tr.immediate = immediate;
    ModuleExp mexp = new ModuleExp();
    mexp.setFile(port.getName());
    tr.push(mexp);
    XQParser lexer = (XQParser) getLexer(port, messages);
    lexer.nesting = 1;
    Vector exps = new Vector(10);
    for (;;)
      {
	Expression sexp = lexer.parse(tr);
	if (sexp == null)
	  break;
	exps.addElement(sexp);
      }
    int nexps = exps.size();
    if (nexps == 0)
      mexp.body = QuoteExp.voidExp;
    else if (nexps == 1)
      mexp.body = (Expression) exps.elementAt(0);
    else
      {
	Expression[] arr = new Expression[nexps];
	exps.copyInto(arr);
	mexp.body = new BeginExp(arr);
      }
    tr.pop(mexp);
    return tr;
  }

  public int getNamespaceOf(Declaration decl)
  {
    return decl.isProcedureDecl() ? FUNCTION_NAMESPACE : VALUE_NAMESPACE;
  }

  public String getName()
  {
    return "XQuery";
  }

  static XQuery instance;

  static int envCounter = 0;

  public XQuery()
  {
    Environment scmEnv = Scheme.builtin();
    environ = new Environment(scmEnv);
    environ.setName ("interaction-environment."+(++envCounter));
    ModuleBody.setMainPrintValues(true);

    /*
    BindingEnumeration e = scmEnv.enumerateAllBindings();
    while (e.hasMoreElements())
      {
	Binding b = e.nextBinding();
	if (b.isBound())
	  {
	    String name = b.getName();
	    Object val = b.get();
	    define(name, val);
	  }
      }
    */

    if (instance == null)
      instance = this;

    Environment saveEnv = Environment.getCurrent();
    Environment.setCurrent(environ);
    try
      {
	// Force it to be loaded now, so we can over-ride let* length etc.
	loadClass("kawa.lib.std_syntax");
	loadClass("kawa.lib.lists");
	loadClass("kawa.lib.strings");
	loadClass("gnu.commonlisp.lisp.PrimOps");
	loadClass("gnu.kawa.slib.HTTP");
	loadClass("gnu.kawa.slib.XStrings");
      }
    catch (Throwable ex)
      {
	// Ignore.  We get a ClassNotFoundException if gnu.kawa.slib.HTTP
	// was not built.  We get a NoClassDefFoundError if gnu.kawa.slib.HTTP
	// can't find servlets in the classpath.
      }
    finally
      {
	Environment.setCurrent(saveEnv);
      }

    define("define", new kawa.standard.set_b());
    define("document", gnu.kawa.xml.Document.document);
    define("unescaped-data", gnu.kawa.xml.MakeUnescapedData.unescapedData);
    define("item-at", gnu.xquery.util.ItemAt.itemAt);
    define("count", gnu.kawa.functions.CountValues.countValues);
    define("min", gnu.xquery.util.MinMax.min);
    define("max", gnu.xquery.util.MinMax.max);
    define("sum", gnu.xquery.util.Reduce.sum);
    define("avg", gnu.xquery.util.Average.avg);
    define("index-of", gnu.xquery.util.IndexOf.indexOf);
    define("last-index-of", gnu.xquery.util.LastIndexOf.lastIndexOf);
    define("position", gnu.xquery.util.Position.position);
    define("sublist", gnu.xquery.util.SubList.subList);
    define("empty", gnu.xquery.util.IsEmptySequence.isEmptySequence);
    define("false", new ConstantFunction0("false", Boolean.FALSE));
    define("true", new ConstantFunction0("true", Boolean.TRUE));
    define("number", gnu.xquery.util.NumberValue.numberValue);
    define("string-value", gnu.xquery.util.StringValue.stringValue);
    define("string", gnu.xquery.util.StringValue.string);
    define("concat", new kawa.standard.string_append());

    define("write-to", gnu.kawa.xml.WriteTo.writeTo);
    define_field("iterator-items", "gnu.kawa.xml.IteratorItems");
    define_field("list-items", "gnu.kawa.xml.ListItems");
    define_field("base-uri", "gnu.xquery.util.BaseUri", "baseUri");
  }

  public static XQuery getInstance()
  {
    if (instance == null)
      instance = new XQuery();
    return instance;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    XQuery interp = new XQuery();
    Interpreter.defaultInterpreter = interp;
    Environment.setCurrent(interp.getEnvironment());
  }

  public Object read (InPort in)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return XQParser.readObject(in);
  }

  public static final XMLFormat writeFormat = new XMLFormat();

  public FormatToConsumer getFormat(boolean readable)
  {
    return writeFormat;
  }

  public Consumer getOutputConsumer(OutPort out)
  {
    return new XMLPrinter(out, false);
  }

  LangPrimType booleanType;

  public Type getTypeFor(String name)
  {
    if (name == "t")
      name = "java.lang.Object";
    return Scheme.string2Type(name);
  }

  public Type getTypeFor (Class clas)
  {
    if (clas.isPrimitive())
      {
	String name = clas.getName();
	if (name.equals("boolean"))
	  {
	    if (booleanType == null)
	      booleanType = new LangPrimType(Type.boolean_type, this);
	    return booleanType;
	  }
	return Scheme.getNamedType(name);
      }
    return Type.make(clas);
  }

  public Procedure getPrompter()
  {
    return new Prompter();
  }
}

class Prompter extends Procedure1
{
  public Object apply1 (Object arg)
  {
    InPort port = (InPort) arg;
    int line = port.getLineNumber() + 1;
    char state = port.readState;
    if (state == '\n')
      state = ' ';
    if (state == '<')
      return "<!--" + line + "-->";
    else
      return "{--" + state + line + "--} ";
  }
}
