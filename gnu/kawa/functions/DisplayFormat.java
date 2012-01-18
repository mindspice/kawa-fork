// Copyright (c) 2001, 2002, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.math.RatNum;
import gnu.math.IntNum;
import java.io.*;
import gnu.text.Char;
import gnu.expr.Keyword;
/* #ifdef enable:XML */
import gnu.kawa.xml.KNode;
import gnu.xml.XMLPrinter;
/* #endif */
import gnu.kawa.xml.XmlNamespace;
import gnu.text.PrettyWriter;
import gnu.text.Printable;
/* #ifdef use:java.util.regex */
import java.util.regex.*;
/* #endif */

/** Handle formatted output for Lisp-like languages. */

public class DisplayFormat extends AbstractFormat
{
  /** Fluid parameter to specify default output base for printing rationals. */
  public static final ThreadLocation outBase
    = new ThreadLocation("out-base");
  static { outBase.setGlobal(IntNum.ten()); }
  /** True if we should print a radix indicator when printing rationals.
   * The default is no; otherwise we follow Common Lisp conventions. */
  public static final ThreadLocation outRadix
    = new ThreadLocation("out-radix");

  /** This field is set true by Scheme when it creates a shared write format */
  public boolean checkSharing = false;

  /** Create a new instance.
   * @param readable if output should be formatted so it could be read
   *   back in again, for example strings shoudl be quoted.
   * @param language the programming language style to use, where
   *   'S' is Scheme, 'C' is Common Lisp, and 'E' is Emacs Lisp.
   */
  public DisplayFormat(boolean readable, char language)
  {
    this.readable = readable;
    this.language = language;
  }

  public static DisplayFormat getEmacsLispFormat(boolean readable)
  {
    return new DisplayFormat(readable, 'E');
  }

  public static DisplayFormat getCommonLispFormat(boolean readable)
  {
    return new DisplayFormat(readable, 'C');
  }

  public static DisplayFormat getSchemeFormat(boolean readable)
  {
    return new DisplayFormat(readable, 'S');
  }

  boolean readable;

  /** 'S' is Scheme-style; 'C' is CommonLisp-style;  'E' is Emacs-style.
   * Note Emacs has its own sub-class gnu.jemacs.lang.Print. */
  char language;

  public boolean getReadableOutput () { return readable; }
  
  @Override
  public void writeBoolean(boolean v, Consumer out)
  {
    write (language == 'S' ? (v ? "#t" : "#f") : (v ? "t" : "nil"), out);
  }

  @Override
  public void write (int v, Consumer out)
  {
    if (! getReadableOutput ())
      Char.print(v, out);
    else
      {
	if (language == 'E'
	    && v > ' ')
	  {
	    out.write('?');
            Char.print(v, out);
	  }
	// else if (language == 'E') ...
	else
	  write(Char.toScmReadableString(v), out);
      }
  }

  /**
   * Format a list.
   * 
   * Try to find shared structures in a list. To accomplish this, each subobject
   * is hashed to the idhash, which is used later to determine whether we've seen
   * a subobject before. There is an added complication when you consider cases
   * like this:
   * 
   * '((b . #1=(a . z)) 3)
   * 
   * It is not known in advance that the printer will have to emit an extra
   * ')' after the '(a . z) pair. Every time we CDR further into the list, we
   * push a position marker onto a stack. Once we've examined the tail of this
   * sublist, we pop all the posn markers off and tell the pretty printer that
   * it might have to emit an extra ')' if the corresponding posn marker 
   * becomes active.
   * 
   * @param value The list on which the method CDR's, termination occurs when
   * this becomes a non-pair or the empty list
   * @param out The output port that is responsible for the pretty printing
   */
  public void writeList(LList value, OutPort out)
  {
    PrettyWriter pout = out.getPrettyWriter();
    // The stack of position markers, populated by CDR'ing further into the list.
    int[] posnStack = null;
    int stackTail = 0;
    Object list = value;
    out.startLogicalBlock("(", false, ")");
    
    while (list instanceof Pair)
      {
	Pair pair = (Pair) list;
	writeObject(pair.getCar(), (Consumer) out);
	list = pair.getCdr();
        while (list instanceof Lazy)
          list = ((Lazy) list).getValue();
        if (list == LList.Empty)
          break;
        out.writeSpaceFill();
        if (! (list instanceof Pair))
	  {
	    out.write(". ");
	    writeObject(LList.checkNonList(list), (Consumer) out);
	    break;
	  }
	if (checkSharing)
	  {
	    
	    int hashIndex = pout.IDHashLookup(list);
	    int posn = pout.IDHashGetFromIndex(hashIndex);
	    if (posn == -1)
	      // Then this is a new (sub)object to be marked
	      {
		// writePositionMarker will return the index to which is was enqueued
		posn = pout.writePositionMarker(true);
		if (posnStack == null) 
		  posnStack = new int[128]; // should be plently for most cases.
		else if (stackTail >= posnStack.length)
		  {
		    int[] newStack = new int[posnStack.length << 1];
		    System.arraycopy(posnStack, 0, newStack, 0, stackTail);
		    posnStack = newStack;
		  }
		posnStack[stackTail++] = posn;
		// Mark (hash) this object
		pout.IDHashPutAtIndex(list, posn, hashIndex);
	      }
	    else
	      {
		out.write(".");
		out.writeSpaceFill();
		pout.writeBackReference(posn);
		list = LList.Empty;
		break;
	      }
	  }
      }
    for (;--stackTail >= 0;)
      pout.writePairEnd(posnStack[stackTail]);

    out.endLogicalBlock(")");
  }

  public void writeObject(Object obj, Consumer out)
  {
    PrettyWriter pout = ((OutPort) out).getPrettyWriter();
    boolean space = false;
    boolean skip = false;
    if (out instanceof OutPort
        && ! (obj instanceof gnu.kawa.xml.UntypedAtomic)
        && ! (obj instanceof Values)
        && (getReadableOutput()
            || ! (obj instanceof Char
                  /* #ifdef use:java.lang.CharSequence */
                  || obj instanceof CharSequence
                  /* #else */
                  // || obj instanceof String || obj instanceof CharSeq
                  /* #endif */
                  || obj instanceof Character)))
      {
        ((OutPort) out).writeWordStart();
        space = true;
      }
    if (checkSharing && isInteresting(obj))
      {
	// The value returned from this hash is the respective index in the
	// queueInts[] from PrettyWriter to which this object should reference.
	int hashIndex = pout.IDHashLookup(obj);
	int posn = pout.IDHashGetFromIndex(hashIndex);
	if (posn == -1)
	  {
	    // Find the position in the queueInts that future (if any) backreferences
	    // should reference
	    posn =  pout.writePositionMarker(false);
	    // Mark (hash) this object
	    pout.IDHashPutAtIndex(obj, posn, hashIndex);
	    // Print the object, instead of emitting print-circle notation
	    skip = false;
	  }
	else
	  // This object is referring to another part of the expression.
	  {
	    // Activate the referenced position marker
	    pout.writeBackReference(posn);
	    // This object is referring to the structure, we shall not print it and
	    // instead we shall emit print-circle notation.
	    skip = true;
	    // Format a fill space after the #N# token
	    space = true;
	  }
      }
    if (!skip)
      writeObjectRaw(obj, out);
    if (space)
      ((OutPort) out).writeWordEnd();
  }

  public void writeObjectRaw(Object obj, Consumer out)
  {
    if (! readable)
      {
        while (obj instanceof Lazy<?>)
          obj = ((Lazy<?>) obj).getValue();
      }
    if (obj instanceof Boolean)
      writeBoolean(((Boolean)obj).booleanValue(), out);
    else if (obj instanceof Char)
      write(((Char) obj).intValue(), out);
    else if (obj instanceof Character)
      write(((Character) obj).charValue(), out);
    else if (obj instanceof Symbol)
      {
        Symbol sym = (Symbol) obj;
        if (sym.getNamespace() == XmlNamespace.HTML)
          {
            write("html:", out);
            write(sym.getLocalPart(), out);
          }
        else
          writeSymbol(sym, out, readable);
      }
    /* #ifdef use:java.net.URI */
    /* #ifdef use:java.lang.CharSequence */
    else if (obj instanceof java.net.URI && getReadableOutput()
             && out instanceof PrintWriter)
      {
        write("#,(URI ", out);
        Strings.printQuoted(obj.toString(), (PrintWriter) out, 1);
        out.write(')');
      }
    /* #endif */
    /* #endif */
    else if
      /* #ifdef use:java.lang.CharSequence */
      (obj instanceof CharSequence) 
      /* #else */
      // (obj instanceof CharSeq || obj instanceof String) 
      /* #endif */
      {
        /* #ifdef use:java.lang.CharSequence */
	CharSequence str = (CharSequence) obj;
        /* #else */
	// String str = obj.toString();
        /* #endif */
	if (getReadableOutput () && out instanceof PrintWriter)
	  Strings.printQuoted(str, (PrintWriter) out, 1);
        else if (obj instanceof String)
          {
            out.write((String) obj);
          }
	else if (obj instanceof CharSeq)
          {
            CharSeq seq = (CharSeq) obj;
            seq.consume(0, seq.size(), out);
          }
        else
          {
            int len = str.length();
            for (int i = 0; i < len;  i++)
              out.write(str.charAt(i));
          }
      }
    else if (obj instanceof LList && out instanceof OutPort)
      writeList((LList) obj, (OutPort) out);
    else if (obj instanceof SimpleVector)
      {
	SimpleVector vec = (SimpleVector) obj;
	String tag = vec.getTag();
	String start, end;
	if (language == 'E')
	  {
	    start = "[";
	    end = "]";
	  }
	else
	  {
	    start = tag == null ? "#(" : ("#" + tag + "(");
	    end = ")";
	  }
	if (out instanceof OutPort)
	  ((OutPort) out).startLogicalBlock(start, false, end);
	else
	  write (start, out);
	int endpos = vec.size() << 1;
	for (int ipos = 0;  ipos < endpos;  ipos += 2)
	  {
	    if (ipos > 0 && out instanceof OutPort)
	      ((OutPort) out).writeSpaceFill();
	    if (! vec.consumeNext(ipos, out))
	      break;
	  }
	if (out instanceof OutPort)
	  ((OutPort) out).endLogicalBlock(end);
	else
	  write (end, out);
      }
    else if (obj instanceof Array)
      {
	write((Array) obj, 0, 0, out);
      }
    /* #ifdef enable:XML */
    else if (obj instanceof KNode)
      {
        if (getReadableOutput())
          write("#", out);
        Writer wout = out instanceof Writer ? (Writer) out
          : new ConsumerWriter(out);
        XMLPrinter xout = new XMLPrinter(wout);
        xout.writeObject(obj);
        xout.closeThis();
      }
    /* #endif */
    else if (obj == Values.empty && getReadableOutput())
      write("#!void", out);
    else if (obj instanceof Consumable
             && (! readable || ! (obj instanceof Printable)))
      ((Consumable) obj).consume(out);
    else if (obj instanceof Printable)
      ((Printable) obj).print(out);
    else if (obj instanceof RatNum)
      {
        int b = 10;
        boolean showRadix = false;
        Object base = outBase.get(null);
        Object printRadix = outRadix.get(null);
        if (printRadix != null
            && (printRadix == Boolean.TRUE
                || "yes".equals(printRadix.toString())))
          showRadix = true;
        if (base instanceof Number)
          b = ((IntNum) base).intValue();
        else if (base != null)
          b = Integer.parseInt(base.toString());
        String asString = ((RatNum) obj).toString(b);
        if (showRadix)
          {
            if (b == 16)
              write("#x", out);
            else if (b == 8)
              write("#o", out);
            else if (b == 2)
              write("#b", out);
            else if (b != 10 || ! (obj instanceof IntNum))
              write("#"+base+"r", out);
          }
        write(asString, out);
        if (showRadix && b == 10 && obj instanceof IntNum)
          write(".", out);
      }
    else if (obj instanceof java.lang.Enum && getReadableOutput())
      {
        write(obj.getClass().getName(), out);
        write(":", out);
        write(((java.lang.Enum) obj).name(), out);
      }
    else
      {
        String asString;
        if (obj == null)
          asString = null;
        else
          {
            Class cl = obj.getClass();
            if (cl.isArray())
              {
                int len = java.lang.reflect.Array.getLength(obj);
                if (out instanceof OutPort)
                  ((OutPort) out).startLogicalBlock("[", false, "]");
                else
                  write("[", out);
                for (int i = 0;  i < len;  i++)
                  {
                    if (i > 0)
                      {
                        write(" ", out);
                        if (out instanceof OutPort)
                          ((OutPort) out).writeBreakFill();
                      }
                    writeObject(java.lang.reflect.Array.get(obj, i), out);
                  }
                if (out instanceof OutPort)
                  ((OutPort) out).endLogicalBlock("]");
                else
                  write("]", out);
                return;
              }
            asString = obj.toString();
          }
	if (asString == null)
	  write("#!null", out);
        else
          write(asString, out);
      }
  }

  /** Recursive helper method for writing out Array (sub-) objects.
   * @param array the Array to write out (part of).
   * @param index the row-major index to start
   * @param level the recurssion level, from 0 to array.rank()-1.
   * @param out the destination
   */
  int write(Array array, int index, int level, Consumer out)
  {
    int rank = array.rank();
    int count = 0;
    String start = level > 0 ? "("
      : rank == 1 ? "#("
      : "#" + rank + "a(";
    if (out instanceof OutPort)
      ((OutPort) out).startLogicalBlock(start, false, ")");
    else
      write (start, out);
    if (rank > 0)
      {
	int size = array.getSize(level);
	level++;
	for (int i = 0;  i < size;  i++)
	  {
	    if (i > 0)
              {
                write(" ", out);
                if (out instanceof OutPort)
                  ((OutPort) out).writeBreakFill();
              }
	    int step;
	    if (level == rank)
	      {
		writeObject(array.getRowMajor(index), out);
		step = 1;
	      }
	    else
	      step = write(array, index, level, out);
	    index += step;
	    count += step;
	  }
      }
    if (out instanceof OutPort)
      ((OutPort) out).endLogicalBlock(")");
    else
      write(")", out);
    return count;
  }

  /* #ifdef use:java.util.regex */
  static Pattern r5rsIdentifierMinusInteriorColons =
    Pattern.compile("(([a-zA-Z]|[!$%&*/:<=>?^_~])"
                    + "([a-zA-Z]|[!$%&*/<=>?^_~]|[0-9]|([-+.@]))*[:]?)"
                    + "|([-+]|[.][.][.])");
  /* #endif */

  void writeSymbol (Symbol sym, Consumer out, boolean readable)
  {
    String prefix = sym.getPrefix();
    Namespace namespace = sym.getNamespace();
    String uri = namespace == null ? null : namespace.getName();
    boolean hasUri = uri != null && uri.length() > 0;
    boolean hasPrefix = prefix != null && prefix.length() > 0;
    boolean suffixColon = false;
    if (namespace == Keyword.keywordNamespace)
      {
        if (language == 'C' || language == 'E')
          out.write(':');
        else
          suffixColon = true;
      }
    else if (hasPrefix || hasUri)
      {
        if (hasPrefix)
          writeSymbol(prefix, out, readable);
        if (hasUri && (readable || ! hasPrefix))
          {
            out.write('{');
            out.write(uri);
            out.write('}');
          }
        out.write(':');
      }
    writeSymbol(sym.getName(), out, readable);
    if (suffixColon)
      out.write(':');
  }

  void writeSymbol (String sym, Consumer out, boolean readable)
  {
    /* #ifdef use:java.util.regex */
    /* Use |...| if symbol doesn't follow R5RS conventions
       for identifiers or has a colon in the interior. */
    if (readable && ! r5rsIdentifierMinusInteriorColons.matcher(sym).matches())
      {
        int len = sym.length();
        if (len == 0)
          {
            write("||", out);
          }
        else
          {
            boolean inVerticalBars = false;
            for (int i = 0;  i < len;  i++)
              {
                char ch = sym.charAt(i);
                if (ch == '|')
                  {
                    write(inVerticalBars ? "|\\" : "\\", out);
                    inVerticalBars = false;
                  }
                else if (! inVerticalBars)
                  {
                    out.write('|');
                    inVerticalBars = true;
                  }
                out.write(ch);
              }
            if (inVerticalBars)
              out.write('|');
          }
        return;
      }
    /* #endif */
    write(sym, out);
  }
  
  /**
   * An "interesting" object is one where object identity is significant.
   *
   * Examples are vectors and lists.
   * No immutable values (for example symbols or numbers) are interesting
   * @param obj The object under test
   * @return true if this object is considered interesting
   */
  private boolean isInteresting (Object obj)
  {
    // FIXME Should probably consider empty lists and vectors, as well as FStrings
    return obj instanceof Pair || obj instanceof SimpleVector;
  }
}
