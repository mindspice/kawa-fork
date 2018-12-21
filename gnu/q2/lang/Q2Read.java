package gnu.q2.lang;
import gnu.kawa.lispexpr.*;
import gnu.expr.*;
import gnu.text.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.Keyword;
import gnu.kawa.io.InPort;
import gnu.kawa.xml.MakeAttribute;
import kawa.lang.*;

/** A class to read Scheme forms (S-expressions). */

public class Q2Read extends LispReader
{
    public static Symbol wordSym = Symbol.valueOf("$word$");

  void init()
  {
    ((InPort) port).readState = ' ';
  }

  public Q2Read(InPort port)
  {
    super(port);
    init();
  }
  
  public Q2Read(InPort port, SourceMessages messages)
  {
    super(port, messages);
    init();
  }

  /** Skip initial tabs and spaces.
   * @return indentation, encoded as @{code (numberOfTabs<<16)+numberOfSpaces}.
   */
  int skipIndentation ()
      throws java.io.IOException, SyntaxException
  {
    int numTabs = 0, numSpaces = 0;
    int ch = port.read();
    while (ch == '\t')
      {
	numTabs++;
	ch = port.read();
      }
    while (ch == ' ')
      {
	numSpaces++;
	ch = port.read();
      }
    if (ch < 0)
      return -1;
    port.unread();
    return (numTabs << 16) + numSpaces;
  }

  int curIndentation;
  boolean resetNeeded;

  /** Read a "command".
   * Assume curIndentation has been set.
   * After return, the read position is before the first non-WS character
   * of the next command (on the next line); curIndentation has been
   * updated to that of the initial whitespace of that line; and a
   * mark() has been set at the start of the line. 
   * Exception: If singleLine, returned position is *before* newline,
   * and mark is not set.
   */
  Object readIndentCommand (boolean singleLine)
    throws java.io.IOException, SyntaxException
  {
    int startIndentation = curIndentation;
    Pair head = new Pair(null, LList.Empty);
    Pair last = head;
    Object obj = LList.Empty;
    PairWithPosition pair = null;
    Object prev = null;
    ReadTable rtable = ReadTable.getCurrent();

    for (;;)
      {
	int ch = read();
	if (ch < 0)
	  break;
	if (ch == ' ' || ch == '\t')
	  continue;
	unread();
	if (ch == ')')
	  break;
        if (ch == '\r' || ch == '\n')
          {
            Operator rhsNeeded = null;
	    if (singleLine)
	      {
                  prev = last.getCar();
                  Q2Translator tr = (Q2Translator) Compilation.getCurrent();
                  Operator op = tr.checkIfOperator(prev);
                  if (op != null && (op.flags & Operator.RHS_NEEDED) != 0)
                      rhsNeeded = op;
                  else
                      break;
	      }
	    ch = read(); // re-read newline
            port.mark(Integer.MAX_VALUE);
            resetNeeded = true;
	    int subIndentation = skipIndentation(); // skipHorSpace.
            if (subIndentation == -1 && rhsNeeded != null)
                eofError("missing right operand after "+rhsNeeded.getName());
            LList qresult = LList.Empty;
            curIndentation = subIndentation;
            for (;;)
              {
                if (curIndentation == -1)
                  break;
                if (subIndentation != curIndentation)
                  {
                    break;
                  }
                int comparedIndent = Q2.compareIndentation(subIndentation, startIndentation);
                if (comparedIndent == Integer.MIN_VALUE)
                  {
                    error('e', "cannot compare indentation - mix of tabs and spaces");
                    break;
                  }
                if (comparedIndent == -1 || comparedIndent == 1)
                  {
                    error('e', "indentation must differ by 2 or more");
                  }
                else if (comparedIndent <= 0)
                  {
                    // reset to start of line FIXME
                    break;
                  }
                // comparedIndent >= 2
                int line = port.getLineNumber();
                int column = port.getColumnNumber();
                Object val = readIndentCommand(false);
                if (val == LList.Empty)
                  break;
                qresult = makePair(val, qresult, line, column);
              }
            if (qresult != LList.Empty)
              {
                qresult = new Pair(kawa.standard.begin.begin,
                                   LList.reverseInPlace(qresult));
                Pair t = new Pair(qresult, LList.Empty);
                last.setCdrBackdoor(t);
                last = t;
              }
            prev = qresult;
            break;
          }
        int line = port.getLineNumber();
        int column = port.getColumnNumber();
        ch = port.read();
        if (ch < 0)
          break;
        last = readValuesAndAppend(ch, rtable, last);
      }
    return makeCommand(head.getCdr());
  }

    private boolean isSubWordStart(int ch, ReadTable rtable) {
        if (ch == '{' || ch == '[' || ch == '(')
            return true;
	int kind = rtable.lookup(ch).getKind();
        return kind != ReadTable.TERMINATING_MACRO
            && kind != ReadTable.WHITESPACE
            && kind != ReadTable.ILLEGAL;
    }

    public Pair readValuesAndAppend(int ch, ReadTable rtable, Pair last)
            throws java.io.IOException, SyntaxException {
        int line = port.getLineNumber();
        int column = port.getColumnNumber() - 1; // Adjust for ch
        Pair next = super.readValuesAndAppend(ch, rtable, last);
        ch = port.peek();
        if (isSubWordStart(ch, rtable)) {
            Pair head = makePair(wordSym, line, column);
            setCdr(head, next);
            Pair first = next;
            Pair last2 = next;
            for (;;) {
                last2 = super.readValuesAndAppend(port.read(), rtable, last2);
                ch = port.peek();
                if (! isSubWordStart(ch, rtable))
                    break;
            }
            if (first.getCar() instanceof SimpleSymbol) {
                Symbol op = LispLanguage.constructNamespace.getSymbol(first.getCar().toString());
                head = makePair(op, line, column);
                setCdr(head, first.getCdr());
            }
            next = makePair(head, line, column);
            setCdr(last, next);
        }
        return next;
    }

    Object makeCommand (Object command)
  {
    return command;
  }

  boolean singleLine()
  {
    return isInteractive() && nesting <= 1;
  }

  public Object readCommand ()
      throws java.io.IOException, SyntaxException
  {
    int indent = skipIndentation();
    if (indent < 0)
      return Sequence.eofValue;
    curIndentation = indent;
    char saveReadState = pushNesting('-');
    try
      {
        Object result = readIndentCommand(singleLine());
        if (resetNeeded)
          {
            resetNeeded = false;
            int line = port.getLineNumber();
            int column = port.getColumnNumber();
            port.reset();
          }
        if (result instanceof Pair)
          {
            Pair presult = (Pair) result;
            if (presult.getCdr() == LList.Empty
                && presult.getCar() == Special.eof)
              return Special.eof;
          }
        return result;
      }
    finally
      {
        popNesting(saveReadState);
      }
 }

    @Override
    protected boolean isTerminatingChar(int ch, ReadTable rtable)
        throws java.io.IOException, SyntaxException
    {
        return ch == '^' || super.isTerminatingChar(ch, rtable);
    }

    @Override
    protected Object handlePostfix(Object value, ReadTable rtable,
                                   int line, int column)
        throws java.io.IOException, SyntaxException {
        if (port.peek() == '^') {
            port.read();
            int rline = port.getLineNumber();
            int rcolumn = port.getColumnNumber();
            int ch = port.read();
            LList r;
            if (ch < 0 || ch == ']' || ch == ')' || ch == '}' ||
                Character.isWhitespace(ch)) {
                unread(ch);
                r = LList.Empty;
            } else {
                Object rightOperand
                    = readValues(ch, rtable.lookup(ch), rtable, -1);
                r = makePair(rightOperand, rline, rcolumn,
                             port.getLineNumber(), port.getColumnNumber());
            }
            r = Pair.make(value, r);
            return Pair.make(Q2.defineSym, r);
        }
        return super.handlePostfix(value, rtable, line, column);
    }

  // RULE: Every newline (not preceded by backslash)
  //   is equivalent to ';'
  // RULE: If a line is followed by one or more lines that are
  //   indented more, add a '(BEGIN' at the end of this line, and
  //   and a ')' at the end of the last line indented more.
  // RULE: Forms separate ';' make a "block": Each form is
  //   either a declaration (visible throughout the block);
  //   or an expression.  Value of block is concatenation of
  //   values of expressions (producting multiple values).
  //   
  /* if parens:
     x + (a b
            c d
              e f) + y
    == x + (a b (c d (e f))) + y [OLD]
    == x + (a b (c d (; e f))) + y OR[*]
    == x + (a b; c d (; e f)) + y
    [*] New RULE[?]: Indentation relative to most recent lparen

    What about:
       x + (a b
        c d
        e f) + y
    == x + (a b (; c d (; e f))) + y OR
       ERROR
     */
    /*
      a b (d e
        f g h)
     */
    /*
      a b c
        d e
    */
    /* <body>
       [%x%]
          a b c
          e f g
            h i
          j k
      == [%x%] (CONCAT (a b c) (e f g (h i)) (j k))
      == [%x%] (; a b c; e f g (; h i); j k)

       [%x%] a b c
          e f g
      == ???
       [%x%] a b c (e f g)
      OR:
       [%x%] (CONCAT (a b c) (e f g))
      == [%x%] a b c (; e f g)

    f a b c
        d e
    == f a (b c) (d e) [probably not]
    == f a b c (; d e)

    if e1
    then
       a b
       c d
    else
       e f
       g h
    ==
      if e1 then (CONCAT (a b) (c d)) else (CONCAT (e f) (g h))

    f
      a b
      c d
    == f (CONCAT (a b) (c d))
    OR f (a b) (c d)
    == f (; a b; c d)
    DEPENDING ON f
    Even if former, what about explicit:
    f (a b) (c d)
    Same?
    Depends on whether f takes a <body> or an <arguments>
    */

  /*
  public Object readCommand (boolean forceList)
      throws java.io.IOException, SyntaxException
  {
    int line = port.getLineNumber();
    int startColumn = port.getColumnNumber();
    int lastColumn = startColumn;
    Object obj = LList.Empty;
    PairWithPosition pair = null, last = null;
    for (;;)
      {
	int ch = read();
	if (ch < 0)
	  break;
	if (ch == ' ' || ch == '\t')
	  continue;
	unread();
	if (ch == ')')
	  break;
	line = port.getLineNumber();
	int column = port.getColumnNumber();
	while (ch == '\r' || ch == '\n')
	  {
	    if (singleLine())
	      return obj;
	    ch = read();
	    skipIndentation(); // skipHorSpace.
	    column = port.getColumnNumber();
	    ch = peek();
	    if (column <= startColumn)
	      break;
	  }
	if (column <= startColumn && last != null)
	  break;
	Object next;
	if (column == lastColumn && last != null)
	  next = readCommand();
	else if (column < lastColumn && last != null)
	  {
	    PairWithPosition p = pair;
	    for (;;)
	      {
		Object n = p.getCdr();
		if (n == LList.Empty)
		  break;
		PairWithPosition np = (PairWithPosition) n;
		int pColumn = np.getColumnNumber()-1;
		if (pColumn >= column)
		  {
		    if (pColumn > column)
		      error('e', "some tokens on previous line indented more than current line");
		    n = np.getCdr();
		    if (n != LList.Empty)
		      {
			if (((PairWithPosition) n).getColumnNumber()-1==column)
			  {
			    p = (PairWithPosition) n;
			    continue;
			  }
			last = (PairWithPosition)
			  makePair(np, port.getLineNumber(), column);
			p.setCdrBackdoor(last);
		      }
		    break;
		  }
		p = np;
	      }
	    next = readCommand();
	  }
	else
	  next = readObject();
	if (next == Sequence.eofValue)
	  break;
	lastColumn = column;
	String filename = port.getName();
	PairWithPosition cur = PairWithPosition.make(next, LList.Empty,
						     filename, line+1, column+1);
	if (last == null)
	  {
	    pair = cur;
	    obj = cur;
	  }
	else if (last.getCar() instanceof Keyword)
	  {
	    Object name = new QuoteExp(((Keyword) last.getCar()).getName());
	    last.setCar(new PairWithPosition(last, MakeAttribute.makeAttribute,
                                              new PairWithPosition(last, name, cur)));
	    continue;
	  }
	else
	  last.setCdrBackdoor(cur);
	last = cur;
      }
    if (! forceList)
      {
	if (obj == last)
	  obj = last.getCar();
	else if (last == null)
	  obj = QuoteExp.voidExp;
      }
    return obj;
  }
  */

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new Q2Read(port)).readObject();
  }

  /** Record '[' location for error messages. */ 
  String expressionStartFile;
  int expressionStartLine;
  int expressionStartColumn;

  void saveExpressionStartPosition()
  {
    expressionStartFile = port.getName();
    expressionStartLine = port.getLineNumber();
    expressionStartColumn = port.getColumnNumber();
  }

    public static final ReaderExtendedLiteral braces
        = new ReaderExtendedLiteral('\\');

 static class ReadTableEntry extends ReaderDispatchMisc
  {
    public Object read (Lexer in, int ch, int count)
      throws java.io.IOException, SyntaxException
    {
      switch (ch)
        {
        case '(':  return readParens(in);
        case ';':  return Symbol.valueOf(";");
        case '{':
            int startLine = in.getLineNumber() + 1;
            int startColumn = in.getColumnNumber() - 2;
            ReadTable rtable = ReadTable.getCurrent();
            return braces.readNamedLiteral((LispReader) in, rtable,
                                           null,  '{', startLine, startColumn);
        case '|':
            in.error("unexpected '|'");
            return Values.empty;
        default:
            throw new Error();
        }
    }

    public Object readParens (Lexer in)
      throws java.io.IOException, SyntaxException
    {
      Q2Read reader = (Q2Read) in;
      char saveReadState = reader.pushNesting('(');
      int startLine = reader.getLineNumber();
      //set('(',  ReaderParens.getInstance('(', ')'));
      int startColumn = reader.getColumnNumber();
      InPort port = reader.getPort();
      boolean lambdaParamList = false;
      if (port.peek() == '|') {
          lambdaParamList = true;
          port.skip();
          ReadTable rtable = ReadTable.getCurrent();
          Pair head = new Pair(null, LList.Empty);
          Pair last = head;
          for (;;) {
             int ch = reader.read();
             if (ch < 0)
               reader.eofError("unexpected EOF in vector starting here",
                              startLine + 1, startColumn);
             if (ch == '|' && reader.peek() == ')') {
                 port.skip();
                 break;
             }
             if (rtable.lookup(ch).getKind() == ReadTable.WHITESPACE)
                 continue;
             last = reader.readValuesAndAppend(ch, rtable, last);
           }
          return Operator.makeLambda(head.getCdr());
      }
      try
        {
          Object result = reader.readIndentCommand(false);
          int ch = port.peek();
          if (ch == ')')
            port.skip();
          else {
              String msg = "missing ')' after '(' starting here";
              if (ch < 0)
                  reader.eofError(msg, startLine + 1, startColumn);
              else
                  reader.error('e', port.getName(), startLine + 1, startColumn,
                               msg);
          }
          if (reader.resetNeeded)
            {
              reader.resetNeeded = false;
              port.mark(0);
            }
          return reader.makeCommand(result);
        }
      finally
        {
          reader.popNesting(saveReadState);
        }
    }
  }

}
