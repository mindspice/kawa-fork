package gnu.jemacs.buffer;
import javax.swing.text.*;
import gnu.lists.*;

public final class Marker extends SeqPosition implements Position
{
  Buffer buffer;

  /** Is this the special point marker? */
  public final boolean isPoint() { return buffer != null && sequence == null; }

  public Marker()
  {
  }

  public Marker(Marker marker)
  {
    buffer = marker.buffer;
    if (buffer != null)
      {
	if (marker.isPoint())
	  init(buffer.content, buffer.curPosition.getDot(), true);
	else
	  init(marker);
      }
  }

  public Marker (Buffer buffer, int offset, boolean isAfter)
  {
    super(buffer.content, offset, isAfter);
    this.buffer = buffer;
  }

  public int getOffset()
  {
    if (buffer == null)
      return -1;
    else if (isPoint())
      return buffer.curPosition.getDot();
    return nextIndex();
  }

  public int getPoint()
  {
    return 1 + getOffset();
  }

  public Buffer getBuffer()
  {
    return buffer;
  }

  public void setDot(int newPosition)
  {
    set(buffer, newPosition);
  }

  public void set(Buffer newBuffer, int newPosition)
  {
    if (isPoint())
      {
        if (newBuffer != buffer)
          {
            String msg;
            if (newBuffer == null)
              msg = "Can't make point-marker point nowhere: ";
            else
              msg = "Can't change buffer of point-marker: ";
            throw new Error(msg+this);
          }
	buffer.curPosition.setDot(newPosition);
      }
    else
      {
        if (sequence != null)
          release();
	sequence = null;
        if (newBuffer == null)
          {
            buffer = null;
            return;
          }

        if (newPosition < 0)
          newPosition = 0;
        else
          {
            int newLength = newBuffer.content.length();
            if (newPosition > newLength)
              newPosition = newLength;
          }
	buffer = newBuffer;
	init(newBuffer.content, newPosition, false);
      }
  }

  public void deleteChar(int count)
  {
    int point = getOffset();
    try
      {
        if (count < 0)
          {
            count = - count;
	    if (point - count < buffer.minDot())
	      Signal.signal("Beginning of buffer");
            point -= count;
          }
	else
	  {
	    if (point + count > buffer.maxDot())
	      Signal.signal("End of buffer");
	  }
        buffer.remove(point, count);

	// Should not be needed, but seems to be.  Otherwise, getDot()
	// returns its *old* value, which is `count' characters too high.
	// The problem seesm to be that Swing does not properly update
	// a Windows's caret position when the underlying Document has text
	// removed.  Unfortunately, this fix probably won't do the right
	// thing for *other windows* that reference the same buffer.  FIXME.
	// (Strangely, the correct thing seems to happen for insertions.)
	buffer.setDot(point);
      }
    catch (javax.swing.text.BadLocationException ex)
      {
        throw new Error("bad location: "+ex);
      }
  }

  public void insert (String string, Style style)
  {
    if (style == null)
      style = Buffer.defaultStyle;
    int point = getOffset();
    try
      {
        buffer.insertString(point, string, style);
      }
    catch (javax.swing.text.BadLocationException ex)
      {
        throw new Error("bad location: "+ex);
      }
    point += string.length();
    setDot(point);
  }

  /** Insert count copies of ch at the current position. */
  public void insert (char ch, int count, Style style)
  {
    if (count < 0)
      return;
    if (style == null)
      style = Buffer.defaultStyle;
    int todo = count > 500 ? 500 : count;
    StringBuffer sbuf = new StringBuffer(todo);
    for (int i = todo;  --i >= 0; )
      sbuf.append(ch);
    String str = sbuf.toString();
    int point = getOffset();
    for (;;)
      {
	try
	  {
	    System.err.println("insertStr '"+ch+"' point:"+point);
	    buffer.insertString(point, str, style);
	  }
	catch (javax.swing.text.BadLocationException ex)
	  {
	    throw new Error("bad location: "+ex);
	  }
	point += todo;
	count -= todo;
	if (count == 0)
	  break;
	if (count < 500)
	  {
	    todo = count;
	    sbuf.setLength(todo);
	    str = sbuf.toString();
	  }
      }
    setDot(point);
  }

  public void forwardChar(int i)
  {
    int point = getOffset();
    int max = buffer.maxDot();
    if (point + i > max)
      {
	point = max;
	Signal.signal("End of buffer");
      }
    point += i;
    setDot(point);
  }

  public void backwardChar(int i)
  {
    int point = getOffset();
    if (point < i)
      {
	point = 0;
	Signal.signal("Beginning of buffer");
      }
    point -= i;
    setDot(point);
  }

  public int currentColumn()
  {
    return buffer.currentColumn(getOffset());
  }

  // force is currently ignored FIXME
  public int moveToColumn(int column, boolean force)
  { 
    int lineStart = buffer.lineStartOffset(getOffset());
    BufferReader port
      = new BufferReader(buffer, lineStart, buffer.maxDot() - lineStart);
    int resultColumn = 0;
    int offset = lineStart;
    for (;;)
      {
	int ch = port.read();
	if (ch < 0 || ch == '\n')
	  {
	    if (force)
	      {
		// FIXME
	      }
	    break;
	  }
	int width = buffer.charWidth((char) ch, resultColumn);
	offset++;
	resultColumn += width;
	if (resultColumn >= column)
	  {
	    if (resultColumn > column && force)
	      {
		// FIXME
	      }
	    break;
	  }
      }
    setDot(offset);
    return resultColumn;
  }

  public int forwardLine(int lines)
  {
    long value = buffer.forwardLine(lines, getOffset());
    setDot((int) value);
    return (int) (value >> 32);
  }

  /** Move to start of frame line COUNTs down.
   * Assume window width is WIDTH.
   * If LINES is negative, this is moving up. */

  /*
  public int verticalMotion(int count, int width)
  {
    if (count == 0)
      {
	moveToColumn ((currentColumn() / width) * width, false);
	return 0;
      }
    if (count > 0)
      {
	int todo = count + currentColumn() / width;
	endOfLine();
	// The loop iterates over buffer lines;
	// H is the number of screen lines in the current line, i.e.
	// the ceiling of dividing the buffer line width by width.
	for (;;)
	  {
	    int h = (currentColumn() + width - 1) / width;
	    if (h <= 0) h = 1;
	    if (h > todo)
	      break;
	    if (eobp())
	      break;
	    todo -= h;
	    forwardChar(1);  // move past '\n'.
	    endOfLine();  // and on to the end of the next line.
	  }
	if (todo >= h && todo > 0)
	  return count - todo + h - 1; // Hit end of buffer.
      }
    else // count > 0 -- Similar algorithm, but for upward motion.
      {
	int todo = - count;
	for (;;)
	  {
	    int h = (currentColumn() + width - 1) / width;
	    if (h <= 0) h = 1;
	    if (h > todo)
	      break;
	    beginningOfLine();
	    if (bobp())
	      break;
	    todo -= h;
	    backwardChar(1); // Move to end of previous line
	  }
	if (todo >= h && todo > 0)
	  return count + todo - 1 + h; // Hit beginning of buffer.
	todo = h - todo - 1;
      }
    moveToColumn(todo * width, false);
    return count;
  }
  */

  public boolean isBeginningOfLine()
  {
    int offset = getOffset();
    return offset == 0 || buffer.content.charAt(offset - 1) == '\n';
  }

  public boolean isEndOfLine()
  {
    int offset = getOffset();
    BufferContent content = buffer.content;
    return offset == content.length() || content.charAt(offset) == '\n';
  }

  public int hashCode()
  {
    if (buffer == null)
      return 0;
    return buffer.hashCode() ^ getOffset();
  }

  public boolean equals (Object other)
  {
    if (! (other instanceof Marker))
      return false;
    Marker m2 = (Marker) other;
    return buffer == m2.buffer && getOffset() == m2.getOffset();
  }

  public String toString()
  {
    if (buffer == null)
      return "#<marker in no buffer>";
    StringBuffer sbuf = new StringBuffer(80);
    sbuf.append("#<marker at ");
    sbuf.append(getPoint());
    sbuf.append(" in ");
    sbuf.append(buffer.getName());
    sbuf.append('>');
    return sbuf.toString();
  }
}
