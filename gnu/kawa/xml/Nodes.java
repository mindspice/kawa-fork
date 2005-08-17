// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.Values;
import gnu.lists.*;
import gnu.xml.*;
import org.w3c.dom.*;

/** Manages a sequence of node references. */

public class Nodes extends Values
  /* #ifdef use:org.w3c.dom.Node */
  // implements org.w3c.dom.NodeList
  /* #endif */
{
  /** Number of data elements for a POSITION_PAIR_FOLLOWS node reference. */
  static final int POS_SIZE = 5;

  int count;

  int nesting = 0;
  NodeTree curFragment;

  public void writePosition (AbstractSequence seq, int ipos)
  {
    count++;
    super.writePosition(seq, ipos);
  }

  public int find (Object seq)
  {
    // See if can re-use the object index of the position before the gap.
    if (gapStart > 0)
      {
	int oindex = getIntN(gapStart - POS_SIZE + 1);
	if (objects[oindex] == seq)
	  return oindex;
      }
    // See if can re-use the object index of the position after the gap.
    if (gapEnd < data.length)
      {
	int oindex = getIntN(gapEnd + 1);
	if (objects[oindex] == seq)
	  return oindex;
      }
    return super.find(seq);
  }

  public void writeObject(Object v)
  {
    if (curFragment != null)
      {
	if (nesting == 0
	    && (v instanceof SeqPosition || v instanceof TreeList))
	  finishFragment();
	else
	  {
	    curFragment.writeObject(v);
	    return;
	  }
      }
    if (v instanceof SeqPosition)
      {
	SeqPosition seq = (SeqPosition) v;
	writePosition(seq.sequence, seq.ipos);
	return;
      }
    if (v instanceof TreeList)
      {
	TreeList tlist = (TreeList) v;
	writePosition(tlist, 0);
	return;
      }
    startFragment();
    curFragment.writeObject(v);
    return;
  }

  void maybeStartTextNode ()
  {
    if (curFragment == null)
      {
	startFragment();
	// OR: throw some exception.
      }
  }

  public void writeFloat (float v)
  {
    maybeStartTextNode();
    curFragment.writeFloat(v);
  }

  public void writeDouble (double v)
  {
    maybeStartTextNode();
    curFragment.writeDouble(v);
  }

  public void writeLong(long v)
  {
    maybeStartTextNode();
    curFragment.writeLong(v);
  }

  public void writeInt(int v)
  {
    maybeStartTextNode();
    curFragment.writeInt(v);
  }

  public void writeChar (int v)
  {
    maybeStartTextNode();
    curFragment.writeChar(v);
  }

  public void writeBoolean (boolean v)
  {
    maybeStartTextNode();
    curFragment.writeBoolean(v);
  }

  public void writeChars (String str)
  {
    maybeStartTextNode();
    curFragment.writeChars(str);
  }

  public void write(char[] buf, int off, int len)
  {
    maybeStartTextNode();
    curFragment.write(buf, off, len);
  }

  private void maybeStartNonTextNode ()
  {
    if (curFragment != null && nesting == 0)
      finishFragment();
    if (curFragment == null)
      startFragment();
    nesting++;
  }

  private void maybeEndNonTextNode ()
  {
    if (--nesting == 0)
      finishFragment();
  }

  public void beginGroup(String typeName, Object type)
  {
    maybeStartNonTextNode();
    curFragment.beginGroup(typeName, type);
  }

  public void endGroup(String typeName)
  {
    curFragment.endGroup(typeName);
    maybeEndNonTextNode();
  }

  public void beginAttribute(String attrName, Object attrType)
  {
    maybeStartNonTextNode();
    curFragment.beginAttribute(attrName, attrType);
  }

  public void endAttribute()
  {
    curFragment.endAttribute();
    maybeEndNonTextNode();
  }

  public void writeBaseUri (Object uri)
  {
    maybeStartNonTextNode();
    curFragment.writeBaseUri(uri);
  }

  public void writeComment(char[] chars, int offset, int length)
  {
    maybeStartNonTextNode();
    curFragment.writeComment(chars, offset, length);
    maybeEndNonTextNode();
  }

  public void writeCDATA(char[] chars, int offset, int length)
  {
    maybeStartNonTextNode();
    curFragment.writeCDATA(chars, offset, length);
  }

  public void writeProcessingInstruction(String target, char[] content,
					 int offset, int length)
  {
    maybeStartNonTextNode();
    curFragment.writeProcessingInstruction(target, content, offset, length);
    maybeEndNonTextNode();
  }

  public void beginDocument()
  {
    maybeStartNonTextNode();
    curFragment.beginDocument();
  }

  public void endDocument()
  {
    curFragment.endDocument();
    maybeEndNonTextNode();
  }

  void startFragment ()
  {
    curFragment = new NodeTree();
    writePosition(curFragment, 0);
  }

  void finishFragment ()
  {
    curFragment = null;
  }

  public int size()
  {
    return count;
  }

  public int getLength()
  {
    return count;
  }

  public Object get (int index)
  {
    int i = POS_SIZE * index;
    if (i >= gapStart)
      i += gapEnd - gapStart;
    if (i < 0 || i >= data.length)
      throw new IndexOutOfBoundsException();
    // Inline of: return getPosNext(i << 1)
    if (data[i] != POSITION_PAIR_FOLLOWS)
      throw new RuntimeException("internal error - unexpected data");
    return KNode.make((NodeTree) objects[getIntN(i+1)], getIntN(i+3));
  }

  public Node item(int index)
  {
    if (index >= count)
      return null;
    else
      return (Node) item(index);
  }

  public Object getPosNext(int ipos)
  {
    int index = posToDataIndex(ipos);
    if (index == data.length)
      return Sequence.eofValue;
    if (data[index] != POSITION_PAIR_FOLLOWS)
      throw new RuntimeException("internal error - unexpected data");
    return KNode.make((NodeTree) objects[getIntN(index+1)], getIntN(index+3));
  }

  /** Optimization of ((SeqPosition) get(index)).sequence.
   * However returns null instead of throwing IndexOutOfBoundsException
   * if index >= count. */
  public AbstractSequence getSeq (int index)
  {
    int i = POS_SIZE * index;
    if (i >= gapStart)
      i += gapEnd - gapStart;
    if (i < 0 || i >= data.length)
      return null;
    // Inline of: return getPosNext(i << 1)
    if (data[i] != POSITION_PAIR_FOLLOWS)
      throw new RuntimeException("internal error - unexpected data");
    return (AbstractSequence) objects[getIntN(i+1)];
  }

  /** Optimization of ((SeqPosition) get(index)). ipos. */
  public int getPos (int index)
  {
    int i = POS_SIZE * index;
    if (i >= gapStart)
      i += gapEnd - gapStart;
    // Inline of: return getPosNext(i << 1)
    if (data[i] != POSITION_PAIR_FOLLOWS)
      throw new RuntimeException("internal error - unexpected data");
    return getIntN(i+3);
  }

  private static KNode root (NodeTree seq, int ipos)
  {
    int end = seq.endPos();
    for (;;)
      {
	int parent = seq.parentPos(ipos);
	if (parent == end)
	  return KNode.make(seq, ipos);
	ipos = parent;
      }
  }

  /** Return the root node of the argument. */
  public static SeqPosition root (Object node)
  {
    KNode spos = (KNode) node;
    return root((NodeTree) spos.sequence, spos.getPos());
  }
}
