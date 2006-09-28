// Copyright (c) 2001, 2004, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.Values;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.NodeTree;
import gnu.kawa.xml.*;
/* #ifdef use:org.w3c.dom.Node */
// import org.w3c.dom.Node;
/* #endif */

public class SequenceUtils
{
  public static Object zeroOrOne (Object arg)
  {
    // Assumes arg is normalized.
    if (arg instanceof Values && ! ((Values) arg).isEmpty())
      throw new IllegalArgumentException();
    return arg;
  }

  public static Object oneOrMore (Object arg)
  {
    if (arg instanceof Values && ((Values) arg).isEmpty())
      throw new IllegalArgumentException();
    return arg;
  }

  public static Object exactlyOne (Object arg)
  {
    // Assumes arg is normalized.
    if (arg instanceof Values)
      throw new IllegalArgumentException();
    return arg;
  }

  public static boolean isEmptySequence(Object arg)
  {
    return arg instanceof Values && ((Values) arg).isEmpty();
  }

  public static boolean exists (Object arg)
  {
    return ! (arg instanceof Values && ((Values) arg).isEmpty());
  }

  public static void remove$X (Object arg, long position, CallContext ctx)
  {
    Consumer out = ctx.consumer;
    if (arg instanceof Values)
      {
        Values values = (Values) arg;
        int ipos  = 0;
        long i = 0;
        for (;;)
          {
            int next = values.nextPos(ipos);
            if (next == 0)
              break;
            if (++i != position )
              values.consumePosRange(ipos, next, out);
            ipos = next;
          }
      }
    else if (position != 1)
      out.writeObject(arg);
  }

  /** Implements the standard XQuery function {@code reverse}. */
  public static void reverse$X (Object arg, CallContext ctx)
  {
    Consumer out = ctx.consumer;
    if (! (arg instanceof Values))
      {
        out.writeObject(arg);
        return;
      }
    Values vals = (Values) arg;
    int ipos = 0;
    int[] poses = new int[100];
    int n = 0;
    for (;;)
      {
        if (n >= poses.length)
          {
            int[] t = new int[2 * n];
            System.arraycopy(poses, 0, t, 0, n);
            poses = t;
          }
        poses[n++] = ipos;
        ipos = vals.nextPos(ipos);
        if (ipos == 0)
          break;
      }
    for (int i = n-1;  --i >= 0; )
      vals.consumePosRange(poses[i], poses[i+1], out);
  }

  public static void indexOf$X (Object seqParam, Object srchParam,
                                NamedCollator collator, CallContext ctx)
  {
    Consumer out = ctx.consumer;
    if (seqParam instanceof Values)
      {
        Values vals = (Values) seqParam;
        int ipos = vals.startPos();
        int i = 1;
        for (; (ipos = vals.nextPos(ipos)) != 0; i++)
          if (Compare.apply(Compare.TRUE_IF_EQU,
                            vals.getPosPrevious(ipos),
                            srchParam, collator))
            out.writeInt(i);
      }
    else if (Compare.apply(Compare.TRUE_IF_EQU, seqParam, srchParam, collator))
      out.writeInt(1);
  }

  public static final NodeType textOrElement
    = new NodeType("element-or-text", NodeType.GROUP_OK|NodeType.TEXT_OK);

  public static boolean deepEqualChildren (NodeTree seq1, int ipos1,
                                           NodeTree seq2, int ipos2,
                                           NamedCollator collator)
  {
    NodeType filter = textOrElement;
    int child1 = seq1.firstChildPos(ipos1, filter);
    int child2 = seq2.firstChildPos(ipos2, filter);
    for (;;)
      {
	if (child1 == 0 || child2 == 0)
          return child1 == child2;
        if (! deepEqual(seq1, child1, seq2, child2, collator))
          return false;
        child1 = seq1.nextMatching(child1, filter, -1, false);
        child2 = seq2.nextMatching(child2, filter, -1, false);
      }
  }

  public static boolean deepEqual (NodeTree seq1, int ipos1,
                                   NodeTree seq2, int ipos2,
                                   NamedCollator collator)
  {
    int kind1 = seq1.getNextKind(ipos1);
    int kind2 = seq1.getNextKind(ipos2);
    switch (kind1)
      {
      case Sequence.GROUP_VALUE:
        if (kind1 != kind2)
          return false;
        // Assumes local-name and namespace-URI are interned.
        String loc1 = seq1.posLocalName(ipos1);
        String loc2 = seq2.posLocalName(ipos2);
        if (loc1 != loc2)
          return false;
        String ns1 = seq1.posNamespaceURI(ipos1);
        String ns2 = seq2.posNamespaceURI(ipos2);
        if (ns1 != ns2)
          return false;
        int attr1 = seq1.firstAttributePos(ipos1);
        int nattr1 = 0;
        for (;;)
          {
            if (attr1 == 0
                || seq1.getNextKind(attr1) != Sequence.ATTRIBUTE_VALUE)
              break;
            nattr1++;
            String local = seq1.posLocalName(attr1);
            String ns = seq1.posNamespaceURI(attr1);
            int attr2 = seq2.getAttributeI(ipos2, ns, local);
            if (attr2 == 0)
              return false;
            String aval1 = KNode.getNodeValue(seq1, attr1);
            String aval2 = KNode.getNodeValue(seq2, attr2);
            if (! Compare.atomicCompare(Compare.TRUE_IF_EQU,
                                        aval1, aval2,
                                        collator))
              return false;
            attr1 = seq1.nextPos(attr1);
          }
        int nattr2 = seq2.getAttributeCount(ipos2);
        if (nattr1 != nattr2)
          return false;
        /* ... fall through ... */
      case Sequence.DOCUMENT_VALUE:
        return deepEqualChildren(seq1, ipos1, seq2, ipos2, collator);
      case Sequence.ATTRIBUTE_VALUE:
        if (seq1.posLocalName(ipos1) != seq2.posLocalName(ipos2)
            || seq1.posNamespaceURI(ipos1) != seq2.posNamespaceURI(ipos2))
          return false;
        return Compare.atomicCompare(Compare.TRUE_IF_EQU,
                                     KAttr.getObjectValue(seq1, ipos1),
                                     KAttr.getObjectValue(seq2, ipos2),
                                     collator);
      case Sequence.PROCESSING_INSTRUCTION_VALUE:
        if (! seq1.posTarget(ipos1).equals(seq2.posTarget(ipos2)))
          return false;
        return KNode.getNodeValue(seq1, ipos1)
          .equals(KNode.getNodeValue(seq2, ipos2));
      case Sequence.COMMENT_VALUE:
      case Sequence.CDATA_VALUE:
      default:
        if (kind1 != kind2)
          return false;
        return KNode.getNodeValue(seq1, ipos1)
          .equals(KNode.getNodeValue(seq2, ipos2));
      }
  }

  public static boolean deepEqual (Object arg1, Object arg2,
                                   NamedCollator collator)
  {
    if (arg1 == arg2)
      return true;
    if (arg1 == null || arg1 == Values.empty)
      return arg2 == null || arg2 == Values.empty;
    if (arg2 == null || arg2 == Values.empty)
      return false;
    int ipos1 = 1, ipos2 = 1;
    boolean is1seq = arg1 instanceof Values;
    boolean is2seq = arg2 instanceof Values;
    Values vals1 = is1seq ? (Values) arg1 : null;
    Values vals2 = is2seq ? (Values) arg2 : null;
    boolean first = true;
    for (;;)
      {
        if (is1seq)
          {
            if (first)
              ipos1 = vals1.startPos();
            ipos1 = vals1.nextPos(ipos1);
          }
        if (is2seq)
          {
            if (first)
              ipos2 = vals2.startPos();
            ipos2 = vals2.nextPos(ipos2);
          }
        if (ipos1 == 0 || ipos2 == 0)
          return ipos1 == ipos2;
        Object item1 = is1seq ? vals1.getPosPrevious(ipos1) : arg1;
        Object item2 = is2seq ? vals2.getPosPrevious(ipos2) : arg2;

        if (! (item1 instanceof KNode) && !( item2 instanceof KNode))
          {
            try
              {
                if (! Compare.atomicCompare(Compare.TRUE_IF_EQU, arg1, arg2, collator))
                  return false;
              }
            catch (Throwable ex)
              {
                return false;
              }
          }
        else if (item1 instanceof KNode && item2 instanceof KNode)
          {
            KNode node1 = (KNode) item1;
            KNode node2 = (KNode) item2;
            if (! deepEqual((NodeTree) node1.sequence, node1.ipos,
                            (NodeTree) node2.sequence, node2.ipos,
                            collator))
              return false;
          }
        else
          return false;

        if (first)
          {
            first = false;
            if (! is1seq)
              ipos1 = 0;
            if (! is2seq)
              ipos2 = 0;
          }
      }
  }
}
