// This file is generated from PrimVector.template. DO NOT EDIT! 
// Copyright (c) 2001, 2002, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;
import gnu.math.UInt;

/** Simple adjustable-length vector of unsigned 32-bit integers (ints). */

public  class U32Vector extends IntVector<UInt>
{
    public U32Vector() {
        data = empty;
    }

    public U32Vector(int size, int value) {
        int[] array = new int[size];
        data = array;
        if (value != 0) {
            while (--size >= 0)
                array[size] = value;
        }
    }

    public U32Vector(int size) {
        this(new int[size]);
    }

    /** Reuses the argument without making a copy. */
    public U32Vector(int[] data) {
        this.data = data;
    }


    /** Makes a copy of (part of) the argument array. */
    public U32Vector(int[] values, int offset, int length) {
        this(length);
        System.arraycopy(values, offset, data, 0, length);
    }

    public final long getLongRaw(int index) {
        return (long) data[index] & 0xffffffffL;
    }

    public final UInt get(int index) {
        return UInt.valueOf(data[effectiveIndex(index)]);
    }

    public final UInt getRaw(int index) {
        return UInt.valueOf(data[index]);
    }

    @Override
    public final void setRaw(int index, UInt value) {
        data[index] = value.intValue();
    }

    @Override
    protected U32Vector newInstance(int newLength) {
        return new U32Vector(newLength < 0 ? data : new int[newLength]);
    }

    public static U32Vector castOrNull(Object obj) {
        if (obj instanceof int[])
            return new U32Vector((int[]) obj);
        if (obj instanceof U32Vector)
            return (U32Vector) obj;
        return null;
    }

    public static U32Vector cast(Object value) {
        U32Vector vec = castOrNull(value);
        if (vec == null) {
            String msg;
            if (value == null)
                msg = "cannot convert null to U32Vector";
            else
                msg = "cannot convert a "+value.getClass().getName()+" to U32Vector";
            throw new ClassCastException(msg);
        }
        return vec;
    }
    public int getElementKind() { return INT_U32_VALUE; }

    public String getTag() { return "u32"; }

    public void consumePosRange(int iposStart, int iposEnd, Consumer out) {
        if (out.ignoring())
            return;
        int i = nextIndex(iposStart);
        int end = nextIndex(iposEnd);
        for (;  i < end;  i++)
            Sequences.writeUInt(getInt(i), out);
    }

    public int compareTo(Object obj) {
        return compareToInt(this, (U32Vector) obj);
    }

}
