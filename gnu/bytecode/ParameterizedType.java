// Copyright (c) 2011  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

public class ParameterizedType extends ObjectType implements Externalizable
{
    ClassType rawType;
    Type[] typeArgumentTypes;
    char[] typeArgumentBounds;

    public ClassType getRawType() {
        return rawType;
    }

    public Class getReflectClass() {
        return rawType.getReflectClass();
    }

    public Type[] getTypeArgumentTypes() {
        return typeArgumentTypes;
    }

    public Type getTypeArgumentType(int index) {
        return typeArgumentTypes[index];
    }

    /** Set a wildcard indicator.
     */
    public void setTypeArgumentBound(int index, char bound) {
	int sz = typeArgumentTypes==null||index>=typeArgumentTypes.length
	    ? index+1
	    : typeArgumentTypes.length;
	char[] bounds = typeArgumentBounds;
	if (bounds == null)
	    typeArgumentBounds = bounds = new char[sz];
	else if (sz > bounds.length) {
	    typeArgumentBounds = new char[sz];
	    System.arraycopy(bounds, 0, typeArgumentBounds, 0, bounds.length);
	    bounds = typeArgumentBounds;
	}
	bounds[index] = bound;
    }

    public char getTypeArgumentBound(int index) {
	if (typeArgumentBounds == null || index >= typeArgumentBounds.length)
	    return '\0';
        return typeArgumentBounds[index];
    }

    public void setTypeArgumentBounds(char[] bounds) {
	this.typeArgumentBounds = bounds;
    }

    @Override
    public String getSignature () { return getRawType().getSignature(); }

    @Override
    public String getGenericSignature() {
        String s = super.getGenericSignature();
        if (s == null) {
            StringBuilder buf = new StringBuilder();
	    buf.append('L');
            buf.append(rawType.getName().replace('.', '/'));
            buf.append('<');
            int n = typeArgumentTypes.length;
	    for (int i = 0;  i < n; i++) {
		char bound = getTypeArgumentBound(i);
		Type tt = getTypeArgumentType(i);
		if (bound == '+' && tt == Type.objectType)
		    buf.append('*');
		else {
		    if (bound != '\0')
			buf.append(bound);
		    buf.append(tt.getMaybeGenericSignature());
		}
	    }
            buf.append(">;");
	    s = buf.toString();
	    super.setGenericSignature(s);
        }
	return s;
    }

    @Override
    public void emitCoerceFromObject (CodeAttr code) {
	getRawType().emitCoerceFromObject(code);
    }

    @Override
    public void emitIsInstance(CodeAttr code) {
        code.emitInstanceof(getRawType());
    }

    @Override
    public String getName() {
        return toString();
    }

    @Override
    public String toString() {
	StringBuilder buf = new StringBuilder();
	buf.append(rawType);
	buf.append('<');
	int n = typeArgumentTypes.length;
	for (int i = 0;  i < n; i++) {
	    if (i > 0)
		buf.append(',');
	    char bound = getTypeArgumentBound(i);
	    if (bound == '+')
		buf.append("? extends ");
	    if (bound == '-')
		buf.append("? super ");
	    buf.append(getTypeArgumentType(i));
	}
	buf.append('>');
	return buf.toString();
    }

    public int compare(Type other) {
	return rawType.compare(other);
    }

    public boolean equals(Object other) {
        if (other == this)
            return true;
        if (! (other instanceof ParameterizedType))
            return false;
        ParameterizedType pother = (ParameterizedType) other;
        if (! Type.isSame(rawType, pother.rawType))
            return false;
        int n = typeArgumentTypes.length;
        Type[] otherArgumentTypes = pother.typeArgumentTypes;
        if (n != otherArgumentTypes.length)
            return false;
        while (--n >= 0) {
            if (! Type.isSame(typeArgumentTypes[n],
                              pother.typeArgumentTypes[n])
                || getTypeArgumentBound(n) != pother.getTypeArgumentBound(n))
                return false;
        }
        return true;
    }

    public ParameterizedType(ClassType rawType, Type... typeArgumentTypes) {
        this.rawType = rawType;
        this.typeArgumentTypes = typeArgumentTypes;
    }
    public static final ParameterizedType make(ClassType rawType,
                                               Type[] typeArgumentTypes,
                                               char[] typeArgumentBounds) {
        ParameterizedType ptype =
            new ParameterizedType(rawType, typeArgumentTypes);
        ptype.typeArgumentBounds = typeArgumentBounds;
        return ptype;
    }

    public void writeExternal(ObjectOutput out) throws IOException
    {
        out.writeObject(rawType);
        out.writeObject(typeArgumentTypes);
        out.writeObject(typeArgumentBounds);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException
    {
        rawType = (ClassType) in.readObject();
        typeArgumentTypes = (Type[]) in.readObject();
        typeArgumentBounds = (char[]) in.readObject();
    }
}
