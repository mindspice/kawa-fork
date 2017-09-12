// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

import java.io.*;

public class Field extends Location implements AttrContainer, Member
// FUTURE: javax.lang.model.VariableElement
{
    int flags;
    Field next;
    Object constantValue;

    Attribute attributes;

    public final Attribute getAttributes()
    {
        return attributes;
    }

    public final void setAttributes(Attribute attributes)
    {
        this.attributes = attributes;
    }

    /** The class that contains this field. */
    ClassType owner;

    /** If non-null, the interned source-file (unmangled) name of the field. */
    String sourceName;

    /** If non-null, a cached version of the Field for reflection. */
    java.lang.reflect.Field rfield;

    /** Add a new Field to a ClassType. */
    public Field(ClassType ctype)
    {
        if (ctype.last_field == null)
            ctype.fields = this;
        else
            ctype.last_field.next = this;
        ctype.last_field = this;
        ctype.fields_count++;
        owner = ctype;
    }

    public final ClassType getDeclaringClass()
    {
        return owner;
    }

    public final void setStaticFlag(boolean is_static)
    {
        if (is_static)
            flags |= Access.STATIC;
        else
            flags ^= ~Access.STATIC;
    }

    public final boolean getStaticFlag()
    {
        return (flags & Access.STATIC) != 0;
    }

    public final int getFlags()
    {
        return flags;
    }

    public final int getModifiers()
    {
        return flags;
    }

    public final void setModifiers(int modifiers)
    {
        flags = modifiers;
    }

    @Override
    public Type getType()
    {
        synchronized (this)
        {
            Type t = super.getType();
            if (t == null && rfield != null)
            {
                t = Type.make(rfield.getType(), rfield.getGenericType());
                super.setType(t);
            }
            return t;
        }
    }

    public synchronized java.lang.reflect.Field getReflectField()
        throws java.lang.NoSuchFieldException
    {
        if (rfield == null)
            rfield = owner.getReflectClass().getDeclaredField(getName());
        return rfield;
    }

    public <T extends java.lang.annotation.Annotation> T getAnnotation(Class<T> clas)
    {
        T ann = RuntimeAnnotationsAttr.getAnnotation(this, clas);
        if (ann != null)
            return ann;
        return rfield == null ? null : rfield.getAnnotation(clas);
    }

    public void setSourceName(String name)
    {
        sourceName = name;
    }

    public String getSourceName()
    {
        if (sourceName == null)
            sourceName = getName().intern();
        return sourceName;
    }

    /**
     * Find a field with the given name.
     * 
     * @param fields
     *            list of fields to search
     * @param name
     *            (interned source) name of field to look for
     */
    public static Field searchField(Field fields, String name)
    {
        for (; fields != null; fields = fields.next)
        {
            if (fields.getSourceName() == name)
                return fields;
        }
        return null;
    }

    public final Field getNext()
    {
        return next;
    }

    /**
     * Set the ConstantValue attribute for this field.
     * 
     * @param value
     *            the value to use for the ConstantValue attribute of this field
     * @param ctype
     *            the class that contains this field This field's type is used
     *            to determine the kind of constant.
     */
    public final void setConstantValue(Object value, ClassType ctype)
    {
        constantValue = value;
    }

    public boolean hasConstantValueAttr()
    {
        return Attribute.get(this, "ConstantValue") != null;
    }

    public String toString()
    {
        StringBuffer sbuf = new StringBuffer(100);
        sbuf.append("Field:");
        sbuf.append(getDeclaringClass().getName());
        sbuf.append('.');
        sbuf.append(name);
        return sbuf.toString();
    }
}
