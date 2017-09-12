// Copyright (c) 2011  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/**
 * A type variable in a parameterized type. Similar to
 * java.lang.reflect.TypeVariable.
 */

public class TypeVariable extends ObjectType
{
    ClassType rawType;
    java.lang.reflect.TypeVariable rtype;

    public TypeVariable(String name)
    {
        this(name, Type.objectType);
    }

    public TypeVariable(String name, ClassType rawType)
    {
        super(name);
        this.rawType = rawType;
    }

    public static TypeVariable make(java.lang.reflect.TypeVariable rtype)
    {
        TypeVariable tvar = new TypeVariable(rtype.getName());
        java.lang.reflect.Type[] bounds = rtype.getBounds();
        Type bound = null;
        if (bounds.length == 1)
        {
            java.lang.reflect.Type bound0 = bounds[0];
            if (bound0 instanceof Class)
                bound = Type.make(bound0);
            else if (bound0 instanceof java.lang.reflect.ParameterizedType)
                bound = Type.make(((java.lang.reflect.ParameterizedType) bound0).getRawType());
        }
        if (bound != null)
            bound = bound.getRawType();
        if (bound instanceof ClassType)
            tvar.rawType = (ClassType) bound;
        tvar.rtype = rtype;
        return tvar;
    }

    public int compare(Type other)
    {
        return rawType.compare(other);
    }

    public ClassType getRawType()
    {
        return rawType;
    }

    public void emitCoerceFromObject(CodeAttr code)
    {
        rawType.emitCoerceFromObject(code);
    }

    @Override
    public String getSignature()
    {
        return getRawType().getSignature();
    }

    public String getGenericSignature()
    {
        return "T" + getName() + ";";
    }

    public boolean equals(Object other)
    {
        if (!(other instanceof TypeVariable))
            return false;
        TypeVariable tvother = (TypeVariable) other;
        return Type.isSame(rawType, tvother.rawType) && getName().equals(tvother.getName());
    }
}
