// Copyright (c) 1997, 1998, 1999, 2001, 2002, 2004, 2005, 2008, 2009, 2011  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

import java.io.*;
import java.util.*;

import org.objectweb.asm.*;

public class ClassType extends ObjectType implements AttrContainer, Externalizable, Member
{
    public static final int JDK_1_1_VERSION = 45 * 0x10000 + 3;
    public static final int JDK_1_2_VERSION = 46 * 0x10000 + 0;
    public static final int JDK_1_3_VERSION = 47 * 0x10000 + 0;
    public static final int JDK_1_4_VERSION = 48 * 0x10000 + 0;
    public static final int JDK_1_5_VERSION = 49 * 0x10000 + 0;
    public static final int JDK_1_6_VERSION = 50 * 0x10000 + 0;
    public static final int JDK_1_7_VERSION = 51 * 0x10000 + 0;
    public static final int JDK_1_8_VERSION = 52 * 0x10000 + 0;
    public static final int JAVA_9_VERSION = 53 * 0x10000 + 0;

    // An old but generally valid default value.
    int classfileFormatVersion = JDK_1_5_VERSION;

    public short getClassfileMajorVersion()
    {
        return (short) (classfileFormatVersion >> 16);
    }

    public short getClassfileMinorVersion()
    {
        return (short) (classfileFormatVersion & 0xFFFF);
    }

    public void setClassfileVersion(int major, int minor)
    {
        classfileFormatVersion = (major & 0xFFFF) * 0x10000 + (minor * 0xFFFF);
    }

    public void setClassfileVersion(int code)
    {
        classfileFormatVersion = code;
    }

    public int getClassfileVersion()
    {
        return classfileFormatVersion;
    }

    public void setClassfileVersionJava5()
    {
        setClassfileVersion(JDK_1_5_VERSION);
    }

    /**
     * Find a ClassType with the given name, or create a new one. Use this for
     * "library classes", where you need the field/method types, but not one
     * where you are about to generate code for.
     * 
     * @param name
     *            the name of the class (e..g. "java.lang.String").
     */
    public static ClassType make(String name)
    {
        return (ClassType) Type.getType(name);
    }

    @Deprecated
    public static ClassType make(String name, ClassType superClass)
    {
        ClassType type = make(name);
        if (type.superClass == null)
            type.setSuper(superClass);
        return type;
    }

    /**
     * The super (base) class of the current class. X.superClass == null means
     * the superClass has not been specified, and defaults to java.lang.Object.
     */
    private ClassType superClass;
    private Type genericSuperClass;

    ClassType[] interfaces;
    Type[] genericInterfaces;
    private ClassType[] allInterfaces;
    int access_flags;

    Attribute attributes;

    public final Attribute getAttributes()
    {
        return attributes;
    }

    public final void setAttributes(Attribute attributes)
    {
        this.attributes = attributes;
    }

    public static final ClassType[] noClasses = {};

    /** Return the modifiers (access flags) for this class. */
    public final synchronized int getModifiers()
    {
        if (access_flags == 0 && (flags & EXISTING_CLASS) != 0 && getReflectClass() != null)
            access_flags = reflectClass.getModifiers();
        return access_flags;
    }

    public final boolean getStaticFlag()
    {
        return (getModifiers() & Access.STATIC) != 0;
    }

    /** Set the modifiers (access flags) for this class. */
    public final void setModifiers(int flags)
    {
        access_flags = flags;
    }

    public final void addModifiers(int flags)
    {
        access_flags |= flags;
    }

    public synchronized String getSimpleName()
    {
        /* #ifdef JAVA5 */
        if ((flags & EXISTING_CLASS) != 0 && getReflectClass() != null)
        {
            try
            {
                return reflectClass.getSimpleName();
            } catch (Exception ex)
            {
                /* ... fall thorugh ... */ }
        }
        /* #endif */
        String name = getName();
        if (enclosingMember instanceof ClassType)
        {
            String enclosingName = ((ClassType) enclosingMember).getName();
            int enclosingLength;
            if (enclosingName != null && name.startsWith(enclosingName)
                && name.length() > (enclosingLength = enclosingName.length()) + 1
                && name.charAt(enclosingLength) == '$')
                return name.substring(enclosingLength + 1);
        }
        int dot = name.lastIndexOf('.');
        if (dot > 0)
            name = name.substring(dot + 1);
        return name;
    }

    public void addMemberClass(ClassType member)
    {
        ClassType prev = null;
        ClassType entry = firstInnerClass;
        while (entry != null)
        {
            if (entry == member)
                return;
            prev = entry;
            entry = entry.nextInnerClass;
        }
        if (prev == null)
            firstInnerClass = member;
        else
            prev.nextInnerClass = member;
    }

    public ClassType getDeclaredClass(String simpleName)
    {
        addMemberClasses();
        for (ClassType member = firstInnerClass; member != null; member = member.nextInnerClass)
        {
            if (simpleName.equals(member.getSimpleName()))
                return member;
        }
        return null;
    }

    ClassType firstInnerClass;
    ClassType nextInnerClass;

    Member enclosingMember;

    public ClassType getDeclaringClass()
    {
        addEnclosingMember();
        if (enclosingMember instanceof ClassType)
            return (ClassType) enclosingMember;
        else
            return null;
    }

    public Member getEnclosingMember()
    {
        addEnclosingMember();
        return enclosingMember;
    }

    public void setEnclosingMember(Member member)
    {
        enclosingMember = member;
    }

    synchronized void addEnclosingMember()
    {
        if ((flags & (ADD_ENCLOSING_DONE | EXISTING_CLASS)) != EXISTING_CLASS)
            return;
        Class clas = getReflectClass();
        flags |= ADD_ENCLOSING_DONE;

        Class dclas;
        /* #ifdef JAVA5 */
        dclas = clas.getEnclosingClass();
        if (dclas == null)
            return;
        if (!clas.isMemberClass())
        {
            java.lang.reflect.Method rmeth = clas.getEnclosingMethod();
            if (rmeth != null)
            {
                enclosingMember = addMethod(rmeth);
                return;
            }
            java.lang.reflect.Constructor rcons = clas.getEnclosingConstructor();
            if (rcons != null)
            {
                enclosingMember = addMethod(rcons);
                return;
            }
        }
        enclosingMember = (ClassType) Type.make(dclas);
        /* #else */
        // dclas = clas.getDeclaringClass();
        // if (dclas != null)
        // enclosingMember = (ClassType) Type.make(dclas);
        /* #endif */
    }

    public synchronized void addMemberClasses()
    {
        if ((flags & (ADD_MEMBERCLASSES_DONE | EXISTING_CLASS)) != EXISTING_CLASS)
            return;
        Class clas = getReflectClass();
        flags |= ADD_MEMBERCLASSES_DONE;
        Class[] memberClasses = clas.getClasses();
        int numMembers = memberClasses.length;
        if (numMembers > 0)
        {
            for (int i = 0; i < numMembers; i++)
            {
                ClassType member = (ClassType) Type.make(memberClasses[i]);
                addMemberClass(member);
            }
        }
    }

    public final boolean hasOuterLink()
    {
        getFields();
        return (flags & HAS_OUTER_LINK) != 0;
    }

    public ClassType getOuterLinkType()
    {
        return !hasOuterLink() ? null : (ClassType) getDeclaredField("this$0").getType();
    }

    /**
     * Note that this class needs an other link ("this$0") field. This is only
     * allowed if !isExisting(). Adjust any existing {@code "<init>"} methods to
     * take the extra implicit parameter.
     * 
     * @param outer
     *            the outer class
     */
    public final Field setOuterLink(ClassType outer)
    {
        if ((flags & EXISTING_CLASS) != 0)
            throw new Error("setOuterLink called for existing class " + getName());
        Field field = getDeclaredField("this$0");
        if (field == null)
        {
            field = addField("this$0", outer);
            flags |= HAS_OUTER_LINK;
            for (Method meth = methods; meth != null; meth = meth.getNext())
            {
                if ("<init>".equals(meth.getName()))
                {
                    if (meth.code != null)
                        throw new Error("setOuterLink called when " + meth + " has code");
                    Type[] arg_types = meth.arg_types;
                    Type[] new_types = new Type[arg_types.length + 1];
                    System.arraycopy(arg_types, 0, new_types, 1, arg_types.length);
                    new_types[0] = outer;
                    meth.arg_types = new_types;
                    meth.signature = null;
                }
            }
        } else if (!outer.equals(field.getType()))
            throw new Error("inconsistent setOuterLink call for " + getName());
        return field;
    }

    /**
     * Check if a member is accessible from this class.
     * 
     * @param member
     *            the member (Field, Method) we're trting to access.
     * @param receiver
     *            the type of the receiver object, if applicable.
     * @return true if the specified component can be accessed from this class.
     */
    public boolean isAccessible(Member member, ObjectType receiver)
    {
        if (member.getStaticFlag())
            receiver = null;
        return isAccessible(member.getDeclaringClass(), receiver, member.getModifiers());
    }

    /**
     * Check if a component is accessible from this class.
     * 
     * @param declaring
     *            the class containing the component (a field, method, or inner
     *            class)
     * @param receiver
     *            the type of the receiver object, if applicable.
     * @param modifiers
     *            the access flags of the component
     * @return true if the specified component can be accessed from this class.
     */
    public boolean isAccessible(ClassType declaring, ObjectType receiver, int modifiers)
    {
        int cmods = declaring.getModifiers();
        // Fast, hopefully-common case.
        if ((modifiers & Access.PUBLIC) != 0 && (cmods & Access.PUBLIC) != 0)
            return true;
        String callerName = getName();
        String className = declaring.getName();
        if (callerName.equals(className))
            return true;
        if ((modifiers & Access.PRIVATE) != 0)
            return false;
        int dot = callerName.lastIndexOf('.');
        String callerPackage = dot >= 0 ? callerName.substring(0, dot) : "";
        dot = className.lastIndexOf('.');
        String classPackage = dot >= 0 ? className.substring(0, dot) : "";
        if (callerPackage.equals(classPackage))
            return true;
        if ((cmods & Access.PUBLIC) == 0)
            return false;
        if ((modifiers & Access.PROTECTED) != 0 && this.isSubclass(declaring)
            && (!(receiver instanceof ClassType) || ((ClassType) receiver).isSubclass(this)))
            return true;
        return false;
    }

    /**
     * Sets the name of the class being defined in this classfile.
     * 
     * @param name
     *            the name to give to the class
     */
    public void setName(String name)
    {
        Type.mapNameToClassType.remove(this_name);
        this_name = name;
        setSignature(nameToSignature(name));
        Type.mapNameToClassType.put(this_name, this);
    }

    public static String nameToSignature(String name)
    {
        return "L" + name.replace('.', '/') + ";";
    }

    SourceDebugExtAttr sourceDbgExt;
    String fileName;

    /**
     * Create a <code>SourceDebugExtAttr</code>, if needed, and set the
     * "stratum". The stratum is typically a programming language such as "JSP",
     * "Scheme", or "Java" (the default).
     */
    public void setStratum(String stratum)
    {
        if (sourceDbgExt == null)
            sourceDbgExt = new SourceDebugExtAttr(this);
        sourceDbgExt.addStratum(stratum);
    }

    /** Set the name of the SourceFile associated with this class. */
    public void setSourceFile(String name)
    {
        if (sourceDbgExt != null)
        {
            sourceDbgExt.addFile(name);
            if (sourceDbgExt.fileCount > 1)
                return;
        }

        name = SourceFileAttr.fixSourceFile(name);
        int slash = name.lastIndexOf('/');
        if (slash >= 0)
            name = name.substring(slash + 1);
        fileName = name;
    }

    TypeVariable[] typeParameters;

    public TypeVariable[] getTypeParameters()
    {
        TypeVariable[] params = typeParameters;
        if (params == null && (flags & EXISTING_CLASS) != 0 && getReflectClass() != null)
        {
            java.lang.reflect.TypeVariable[] rparams = reflectClass.getTypeParameters();
            int nparams = rparams.length;
            params = new TypeVariable[nparams];
            for (int i = 0; i < nparams; i++)
            {
                params[i] = TypeVariable.make(rparams[i]);
            }
            typeParameters = params;
        }
        return params;
    }

    public void setTypeParameters(TypeVariable[] typeParameters)
    {
        this.typeParameters = typeParameters;
    }

    /**
     * Set the superclass of the is class.
     * 
     * @param name
     *            name of super class, or null if this is "Object".
     */
    public void setSuper(String name)
    {
        setSuper(name == null ? Type.pointer_type : ClassType.make(name));
    }

    public void setSuper(ClassType superClass)
    {
        this.superClass = superClass;
        this.genericSuperClass = superClass;
    }

    public void setSuper(Type superClass)
    {
        this.superClass = (ClassType) superClass.getRawType();
        this.genericSuperClass = superClass;
    }

    public synchronized ClassType getSuperclass()
    {
        if (superClass == null && !isInterface() && !("java.lang.Object".equals(getName()))
            && (flags & EXISTING_CLASS) != 0 && getReflectClass() != null)
        {
            superClass = (ClassType) make(reflectClass.getSuperclass());
        }
        return superClass;
    }

    public synchronized Type getGenericSuperclass()
    {
        if (genericSuperClass == null && !isInterface() && !("java.lang.Object".equals(getName()))
            && (flags & EXISTING_CLASS) != 0 && getReflectClass() != null)
        {
            genericSuperClass = make(reflectClass.getGenericSuperclass());
        }
        return genericSuperClass;
    }

    public String getPackageName()
    {
        String name = getName();
        int index = name.lastIndexOf('.');
        return index < 0 ? "" : name.substring(0, index);
    }

    /**
     * @return the interfaces this class is declared to implement (not those
     *         inherited from its superclass/superinterfaces).
     */
    public synchronized ClassType[] getInterfaces()
    {
        if (interfaces == null && (flags & EXISTING_CLASS) != 0 && getReflectClass() != null)
        {
            Class[] reflectInterfaces = reflectClass.getInterfaces();
            int numInterfaces = reflectInterfaces.length;
            interfaces = numInterfaces == 0 ? noClasses : new ClassType[numInterfaces];

            for (int i = 0; i < numInterfaces; i++)
                interfaces[i] = (ClassType) Type.make(reflectInterfaces[i]);
        }
        return interfaces;
    }

    public synchronized Type[] getGenericInterfaces()
    {
        if (genericInterfaces == null && (flags & EXISTING_CLASS) != 0 && getReflectClass() != null)
        {
            java.lang.reflect.Type[] reflectInterfaces = reflectClass.getGenericInterfaces();
            int numInterfaces = reflectInterfaces.length;
            genericInterfaces = numInterfaces == 0 ? Type.typeArray0 : new Type[numInterfaces];

            for (int i = 0; i < numInterfaces; i++)
                genericInterfaces[i] = Type.make(reflectInterfaces[i]);
        }
        return genericInterfaces;
    }

    /**
     * Get all the interfaces this class implements. Includes those inherited
     * from its superclass/superinterfaces.
     */
    public synchronized ClassType[] getAllInterfaces()
    {
        if (allInterfaces == null)
        {
            LinkedHashMap<String, ClassType> map = new LinkedHashMap<String, ClassType>();
            for (ClassType t = this; t != null; t = t.getSuperclass())
            {
                if (!t.addInterfaces(map))
                    return null;
            }
            ClassType[] allInts = new ClassType[map.size()];
            int i = 0;
            for (ClassType intf : map.values())
            {
                allInts[i++] = intf;
            }
            allInterfaces = allInts;
        }
        return allInterfaces;
    }

    private boolean addInterfaces(LinkedHashMap<String, ClassType> map)
    {
        ClassType[] intfs = getInterfaces();
        if (intfs == null)
            return false;
        for (ClassType intf : intfs)
        {
            if (map.put(intf.getName(), intf) == null && !intf.addInterfaces(map))
                return false;
        }
        return true;
    }

    public void setInterfaces(ClassType[] interfaces)
    {
        this.interfaces = interfaces;
        this.genericInterfaces = interfaces;
    }

    public void setInterfaces(Type[] interfaces)
    {
        this.interfaces = new ClassType[interfaces.length];
        for (int i = 0; i < interfaces.length; i++)
            this.interfaces[i] = (ClassType) interfaces[i].getRawType();
        this.genericInterfaces = interfaces;
    }

    /** Add an interface to the list of implemented interfaces. */
    public void addInterface(ClassType newInterface)
    {
        addInterface((Type) newInterface);
    }

    /** Add an interface to the list of implemented interfaces. */
    public void addInterface(Type newInterface)
    {
        int oldCount;
        if (interfaces == null || interfaces.length == 0)
        {
            oldCount = 0;
            interfaces = new ClassType[1];
            genericInterfaces = new Type[1];
        } else
        {
            oldCount = interfaces.length;
            for (int i = oldCount; --i >= 0;)
                if (interfaces[i] == newInterface)
                    return;
            ClassType[] newInterfaces = new ClassType[oldCount + 1];
            System.arraycopy(interfaces, 0, newInterfaces, 0, oldCount);
            interfaces = newInterfaces;
            Type[] newGenericInterfaces = new Type[oldCount + 1];
            System.arraycopy(genericInterfaces, 0, newGenericInterfaces, 0, oldCount);
            genericInterfaces = newGenericInterfaces;
        }
        interfaces[oldCount] = (ClassType) newInterface.getRawType();
        genericInterfaces[oldCount] = newInterface;
    }

    public final boolean isInterface()
    {
        return (getModifiers() & Access.INTERFACE) != 0;
    }

    public final void setInterface(boolean val)
    {
        if (val)
            access_flags |= Access.INTERFACE | Access.ABSTRACT;
        else
            access_flags &= ~Access.INTERFACE | Access.ABSTRACT;
    }

    public final boolean isFinal()
    {
        return (getModifiers() & Access.FINAL) != 0;
    }

    public final boolean isAnnotation()
    {
        return (getModifiers() & Access.ANNOTATION) != 0;
    }

    ClassVisitor cw;
    ClassWriter mcw;

    public ClassType()
    {
    }

    public ClassType(String class_name)
    {
        this();
        setName(class_name);
    }

    boolean cwVisited = false;

    public void cwVisit()
    {
        if (!isExisting() && !cwVisited)
        {
            cwVisited = true;
            cw = mcw = new MyClassWriter(ClassWriter.COMPUTE_FRAMES);
            // cw = new org.objectweb.asm.util.CheckClassAdapter(mcw);
            String[] s_intfs = null;
            if (interfaces != null)
            {
                s_intfs = new String[interfaces.length];
                for (int i = 0; i < interfaces.length; i++)
                {
                    s_intfs[i] = interfaces[i].getInternalName();
                }
            }
            String classSignature = null;
            if (genericSuperClass != superClass
                || (typeParameters != null && typeParameters.length != 0)
                || (interfaces != null && !Arrays.equals(genericInterfaces, interfaces)))
                classSignature = getClassSignature();
            cw.visit(getClassfileMajorVersion(), getModifiers(), getInternalName(), classSignature,
                getSuperclass().getInternalName(), s_intfs);
        }
    }

    Field fields;
    int fields_count;
    Field last_field;

    /** Get the fields of this class. */
    public final synchronized Field getFields()
    {
        if ((flags & (ADD_FIELDS_DONE | EXISTING_CLASS)) == EXISTING_CLASS)
            addFields();
        return fields;
    }

    public final int getFieldCount()
    {
        return fields_count;
    }

    /**
     * Find a field with the given name declared in this class.
     * 
     * @return the matching field, or null if there is no such field.
     */
    public Field getDeclaredField(String name)
    {
        for (Field field = getFields(); field != null; field = field.next)
        {
            if (name.equals(field.name))
                return field;
        }
        return null;
    }

    /**
     * Find a field with the given name declared in this class or its ancestors.
     * 
     * @param name
     *            the name of the field.
     * @param mask
     *            of match a field whose modifiers has one of these bits set.
     *            Howeve, if mask is -1, ignore the access flags.
     * @return the matching field, or null if there is no such field.
     */
    public synchronized Field getField(String name, int mask)
    {
        ClassType cl = this;
        for (;;)
        {
            Field field = cl.getDeclaredField(name);
            if (field != null && (mask == -1 || (field.getModifiers() & mask) != 0))
                return field;
            ClassType[] interfaces = cl.getInterfaces();
            if (interfaces != null)
            {
                for (int i = 0; i < interfaces.length; i++)
                {
                    field = interfaces[i].getField(name, mask);
                    if (field != null)
                        return field;
                }
            }
            cl = cl.getSuperclass();
            if (cl == null)
                return null;
        }
    }

    /**
     * Find a field with the given name declared in this class or its ancestors.
     * 
     * @return the matching field, or null if there is no such field.
     */
    public Field getField(String name)
    {
        return getField(name, Access.PUBLIC);
    }

    /**
     * Add a new field to this class.
     */
    public Field addField()
    {
        return new Field(this);
    }

    /**
     * Add a new field to this class, and name the field.
     * 
     * @param name
     *            the name of the new field
     */
    public Field addField(String name)
    {
        Field field = new Field(this);
        field.setName(name);
        return field;
    }

    public final Field addField(String name, Type type)
    {
        Field field = new Field(this);
        field.setName(name);
        field.setType(type);
        return field;
    }

    public final Field addField(String name, Type type, int flags)
    {
        Field field = addField(name, type);
        field.flags = flags;
        return field;
    }

    /**
     * Use reflection to add all the declared fields of this class. Does not add
     * private fields. Does not check for duplicate (already-known) fields. Is
     * not thread-safe if another thread may access this ClassType.
     */
    public synchronized void addFields()
    {
        Class clas = getReflectClass();
        java.lang.reflect.Field[] fields;
        try
        {
            fields = clas.getDeclaredFields();
        } catch (SecurityException ex)
        {
            fields = clas.getFields();
        }
        int count = fields.length;
        for (int i = 0; i < count; i++)
        {
            java.lang.reflect.Field field = fields[i];
            if ("this$0".equals(field.getName()))
                flags |= HAS_OUTER_LINK;
            int mods = field.getModifiers();
            if ((mods & Access.PRIVATE) == 0)
            {
                Field fld = addField(field.getName(), null, // lazy type lookup
                    mods);
                fld.rfield = field;
            }
        }
        flags |= ADD_FIELDS_DONE;
    }

    public void removeField(Field field, Field prev)
    {
        if (field != (prev == null ? fields : prev.next))
            new Error();
        if (prev == null)
            fields = field.next;
        else
            prev.next = field.next;
        if (last_field == field)
            last_field = null;
        fields_count--;
    }

    Method methods;
    int methods_count;
    Method last_method;
    public Method constructor;

    /** Get the methods of this class. */
    public final Method getMethods()
    {
        return methods;
    }

    public final int getMethodCount()
    {
        return methods_count;
    }

    Method addMethod()
    {
        cwVisit();
        return new Method(this, 0);
    }

    public Method addMethod(String name)
    {
        cwVisit();
        return addMethod(name, 0);
    }

    public Method addMethod(String name, int flags)
    {
        cwVisit();
        Method method = new Method(this, flags);
        method.setName(name);
        return method;
    }

    // deprecated:
    public Method addMethod(String name, Type[] arg_types, Type return_type, int flags)
    {
        return addMethod(name, flags, arg_types, return_type);
    }

    /**
     * Add a method to this ClassType. If an existing method matches, return
     * that. Otherwise, create a new one. In contrast, the other addMethod
     * methods always create new Methods.
     */
    public synchronized Method addMethod(String name, int flags, Type[] generic_arg_types,
        Type return_type)
    {
        cwVisit();
        int n = generic_arg_types.length;
        Type[] arg_types = new Type[n];
        for (int i = 0; i < n; i++)
        {
            Type g = generic_arg_types[i];
            arg_types[i] = g == null ? null : g.getRawType();
        }
        Method method = getDeclaredMethod(name, arg_types);
        if (method != null && return_type.equals(method.getReturnType())
            && (flags & method.access_flags) == flags)
            return method;
        method = addMethod(name, flags);
        method.arg_types = arg_types;
        method.generic_arg_types = generic_arg_types;
        method.return_type = return_type;
        return method;
    }

    public Method addMethod(java.lang.reflect.Method method)
    {
        int modifiers = method.getModifiers();
        Class[] paramTypes = method.getParameterTypes();
        java.lang.reflect.Type[] genericParamTypes = method.getGenericParameterTypes();
        int j = paramTypes.length;
        Type[] args = new Type[j];
        if (genericParamTypes.length == 0)
        {
            while (--j >= 0)
                args[j] = Type.make(paramTypes[j]);
        } else
        {
            while (--j >= 0)
                args[j] = Type.make(paramTypes[j], genericParamTypes[j]);
        }
        Type rtype = Type.make(method.getReturnType(), method.getGenericReturnType());
        Method meth = addMethod(method.getName(), modifiers, args, rtype);
        meth.rmethod = method;
        return meth;
    }

    public Method addMethod(java.lang.reflect.Constructor method)
    {
        Class[] paramTypes = method.getParameterTypes();
        java.lang.reflect.Type[] genericParamTypes = method.getGenericParameterTypes();
        int modifiers = method.getModifiers();
        int j = paramTypes.length;
        Type[] args = new Type[j];
        if (genericParamTypes.length == 0)
        {
            while (--j >= 0)
                args[j] = Type.make(paramTypes[j]);
        } else
        {
            while (--j >= 0)
                args[j] = Type.make(paramTypes[j], genericParamTypes[j]);
        }
        Method meth = addMethod("<init>", modifiers, args, Type.voidType);
        meth.rmethod = method;
        return meth;
    }

    public Method addMethod(String name, String signature, int flags)
    {
        cwVisit();
        Method meth = addMethod(name, flags);
        meth.setSignature(signature);
        return meth;
    }

    /**
     * Add a method to this ClassType. If an existing method matches, return
     * that. Otherwise, create a new one.
     */
    public Method getMethod(java.lang.reflect.Method method)
    {
        String name = method.getName();
        Class[] parameterClasses = method.getParameterTypes();
        Type[] parameterTypes = new Type[parameterClasses.length];
        for (int i = parameterClasses.length; --i >= 0;)
            parameterTypes[i] = Type.make(parameterClasses[i]);
        return addMethod(name, method.getModifiers(), parameterTypes,
            Type.make(method.getReturnType()));
    }

    public final synchronized Method getDeclaredMethods()
    {
        if ((flags & (ADD_METHODS_DONE | EXISTING_CLASS)) == EXISTING_CLASS)
            addMethods(getReflectClass());
        return methods;
    }

    /**
     * Count methods matching a given filter.
     * 
     * @param filter
     *            to select methods to return
     * @param searchSupers
     *            0 if only current class should be searched, 1 if superclasses
     *            should also be searched, 2 if super-interfaces should also be
     *            searched
     * @return number of methods that match
     */
    public final int countMethods(Filter filter, int searchSupers)
    {
        Vector vec = new Vector();
        getMethods(filter, searchSupers, vec);
        return vec.size();
    }

    public Method[] getMethods(Filter filter, boolean searchSupers)
    {
        return getMethods(filter, searchSupers ? 1 : 0);
    }

    /**
     * Get methods matching a given filter.
     * 
     * @param filter
     *            to select methods to return
     * @param searchSupers
     *            0 if only current class should be searched, 1 if superclasses
     *            should also be searched, 2 if super-interfaces should also be
     *            searched
     * @return a fresh array containing the methods satisfying the filter
     */
    public Method[] getMethods(Filter filter, int searchSupers)
    {
        Vector<Method> vec = new Vector();
        getMethods(filter, searchSupers, vec);
        int count = vec.size();
        Method[] result = new Method[count];
        for (int i = 0; i < count; i++)
            result[i] = vec.elementAt(i);
        return result;
    }

    /**
     * Helper to get methods satisfying a filtering predicate.
     * 
     * @param filter
     *            to select methods to return
     * @param searchSupers
     *            0 if only current class should be searched, 1 if superclasses
     *            should also be searched, 2 if super-interfaces should also be
     *            searched
     * @param result
     *            array to place selected methods in
     * @param offset
     *            start of where in result to place result
     * @return number of methods placed in result array
     */
    @Deprecated
    public int getMethods(Filter filter, int searchSupers, Method[] result, int offset)
    {
        Vector<Method> vec = new Vector<Method>();
        getMethods(filter, searchSupers, vec);
        int count = vec.size();
        for (int i = 0; i < count; i++)
            result[offset + i] = vec.elementAt(i);
        return count;
    }

    /**
     * Helper to get methods satisfying a filtering predicate.
     * 
     * @param filter
     *            to select methods to return
     * @param searchSupers
     *            0 if only current class should be searched, 1 if superclasses
     *            should also be searched, 2 if super-interfaces should also be
     *            searched
     * @param result
     *            List to add selected methods in
     * @return number of methods placed in result list
     */
    public int getMethods(Filter filter, int searchSupers,
        /* #ifdef JAVA5 */
        List<Method>
        /* #else */
        // Vector
        /* #endif */
        result)
    {
        int count = 0;
        String inheritingPackage = null;
        for (ClassType ctype = this; ctype != null; ctype = ctype.isInterface() ? Type.objectType
            : ctype.getSuperclass())
        {
            String curPackage = ctype.getPackageName();
            for (Method meth = ctype.getDeclaredMethods(); meth != null; meth = meth.getNext())
            {
                if (ctype != this)
                {
                    int mmods = meth.getModifiers();
                    if ((mmods & Access.PRIVATE) != 0)
                        continue;
                    if ((mmods & (Access.PUBLIC | Access.PROTECTED)) == 0
                        && !curPackage.equals(inheritingPackage))
                        continue;
                }
                if (filter.select(meth))
                {
                    if (result != null)
                    {
                        result.add(meth);
                    }
                    count++;
                }
            }

            inheritingPackage = curPackage;

            if (searchSupers == 0)
                break;
        }

        if (searchSupers > 1)
        {
            ClassType[] interfaces = getAllInterfaces();
            if (interfaces != null)
            {
                for (int i = 0; i < interfaces.length; i++)
                    count += interfaces[i].getMethods(filter, 0, result);
            }
        }
        return count;
    }

    static class AbstractMethodFilter implements gnu.bytecode.Filter
    {
        public static final AbstractMethodFilter instance = new AbstractMethodFilter();

        public boolean select(Object value)
        {
            gnu.bytecode.Method method = (gnu.bytecode.Method) value;
            return method.isAbstract();
        }
    }

    public Method[] getAbstractMethods()
    {
        return getMethods(AbstractMethodFilter.instance, 2);
    }

    /**
     * Look for a matching method.
     * 
     * @param name
     *            method name
     * @param arg_types
     *            parameter types that must match. Can also be null, to match
     *            any parameter type list. Otherwise, an element of arg_types
     *            must be the same type (equals), though a null element of
     *            arg_types is a wildcard that matches any type.
     */
    public Method getDeclaredMethod(String name, Type[] arg_types)
    {
        int needOuterLinkArg = "<init>".equals(name) && hasOuterLink() ? 1 : 0;
        Method found = null;
        for (Method method = getDeclaredMethods(); method != null; method = method.next)
        {
            if (!name.equals(method.getName()))
                continue;
            Type[] method_args = method.getParameterTypes();
            boolean synthetic = (method.getModifiers() & (Access.SYNTHETIC | Access.BRIDGE)) != 0;
            if (arg_types == null || (arg_types == method_args && needOuterLinkArg == 0))
            {
                if (!synthetic)
                    return method;
                found = method;
            }
            int i = arg_types.length;
            if (i != method_args.length - needOuterLinkArg)
                continue;
            while (--i >= 0)
            {
                Type meth_type = method_args[i + needOuterLinkArg];
                Type need_type = arg_types[i];
                if (meth_type == need_type || need_type == null)
                    continue;
                String meth_sig = meth_type.getSignature();
                String need_sig = need_type.getSignature();
                if (!meth_sig.equals(need_sig))
                    break;
            }
            if (i < 0)
            {
                if (!synthetic)
                    return method;
                found = method;
            }
        }
        return found;
    }

    synchronized Method getDeclaredMethod(String name, boolean mustBeStatic, int argCount)
    {
        Method result = null;
        int needOuterLinkArg = "<init>".equals(name) && hasOuterLink() ? 1 : 0;
        for (Method method = getDeclaredMethods(); method != null; method = method.next)
        {
            if (mustBeStatic && !method.getStaticFlag())
                continue;
            if (name.equals(method.getName())
                && argCount + needOuterLinkArg == method.getParameterTypes().length)
            {
                if (result != null)
                    throw new Error("ambiguous call to getDeclaredMethod(\"" + name + "\", "
                        + argCount + ")\n - " + result + "\n - " + method);
                result = method;
            }
        }
        return result;
    }

    /** Get a method with matching name and number of arguments. */
    public Method getDeclaredMethod(String name, int argCount)
    {
        return getDeclaredMethod(name, false, argCount);
    }

    /** Get a static method with matching name and number of arguments. */
    public Method getDeclaredStaticMethod(String name, int argCount)
    {
        return getDeclaredMethod(name, true, argCount);
    }

    /**
     * Looks for a method matching the name and types. Note looks for an exact
     * match, unless a type is null, not necessarily the best match.
     */
    public synchronized Method getMethod(String name, Type[] arg_types)
    {
        for (ClassType cl = this; cl != null; cl = cl.getSuperclass())
        {
            Method m = cl.getDeclaredMethod(name, arg_types);
            if (m != null)
                return m;
        }
        ClassType[] interfaces = getAllInterfaces();
        if (interfaces != null)
        {
            for (int i = 0; i < interfaces.length; i++)
            {
                Method m = interfaces[i].getDeclaredMethod(name, arg_types);
                if (m != null)
                    return m;
            }
        }
        return null;
    }

    public Method getDefaultConstructor()
    {
        return getDeclaredMethod("<init>", Type.typeArray0);
    }

    /**
     * Use reflection to add all the declared methods of this class. Does not
     * add constructors nor private or package-private methods. Does not check
     * for duplicate (already-known) methods.
     * 
     * @param clas
     *            should be the same as getReflectClass().
     */
    public synchronized void addMethods(Class clas)
    {
        // Set this flag BEFORE the actual addition.
        // This prevents this method to be called indirectly for the same class
        // while it is executed, which would result in methods being listed
        // twice in this class.
        flags |= ADD_METHODS_DONE;

        java.lang.reflect.Method[] methods;
        try
        {
            methods = clas.getDeclaredMethods();
        } catch (SecurityException ex)
        {
            methods = clas.getMethods();
        }
        int count = methods.length;
        for (int i = 0; i < count; i++)
        {
            java.lang.reflect.Method method = methods[i];
            if (!method.getDeclaringClass().equals(clas))
                continue;
            addMethod(method);
        }

        java.lang.reflect.Constructor[] cmethods;
        try
        {
            cmethods = clas.getDeclaredConstructors();
        } catch (SecurityException ex)
        {
            cmethods = clas.getConstructors();
        }
        count = cmethods.length;
        for (int i = 0; i < count; i++)
        {
            java.lang.reflect.Constructor method = cmethods[i];
            if (!method.getDeclaringClass().equals(clas))
                continue;
            addMethod(method);
        }
    }

    public Method[] getMatchingMethods(String name, Type[] paramTypes, int flags)
    {
        int nMatches = 0;
        java.util.Vector matches = new java.util.Vector(10);
        for (Method method = methods; method != null; method = method.getNext())
        {
            if (!name.equals(method.getName()))
                continue;
            if ((flags & Access.STATIC) != (method.access_flags & Access.STATIC))
                continue;
            if ((flags & Access.PUBLIC) > (method.access_flags & Access.PUBLIC))
                continue;
            Type[] mtypes = method.arg_types;
            if (mtypes.length != paramTypes.length)
                continue;
            nMatches++;
            matches.addElement(method);
        }
        Method[] result = new Method[nMatches];
        matches.copyInto(result);
        return result;
    }

    public <T extends java.lang.annotation.Annotation> T getAnnotation(Class<T> clas)
    {
        T ann = RuntimeAnnotationsAttr.getAnnotation(this, clas);
        if (ann != null)
            return ann;
        if ((flags & EXISTING_CLASS) != 0 && getReflectClass() != null)
        {
            Class<?> c = getReflectClass();
            return c.getAnnotation(clas);
        }
        return null;
    }

    public String getClassSignature()
    {
        StringBuilder buf = new StringBuilder();
        if (typeParameters != null && typeParameters.length != 0)
        {
            int n = typeParameters.length;
            buf.append('<');
            for (int i = 0; i < n; i++)
            {
                TypeVariable tt = typeParameters[i];
                buf.append(tt.getName());
                buf.append(":");
                buf.append(tt.getRawType().getMaybeGenericSignature());
            }
            buf.append(">");
        }
        buf.append(getGenericSuperclass().getMaybeGenericSignature());
        if (getInterfaces() != null)
            for (Type c : getGenericInterfaces())
                buf.append(c.getMaybeGenericSignature());
        return buf.toString();
    }

    /**
     * Do various fixups after generating code but before we can write it out.
     * This includes assigning constant pool indexes where needed, finalizing
     * labels, etc.
     */
    public void doFixups()
    {
        if (superClass == this)
            setSuper((ClassType) null);

        flags |= ADD_FIELDS_DONE | ADD_METHODS_DONE | ADD_ENCLOSING_DONE;
    }

    public void writeToStream(OutputStream stream) throws java.io.IOException
    {
        stream.write(writeToArray());
    }

    public void writeToFile(String filename) throws java.io.IOException
    {
        OutputStream stream = new BufferedOutputStream(new FileOutputStream(filename));
        writeToStream(stream);
        stream.close();
    }

    public void writeToFile() throws java.io.IOException
    {
        writeToFile(this_name.replace('.', File.separatorChar) + ".class");
    }

    static void visitAnnotation(AnnotationVisitor av, String name, AnnotationEntry.Value value)
    {
        // TODO
        if (value.kind == 'e')
        {
            Enum e = (Enum) value.getValue();
            av.visitEnum(name, "L" + e.getDeclaringClass().getName().replace('.', '/') + ";",
                e.name());
        } else if (value.kind == 'c')
        {
            String sig = value.value instanceof String ? (String) value.value
                : ((ClassType) value.value).getInternalName();
            av.visit(name, org.objectweb.asm.Type.getObjectType(sig));
        } else if (value.kind == '[')
        {
            AnnotationVisitor av2 = av.visitArray(name);
            List<AnnotationEntry.Value> lvalue = (List<AnnotationEntry.Value>) value.value;
            for (AnnotationEntry.Value value2 : lvalue)
                visitAnnotation(av2, null, value2);
            av2.visitEnd();
        } else
        {
            av.visit(name, value.getValue());
        }
    }

    static void visitAnnotation(AnnotationVisitor av, AnnotationEntry ae)
    {
        for (Map.Entry<String, AnnotationEntry.Value> entry : ae.elementsValue.entrySet())
        {
            String name = entry.getKey();
            AnnotationEntry.Value value = entry.getValue();
            visitAnnotation(av, name, value);
        }
        av.visitEnd();
    }

    static List<AnnotationEntry> getAnnotationEntries(AttrContainer ac, boolean visible)
    {
        RuntimeAnnotationsAttr attr = visible
            ? RuntimeAnnotationsAttr.getRuntimeVisibleAnnotations(ac)
            : RuntimeAnnotationsAttr.getRuntimeInvisibleAnnotations(ac);
        if (attr.entries == null)
            return Collections.emptyList();
        else
            return Arrays.asList(attr.entries).subList(0, attr.numEntries);
    }

    boolean cwEnded = false;

    public synchronized byte[] writeToArray()
    {
        if (!cwEnded)
        {
            doFixups();
            cwVisit();

            if (enclosingMember instanceof Method)
            {
                Method m = (Method) enclosingMember;
                cw.visitOuterClass(m.getDeclaringClass().getInternalName(), m.getName(),
                    m.getSignature());
                addToTypeMap(m.getDeclaringClass());
            } else if (enclosingMember instanceof ClassType)
            {
                cw.visitOuterClass(((ClassType) enclosingMember).getInternalName(), null, null);
                addToTypeMap((ClassType) enclosingMember);
            }

            for (ClassType member = firstInnerClass; member != null; member = member.nextInnerClass)
            {
                addToTypeMap(member);
            }

            for (ClassType c : usedTypes.values())
            {
                if (c.getEnclosingMember() != null)
                {
                    String outer = c.getDeclaringClass() == null ? null
                        : c.getDeclaringClass().getInternalName();
                    String simple = c.getSimpleName();
                    simple = (simple == null || simple.length() == 0) ? null : simple;
                    cw.visitInnerClass(c.getInternalName(), outer, simple,
                        c.getModifiers() & ~Access.SUPER);
                }
            }

            Field f = fields;
            while (f != null)
            {
                String signature = f.getType().getSignature();
                String genericSignature = f.getType().getGenericSignature();
                FieldVisitor fv = cw.visitField(f.getModifiers(), f.getName(), signature,
                    signature.equals(genericSignature) ? null : genericSignature, f.constantValue);
                for (AnnotationEntry ae : getAnnotationEntries(f, true))
                    visitAnnotation(fv.visitAnnotation(ae.annotationType.getSignature(), true), ae);
                for (AnnotationEntry ae : getAnnotationEntries(f, false))
                    visitAnnotation(fv.visitAnnotation(ae.annotationType.getSignature(), false),
                        ae);
                fv.visitEnd();
                f = f.getNext();
            }
            Method m = methods;
            while (m != null)
            {
                m.mvVisit();
                if (m.code != null)
                {
                    for (Fragment fragment : m.code.fragments)
                    {
                        fragment.emit();
                    }
                    m.code.emit();
                    m.mv.visitMaxs(100, 100);
                }
                for (AnnotationEntry ae : getAnnotationEntries(m, true))
                    visitAnnotation(m.mv.visitAnnotation(ae.annotationType.getSignature(), true),
                        ae);
                for (AnnotationEntry ae : getAnnotationEntries(m, false))
                    visitAnnotation(m.mv.visitAnnotation(ae.annotationType.getSignature(), false),
                        ae);
                m.mv.visitEnd();
                m = m.getNext();
            }
            for (AnnotationEntry ae : getAnnotationEntries(this, true))
                visitAnnotation(cw.visitAnnotation(ae.annotationType.getSignature(), true), ae);
            for (AnnotationEntry ae : getAnnotationEntries(this, false))
                visitAnnotation(cw.visitAnnotation(ae.annotationType.getSignature(), false), ae);
            String debug = null;
            if (sourceDbgExt != null)
                debug = sourceDbgExt.getData();
            cw.visitSource(fileName, debug);
            cw.visitEnd();
            cwEnded = true;
        }
        return mcw.toByteArray();
    }

    /** True if this class/interface implements the interface iface. */
    public final boolean implementsInterface(ClassType iface)
    {
        if (this == iface)
            return true;
        ClassType baseClass = this.getSuperclass();
        if (baseClass != null && baseClass.implementsInterface(iface))
            return true;
        ClassType[] interfaces = getInterfaces();
        if (interfaces != null)
        {
            for (int i = interfaces.length; --i >= 0;)
            {
                if (interfaces[i].implementsInterface(iface))
                    return true;
            }
        }
        return false;
    }

    /**
     * A more efficient version of isSubclass(ClassType.make(cname)). Does not
     * cause the named class be loaded if it hasn't been.
     * 
     * @param cname
     *            a class name - cannot be an interface name
     */
    public final boolean isSubclass(String cname)
    {
        ClassType ctype = this;
        for (;;)
        {
            if (cname.equals(ctype.getName()))
                return true;
            ctype = ctype.getSuperclass();
            if (ctype == null)
                return false;
        }
    }

    public final boolean isSubclass(ClassType other)
    {
        if (other.isInterface())
            return implementsInterface(other);
        if ((this == javalangStringType && other == toStringType))
            return true;
        if (other == Type.javalangObjectType)
            return true;
        ClassType baseClass = this;
        while (baseClass != null)
        {
            if (baseClass == other)
                return true;
            baseClass = baseClass.getSuperclass();
        }
        return false;
    }

    @Override
    public int isCompatibleWithValue(Type valueType)
    {
        if (this == objectType && valueType instanceof ObjectType)
            return 2;
        if (valueType == Type.nullType || Type.isSame(this, valueType))
            return 2;
        if (isInterface())
        {
            Type rawType = valueType.getRawType();
            if (!(rawType instanceof ClassType))
                return -1;
            else if (rawType == objectType)
                return 0;
            else if (((ClassType) rawType).implementsInterface(this))
                return 2;
            else if (rawType.isInterface() && implementsInterface((ClassType) rawType))
                return 0;
            else
                // In theory a subclass of valueType might implement this
                // interface, but that is not normal practice.
                return -1;
        }
        int comp = compare(valueType);
        if (comp >= 0)
        {
            return valueType instanceof ObjectType ? 2 : 1;
        } else
            return comp == -3 ? -1 : 0;
    }

    public int compare(Type other)
    {
        if (other == nullType)
            return 1;
        if (!(other instanceof ClassType))
            return swappedCompareResult(other.compare(this));
        String name = getName();
        if (name != null && name.equals(other.getName()))
            return 0;
        ClassType cother = (ClassType) other;
        if (isSubclass(cother))
            return -1;
        if (cother.isSubclass(this))
            return 1;
        if (this.isInterface())
            return cother.isAnnotation() || cother.isFinal() ? -3
                : cother == Type.javalangObjectType ? -1 : -2;
        if (cother.isInterface())
            return isAnnotation() || isFinal() ? -3 : this == Type.javalangObjectType ? 1 : -2;
        return -3;
    }

    public String toString()
    {
        return "ClassType " + getName();
    }

    /**
     * @serialData Write the class name (as given by getName()) using writeUTF.
     */
    public void writeExternal(ObjectOutput out) throws IOException
    {
        out.writeUTF(getName());
    }

    public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException
    {
        setName(in.readUTF());
        flags |= ClassType.EXISTING_CLASS;
    }

    public Object readResolve() throws ObjectStreamException
    {
        String name = getName();
        /* #ifdef JAVA5 */
        java.util.HashMap<String, Type> map = mapNameToType;
        /* #else */
        // java.util.Hashtable map = mapNameToType;
        /* #endif */
        synchronized (map)
        {
            Type found = (Type) map.get(name);
            if (found != null)
                return found;
            map.put(name, this);
        }
        return this;
    }

    /** Clear various object references, to help garbage collection. */
    public void cleanupAfterCompilation()
    {
        for (Method meth = methods; meth != null; meth = meth.getNext())
            meth.cleanupAfterCompilation();

        attributes = null;
        sourceDbgExt = null;
        cw = mcw = null;
        usedTypes = null;
    }

    /**
     * Check to see if this is a Single Abstract Method (SAM) type. I.e. an
     * interface or abstract class that has one and only one abstract method.
     * (One way that lambdas/closures are useful is that when given a lambda in
     * a context that requires a SAM, create an implementing class using the
     * lambda for the abstract method.)
     * 
     * @return the single abstract Method, or null if this is not a SAM type.
     */
    public Method checkSingleAbstractMethod()
    {
        Method[] methods = getAbstractMethods();
        int nmethods = methods.length;
        Method result = null;
        for (int i = 0; i < nmethods; i++)
        {
            Method meth = methods[i];
            final String mname = meth.getName();
            final String sig = meth.getSignature();
            Filter<Method> filter = new Filter<Method>() {
                public boolean select(Method m)
                {
                    return (!m.isAbstract() && mname.equals(m.getName())
                        && sig.equals(m.getSignature()));
                }
            };
            if (countMethods(filter, 2) > 0)
                continue;
            if (result != null)
                return null;
            result = meth;
        }
        return result;
    }

    Map<String, ClassType> usedTypes = new HashMap<String, ClassType>();

    void addToTypeMap(ClassType ct)
    {
        if (isExisting() || ct == null)
            return;
        ClassType current = usedTypes.get(ct.getInternalName());
        // if (current != null && current != ct)
        // throw new Error(getInternalName() + ":" + ct.getInternalName()
        // + " is already in the map");
        usedTypes.put(ct.getInternalName(), ct);
    }

    class MyClassWriter extends ClassWriter
    {
        public MyClassWriter(int flags)
        {
            super(flags);
        }

        protected String getCommonSuperClass(String name1, String name2)
        {
            ClassType type1 = usedTypes.get(name1);
            ClassType type2 = usedTypes.get(name2);
            ClassType merged = (ClassType) Type.lowestCommonSuperType(type1, type2);
            addToTypeMap(merged);
            return merged.getInternalName();
        }
    }
}
