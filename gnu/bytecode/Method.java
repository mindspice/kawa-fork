// Copyright (c) 1997, 2007  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

import java.io.*;

/**
 * Represents a method in a <code>ClassType</code>.
 * <p>
 * A <code>Method</code> contain a <code>CodeAttr</code> object; the interface
 * for generating bytecode instructions is primarily in <code>CodeAttr</code>.
 * <p>
 * All the methods whose name start with <code>compile_</code> are deprecated,
 * and should not be used; use the methods in <code>CodeAttr</code>instead.
 */

public class Method implements AttrContainer, Member
{
    private String name;
    Type[] arg_types;
    Type[] generic_arg_types;
    Type return_type;
    int access_flags;
    int name_index; /* Index in constant table, or 0 if un-assigned */
    int signature_index; /* Index in constant table, or 0 if un-assigned */
    Method next;
    ClassType classfile;
    CodeAttr code;
    /* #ifdef JAVA8 */
    java.lang.reflect.Executable rmethod;
    /* #else */
    // java.lang.reflect.AccessibleObject rmethod;
    /* #endif */

    Attribute attributes;

    public final Attribute getAttributes()
    {
        return attributes;
    }

    public final void setAttributes(Attribute attributes)
    {
        this.attributes = attributes;
    }

    ExceptionsAttr exceptions;

    public final ExceptionsAttr getExceptionAttr()
    {
        return exceptions;
    }

    public void setExceptions(ClassType[] exn_types)
    {
        if (exceptions == null)
            exceptions = new ExceptionsAttr(this);
        exceptions.setExceptions(exn_types);
    }

    public final CodeAttr getCode()
    {
        return code;
    }

    private Method()
    {
    }

    /**
     * Make a generic "clone" method. This is used for array types.
     */
    static Method makeCloneMethod(Type returnType)
    {
        Method method = new Method();
        method.name = "clone";
        method.access_flags = Access.PUBLIC;
        method.arg_types = Type.typeArray0;
        method.return_type = returnType;
        method.classfile = Type.objectType;
        return method;
    }

    /**
     * A copy constructor, except you can override the declaring class. This can
     * be used to improve binary compatibility by emitting method references
     * where the declared class is the type of the receiver.
     */
    public Method(Method base, ClassType clas)
    {
        arg_types = base.arg_types;
        return_type = base.return_type;
        name = base.name;
        access_flags = base.access_flags;
        classfile = clas;
    }

    Method(ClassType clfile, int flags)
    {
        if (clfile.last_method == null)
            clfile.methods = this;
        else
            clfile.last_method.next = this;
        clfile.last_method = this;
        clfile.methods_count++;
        access_flags = flags;
        classfile = clfile;
    }

    public final void setStaticFlag(boolean is_static)
    {
        if (is_static)
            access_flags |= Access.STATIC;
        else
            access_flags ^= ~Access.STATIC;
    }

    public final boolean getStaticFlag()
    {
        return (access_flags & Access.STATIC) != 0;
    }

    public final boolean isAbstract()
    {
        return (access_flags & Access.ABSTRACT) != 0;
    }

    public final boolean isNative()
    {
        return (access_flags & Access.NATIVE) != 0;
    }

    public int getModifiers()
    {
        return access_flags;
    }

    public void setModifiers(int modifiers)
    {
        access_flags = modifiers;
    }

    public Scope pushScope()
    {
        prepareCode(0);
        return code.pushScope();
    }

    /** True if control could reach here. */
    public final boolean reachableHere()
    {
        return code.reachableHere();
    }

    public Scope popScope()
    {
        return code.popScope();
    }

    /**
     * Allocate slots for a local variable (or parameter).
     * 
     * @param local
     *            the variable we need to allocate
     */
    @Deprecated
    public void allocate_local(Variable local)
    {
        local.allocateLocal(code);
    }

    org.objectweb.asm.commons.CodeSizeEvaluator mv;

    synchronized void mvVisit()
    {
        if (mv == null)
        {
            String[] s_exceptions = null;
            if (exceptions != null)
            {
                ClassType[] c_exceptions = exceptions.getExceptions();
                s_exceptions = new String[c_exceptions.length];
                for (int i = 0; i < c_exceptions.length; i++)
                    s_exceptions[i] = c_exceptions[i].getInternalName();
            }
            String generic_sig = makeGenericSignature(generic_arg_types, return_type);
            if (generic_sig.equals(getSignature()))
                generic_sig = null;
            mv = new org.objectweb.asm.commons.CodeSizeEvaluator(classfile.cw
                .visitMethod(getModifiers(), getName(), getSignature(), generic_sig, s_exceptions));
        }
    }

    /**
     * Allocate a Code attribute, and prepare to generate code. Most code
     * generators should use the startCode convenience method.
     */
    public void initCode()
    {
        mvVisit();
        mv.visitCode();
        prepareCode(0);
        code.sourceDbgExt = classfile.sourceDbgExt;
        code.noteParamTypes();
        code.pushScope();
    }

    /**
     * @deprecated Use startCode instead
     */
    public void init_param_slots()
    {
        startCode();
    }

    /** Recommended method to create a new CodeAttr for this Method. */
    public CodeAttr startCode()
    {
        initCode();
        code.addParamLocals();
        return code;
    }

    void kill_local(Variable var)
    {
        var.freeLocal(code);
    }

    /**
     * Method that must be called before we generate any instructions. Set so
     * there is room for at least max_size bytes of code.
     */
    void prepareCode(int max_size)
    {
        if (code == null)
            code = new CodeAttr(this);
    }

    // This method should be called before we generate code for
    // an instruction (or sequence).
    // An upper bound of the intruction length is max_size.
    // deprecated!
    void instruction_start_hook(int max_size)
    {
        prepareCode(max_size);
    }

    final Type pop_stack_type()
    {
        return code.popType();
    }

    final void push_stack_type(Type type)
    {
        code.pushType(type);
    }

    public void compile_checkcast(Type type)
    {
        code.emitCheckcast(type);
    }

    public void maybe_compile_checkcast(Type type)
    {
        Type stack_type = code.topType();
        if (type != stack_type) // FIXME rather simple-minded, but safe.
            code.emitCheckcast(type);
    }

    /**
     * Comple code to push the contents of a local variable onto the statck.
     * 
     * @param var
     *            The variable whose contents we want to push.
     * @deprecated
     */
    public void push_var(Variable var)
    {
        code.emitLoad(var);
    }

    @Deprecated
    public void compile_push_value(Variable var)
    {
        code.emitLoad(var);
    }

    @Deprecated
    public void compile_store_value(Variable var)
    {
        code.emitStore(var);
    }

    public void compile_push_this()
    {
        code.emitPushThis();
    }

    public static String makeSignature(Type arg_types[], Type return_type)
    {
        /* #ifdef JAVA5 */
        StringBuilder buf = new StringBuilder(100);
        /* #else */
        // StringBuffer buf = new StringBuffer(100);
        /* #endif */
        int args_count = arg_types.length;
        buf.append('(');
        for (int i = 0; i < args_count; i++)
            buf.append(arg_types[i].getSignature());
        buf.append(')');
        buf.append(return_type.getSignature());
        return buf.toString();
    }

    public static String makeGenericSignature(Type arg_types[], Type return_type)
    {
        StringBuilder buf = new StringBuilder(100);
        int args_count = arg_types.length;
        buf.append('(');
        for (int i = 0; i < args_count; i++)
            buf.append(arg_types[i].getMaybeGenericSignature());
        buf.append(')');
        buf.append(return_type.getMaybeGenericSignature());
        return buf.toString();
    }

    String signature;

    public String getSignature()
    {
        if (signature == null)
            signature = makeSignature(arg_types, return_type);
        return signature;
    }

    public void setSignature(String signature)
    {
        int len = signature.length();
        if (len < 3 || signature.charAt(0) != '(')
            throw new ClassFormatError("bad method signature");
        int pos = 1;
        java.util.Stack<Type> types = new java.util.Stack<Type>();
        for (;;)
        {
            int arg_sig_len = Type.signatureLength(signature, pos);
            if (arg_sig_len < 0)
            {
                if (pos < len && signature.charAt(pos) == ')')
                    break;
                throw new ClassFormatError("bad method signature");
            }
            Type arg_type = Type.signatureToType(signature, pos, arg_sig_len);
            types.push(arg_type);
            pos += arg_sig_len;
        }
        arg_types = new Type[types.size()];
        for (int i = types.size(); --i >= 0;)
            arg_types[i] = (Type) types.pop();
        generic_arg_types = arg_types.clone();
        return_type = Type.signatureToType(signature, pos + 1, len - pos - 1);
    }

    public <T extends java.lang.annotation.Annotation> T getAnnotation(Class<T> clas)
    {
        T ann = RuntimeAnnotationsAttr.getAnnotation(this, clas);
        if (ann != null)
            return ann;
        return rmethod == null ? null : rmethod.getAnnotation(clas);
    }

    public ClassType getDeclaringClass()
    {
        return classfile;
    }

    public final Type getReturnType()
    {
        return return_type;
    }

    public final void setReturnType(Type type)
    {
        return_type = type;
    }

    public final Type[] getParameterTypes()
    {
        return arg_types;
    }

    public final Type[] getGenericParameterTypes()
    {
        return generic_arg_types;
    }

    TypeVariable[] typeParameters;

    public synchronized TypeVariable[] getTypeParameters()
    {
        TypeVariable[] params = typeParameters;
        /* #ifdef JAVA8 */
        if (params == null && rmethod != null)
        {
            java.lang.reflect.TypeVariable[] rparams = rmethod.getTypeParameters();
            int nparams = rparams.length;
            params = new TypeVariable[nparams];
            for (int i = 0; i < nparams; i++)
            {
                params[i] = TypeVariable.make(rparams[i]);
            }
            typeParameters = params;
        }
        /* #endif */
        return params;
    }

    public final ClassType[] getExceptions()
    {
        if (exceptions == null)
            return null;
        return exceptions.getExceptions();
    }

    public final String getName()
    {
        return name;
    }

    public final void setName(String name)
    {
        this.name = name;
    }

    public final Method getNext()
    {
        return next;
    }

    public void listParameters(StringBuffer sbuf)
    {
        int args_count = arg_types.length;
        sbuf.append('(');
        for (int i = 0; i < args_count; i++)
        {
            if (i > 0)
                sbuf.append(',');
            sbuf.append(arg_types[i].getName());
        }
        sbuf.append(')');
    }

    public String toString()
    {
        StringBuffer sbuf = new StringBuffer(100);
        ClassType declaring = getDeclaringClass();
        sbuf.append(declaring != null ? declaring.getName() : "???");
        sbuf.append('.');
        sbuf.append(name);
        if (arg_types != null)
        {
            listParameters(sbuf);
            sbuf.append(return_type.getName());
        }
        return sbuf.toString();
    }

    public void cleanupAfterCompilation()
    {
        attributes = null;
        exceptions = null;
        code = null;
        mv = null;
    }
};
