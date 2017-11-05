// Copyright (c) 1997, 1998, 1999, 2001, 2003, 2004, 2008 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

import org.objectweb.asm.Handle;
import org.objectweb.asm.MethodVisitor;

import static org.objectweb.asm.Opcodes.*;

/**
 * Represents the contents of a standard "Code" attribute.
 * <p>
 * Most of the actual methods that generate bytecode operation are in this class
 * (typically with names starting with <code>emit</code>), though there are also
 * some in <code>Method</code>.
 * <p>
 * Note that a <code>CodeAttr</code> is an <code>Attribute</code> of a
 * <code>Method</code>, and can in turn contain other <code>Attribute</code>s,
 * such as a <code>LineNumbersAttr</code>.
 *
 * @author Per Bothner
 */

public class CodeAttr extends Attribute implements AttrContainer
{
    Attribute attributes;
    Label prevGoto;
    java.util.ArrayDeque<Label> afterGotoLabels = new java.util.ArrayDeque<Label>();
    java.util.ArrayDeque<Integer> afterGotoLineNumbers = new java.util.ArrayDeque<Integer>();
    MethodVisitor mvisitor;
    int insnCount = 0;

    void flushGoto()
    {
        if (prevGoto != null)
        {
            if (!afterGotoLabels.contains(prevGoto))
                emitJumpInsn(GOTO, prevGoto.asmLabel);
            prevGoto = null;
            while (!afterGotoLabels.isEmpty())
                mvisitor.visitLabel(afterGotoLabels.poll().asmLabel);
            while (!afterGotoLineNumbers.isEmpty())
                putLineNumber(afterGotoLineNumbers.poll());
        }
    }

    void emitFieldInsn(int opcode, String owner, String name, String desc)
    {
        insnCount++;
        mvisitor.visitFieldInsn(opcode, owner, name, desc);
    }

    void emitIincInsn(int var, int increment)
    {
        insnCount++;
        mvisitor.visitIincInsn(var, increment);
    }

    void emitInsn(int opcode)
    {
        insnCount++;
        mvisitor.visitInsn(opcode);
    }

    void emitIntInsn(int opcode, int operand)
    {
        insnCount++;
        mvisitor.visitIntInsn(opcode, operand);
    }

    void emitJumpInsn(int opcode, org.objectweb.asm.Label label)
    {
        insnCount++;
        mvisitor.visitJumpInsn(opcode, label);
    }

    void emitLdcInsn(Object cst)
    {
        insnCount++;
        mvisitor.visitLdcInsn(cst);
    }

    void emitLookupSwitchInsn(org.objectweb.asm.Label dflt, int[] keys,
        org.objectweb.asm.Label[] labels)
    {
        insnCount++;
        mvisitor.visitLookupSwitchInsn(dflt, keys, labels);
    }

    void emitMethodInsn(int opcode, String owner, String name, String desc, boolean itf)
    {
        insnCount++;
        mvisitor.visitMethodInsn(opcode, owner, name, desc, itf);
    }

    void emitMultiANewArrayInsn(String desc, int dims)
    {
        insnCount++;
        mvisitor.visitMultiANewArrayInsn(desc, dims);
    }

    void emitTableSwitchInsn(int min, int max, org.objectweb.asm.Label asmLabel,
        org.objectweb.asm.Label... asmLabels)
    {
        insnCount++;
        mvisitor.visitTableSwitchInsn(min, max, asmLabel, asmLabels);
    }

    void emitTypeInsn(int opcode, String type)
    {
        insnCount++;
        mvisitor.visitTypeInsn(opcode, type);
    }

    void emitVarInsn(int opcode, int var)
    {
        insnCount++;
        mvisitor.visitVarInsn(opcode, var);
    }

    public final Attribute getAttributes()
    {
        return attributes;
    }

    public final void setAttributes(Attribute attributes)
    {
        this.attributes = attributes;
    }

    public LocalVarsAttr locals;

    SourceDebugExtAttr sourceDbgExt;

    public static final int GENERATE_STACK_MAP_TABLE = 1;
    public static final int DONT_USE_JSR = 2;
    int flags;

    public Type[] stack_types;
    Type[] local_types = new Type[20];

    /** Previously-defined label. */
    Label previousLabel;
    /**
     * Set of vars set in current block (since previousLabel). This is so we can
     * differentiate variable set locally, versus definitions that reach us (and
     * that might be invalidated by future flows).
     */
    boolean[] varsSetInCurrentBlock;

    int SP; // Current stack size (in "words")
    private int max_stack;
    private int max_locals;

    boolean useJsr()
    {
        return (flags & DONT_USE_JSR) == 0;
    }

    /**
     * If true we get a line number entry for each instruction. Normally false,
     * but can be a convenient hack to allow instruction-level
     * stepping/debugging and stacktraces. In this case {@code LINE==PC}.
     */
    public static boolean instructionLineMode = false;

    /** The stack of currently active conditionals. */
    IfState if_stack;

    /** The stack of currently active try statements. */
    TryState try_stack;

    int prev_linenumber = -1;

    public final Method getMethod()
    {
        return (Method) getContainer();
    }

    public final int getSP()
    {
        return SP;
    }

    public int getInsnCount()
    {
        return insnCount;
    }

    /*
     * True if we cannot fall through to bytes[PC] - the previous instruction
     * was an uncondition control transfer.
     */
    private boolean unreachable_here;

    /** True if control could reach here. */
    public final boolean reachableHere()
    {
        return !unreachable_here;
    }

    public final void setReachable(boolean val)
    {
        unreachable_here = !val;
    }

    public final void setUnreachable()
    {
        unreachable_here = true;
    }

    /** Get the maximum number of words on the operand stack in this method. */
    public int getMaxStack()
    {
        return max_stack;
    }

    /** Get the maximum number of local variable words in this method. */
    public int getMaxLocals()
    {
        return max_locals;
    }

    /** Set the maximum number of words on the operand stack in this method. */
    public void setMaxStack(int n)
    {
        max_stack = n;
    }

    /** Set the maximum number of local variable words in this method. */
    public void setMaxLocals(int n)
    {
        max_locals = n;
    }

    public CodeAttr(Method meth)
    {
        super("Code");
        addToFrontOf(meth);
        meth.code = this;
        if (meth.getDeclaringClass().getClassfileMajorVersion() >= 50)
            flags |= GENERATE_STACK_MAP_TABLE | DONT_USE_JSR;
        mvisitor = meth.mv;
    }

    /** Get opcode that implements NOT (x OPCODE y). */
    byte invert_opcode(byte opcode)
    {
        int iopcode = opcode & 0xFF;
        if ((iopcode >= 153 && iopcode <= 166) || (iopcode >= 198 && iopcode <= 199))
            return (byte) (iopcode ^ 1);
        throw new Error("unknown opcode to invert_opcode");
    }

    public final void putLineNumber(String filename, int linenumber)
    {
        if (filename != null)
            getMethod().classfile.setSourceFile(filename);
        putLineNumber(linenumber);
    }

    public final void putLineNumber(int linenumber)
    {
        if (prevGoto != null)
        {
            afterGotoLineNumbers.add(linenumber);
            return;
        }
        if (linenumber == prev_linenumber)
            return;
        if (sourceDbgExt != null)
            linenumber = sourceDbgExt.fixLine(linenumber);
        org.objectweb.asm.Label l = new org.objectweb.asm.Label();
        mvisitor.visitLabel(l);
        mvisitor.visitLineNumber(linenumber, l);
        prev_linenumber = linenumber;
    }

    /** Initialize local_types from parameters. */
    void noteParamTypes()
    {
        Method method = getMethod();
        int offset = 0;
        if ((method.access_flags & Access.STATIC) == 0)
        {
            Type type = method.classfile;
            if ("<init>".equals(method.getName()) && !"java.lang.Object".equals(type.getName()))
                type = UninitializedType.uninitializedThis((ClassType) type);
            noteVarType(offset++, type);
        }
        int arg_count = method.arg_types.length;
        for (int i = 0; i < arg_count; i++)
        {
            Type type = method.arg_types[i];
            noteVarType(offset++, type);
            for (int size = type.getSizeInWords(); --size > 0;)
                offset++;
        }
    }

    void setPreviousLabelHere(Label here)
    {
        previousLabel = here;
        boolean[] varsSet = varsSetInCurrentBlock;
        if (varsSet != null)
            for (int i = varsSet.length; --i >= 0;)
                varsSet[i] = false;
    }

    public void noteVarType(int offset, Type type)
    {
        int size = type.getSizeInWords();

        if (local_types == null)
            local_types = new Type[offset + size + 20];
        else if (offset + size > local_types.length)
        {
            Type[] new_array = new Type[2 * (offset + size)];
            System.arraycopy(local_types, 0, new_array, 0, local_types.length);
            local_types = new_array;
        }
        local_types[offset] = type;
        if (varsSetInCurrentBlock == null)
            varsSetInCurrentBlock = new boolean[local_types.length];
        else if (varsSetInCurrentBlock.length <= offset)
        {
            boolean[] tmp = new boolean[local_types.length];
            System.arraycopy(varsSetInCurrentBlock, 0, tmp, 0, varsSetInCurrentBlock.length);
            varsSetInCurrentBlock = tmp;
        }
        varsSetInCurrentBlock[offset] = true;
        if (offset > 0)
        {
            Type prev = local_types[offset - 1];
            if (prev != null && prev.getSizeInWords() == 2)
                local_types[offset - 1] = null;
        }
        while (--size > 0)
            local_types[++offset] = null;
    }

    /** Set the current type state from a label. */
    public final void setTypes(Label label)
    {
        setTypes(label.localTypes, label.stackTypes);
    }

    /** Set the current type state from a label. */
    public final void setTypes(Type[] labelLocals, Type[] labelStack)
    {
        int usedStack = labelStack.length;
        int usedLocals = labelLocals.length;
        if (local_types != null)
        {
            if (usedLocals > 0)
                System.arraycopy(labelLocals, 0, local_types, 0, usedLocals);
            for (int i = usedLocals; i < local_types.length; i++)
                local_types[i] = null;
        }
        if (stack_types == null || usedStack > stack_types.length)
            stack_types = new Type[usedStack];
        else
        {
            for (int i = usedStack; i < stack_types.length; i++)
                stack_types[i] = null;
        }
        System.arraycopy(labelStack, 0, stack_types, 0, usedStack);
        SP = usedStack;
    }

    public final void pushType(Type type)
    {
        if (type.size == 0)
            throw new Error("pushing void type onto stack");
        if (stack_types == null || stack_types.length == 0) // ??
            stack_types = new Type[20];
        else if (SP + 1 >= stack_types.length)
        {
            Type[] new_array = new Type[2 * stack_types.length];
            System.arraycopy(stack_types, 0, new_array, 0, SP);
            stack_types = new_array;
        }
        if (type.getRawType() instanceof ClassType)
            getMethod().classfile.addToTypeMap((ClassType) type.getRawType());
        if (type.size == 8)
            stack_types[SP++] = Type.voidType;
        stack_types[SP++] = type;
        if (SP > max_stack)
            max_stack = SP;
    }

    public final Type popType()
    {
        if (SP <= 0)
            throw new Error("popType called with empty stack " + getMethod());
        Type type = stack_types[--SP];
        if (type.size == 8)
            if (!popType().isVoid())
                throw new Error("missing void type on stack");
        return type;
    }

    public final Type topType()
    {
        return stack_types[SP - 1];
    }

    /**
     * Compile code to pop values off the stack (and ignore them).
     * 
     * @param nvalues
     *            the number of values (not words) to pop
     */
    public void emitPop(int nvalues)
    {
        flushGoto();
        for (; nvalues > 0; --nvalues)
        {
            Type type = popType();
            if (type.size > 4)
                emitInsn(POP2);
            else if (nvalues > 1)
            { // optimization: can we pop 2 4-byte words
              // using a pop2
                Type type2 = popType();
                if (type2.size > 4)
                {
                    emitInsn(POP);
                }
                emitInsn(POP2);
                --nvalues;
            } else
                emitInsn(POP);
        }
    }

    void defineRaw(Label l)
    {
        if (prevGoto != null)
            afterGotoLabels.add(l);
        else
            mvisitor.visitLabel(l.asmLabel);
    }

    /**
     * Get a new Label for the current location. Unlike Label.define, does not
     * change reachableHere().
     */
    public Label getLabel()
    {
        Label label = new Label();
        label.defineRaw(this);
        return label;
    }

    public void emitSwap()
    {
        flushGoto();
        Type type1 = popType();
        Type type2 = popType();

        if (type1.size > 4 || type2.size > 4)
        {
            // There is no swap instruction in the JVM for this case.
            // Fall back to a more convoluted way.
            pushType(type2);
            pushType(type1);
            emitDupX();
            emitPop(1);
        } else
        {
            pushType(type1);
            emitInsn(SWAP);
            pushType(type2);
        }
    }

    /** Emit code to duplicate the top element of the stack. */
    public void emitDup()
    {
        flushGoto();
        Type type = topType();
        emitInsn(type.size <= 4 ? DUP : DUP2); // dup or dup2
        pushType(type);
    }

    /**
     * Emit code to duplicate the top element of the stack and place the copy
     * before the previous element.
     */
    public void emitDupX()
    {
        flushGoto();
        Type type = popType();
        Type skipedType = popType();

        if (skipedType.size <= 4)
            emitInsn(type.size <= 4 ? DUP_X1 : DUP2_X1);
        else
            emitInsn(type.size <= 4 ? DUP_X2 : DUP2_X2);

        pushType(type);
        pushType(skipedType);
        pushType(type);
    }

    /**
     * Compile code to duplicate with offset.
     * 
     * @param size
     *            the size of the stack item to duplicate (1 or 2)
     * @param offset
     *            where to insert the result (must be 0, 1, or 2) The new words
     *            get inserted at stack[SP-size-offset]
     */
    public void emitDup(int size, int offset)
    {
        flushGoto();
        if (size == 0)
            return;
        // copied1 and (optionally copied2) are the types of the duplicated
        // words
        Type copied1 = popType();
        Type copied2 = null;
        if (size == 1)
        {
            if (copied1.size > 4)
                throw new Error("using dup for 2-word type");
        } else if (size != 2)
            throw new Error("invalid size to emitDup");
        else if (copied1.size <= 4)
        {
            copied2 = popType();
            if (copied2.size > 4)
                throw new Error("dup will cause invalid types on stack");
        }

        int kind;
        // These are the types of the words (in any) that are "skipped":
        Type skipped1 = null;
        Type skipped2 = null;
        if (offset == 0)
        {
            kind = size == 1 ? DUP : DUP2;
        } else if (offset == 1)
        {
            kind = size == 1 ? DUP_X1 : DUP2_X1;
            skipped1 = popType();
            if (skipped1.size > 4)
                throw new Error("dup will cause invalid types on stack");
        } else if (offset == 2)
        {
            kind = size == 1 ? DUP_X2 : DUP2_X2; // dup_x2 or dup2_x2
            skipped1 = popType();
            if (skipped1.size <= 4)
            {
                skipped2 = popType();
                if (skipped2.size > 4)
                    throw new Error("dup will cause invalid types on stack");
            }
        } else
            throw new Error("emitDup:  invalid offset");

        emitInsn(kind);
        if (copied2 != null)
            pushType(copied2);
        pushType(copied1);
        if (skipped2 != null)
            pushType(skipped2);
        if (skipped1 != null)
            pushType(skipped1);
        if (copied2 != null)
            pushType(copied2);
        pushType(copied1);
    }

    /**
     * Compile code to duplicate the top 1 or 2 words.
     * 
     * @param size
     *            number of words to duplicate
     */
    public void emitDup(int size)
    {
        emitDup(size, 0);
    }

    public void emitDup(Type type)
    {
        emitDup(type.size > 4 ? 2 : 1, 0);
    }

    public void enterScope(Scope scope)
    {
        scope.setStartPC(this);
        locals.enterScope(scope);
    }

    public Scope pushScope()
    {
        Scope scope = new Scope();
        if (locals == null)
            locals = new LocalVarsAttr(getMethod());
        enterScope(scope);
        if (locals.parameter_scope == null)
            locals.parameter_scope = scope;
        return scope;
    }

    /**
     * Create a Scope that is automatically popped. I.e. the next popScope will
     * keep popping autoPop scopes until it gets to a non-autoPop scope. An
     * autoPop Scope is useful for variables that are assigned and set in the
     * middle of a managed Scope.
     */
    public Scope pushAutoPoppableScope()
    {
        Scope scope = pushScope();
        scope.autoPop = true;
        return scope;
    }

    public Scope getCurrentScope()
    {
        return locals.current_scope;
    }

    public Scope popScope()
    {
        flushGoto();
        Label end = getLabel();
        for (;;)
        {
            Scope scope = locals.current_scope;
            Variable v = scope.vars;
            while (v != null)
            {
                if (v.getName() != null)
                    mvisitor.visitLocalVariable(v.getName(), v.getSignature(),
                        v.getType().getGenericSignature(), scope.start.asmLabel, end.asmLabel,
                        v.offset);
                if (v == scope.last_var)
                    break;
                v = v.nextVar();
            }
            locals.current_scope = scope.parent;
            scope.freeLocals(this);
            scope.end = end;
            if (!scope.autoPop)
                return scope;
        }
    }

    /** Get the index'th parameter. */
    public Variable getArg(int index)
    {
        return locals.parameter_scope.getVariable(index);
    }

    /**
     * Search by name for a Variable
     * 
     * @param name
     *            name to search for
     * @return the Variable, or null if not found (in any scope of this Method).
     */
    public Variable lookup(String name)
    {
        Scope scope = locals.current_scope;
        for (; scope != null; scope = scope.parent)
        {
            Variable var = scope.lookup(name);
            if (var != null)
                return var;
        }
        return null;
    }

    /**
     * Add a new local variable (in the current scope).
     * 
     * @param type
     *            type of the new Variable.
     * @return the new Variable.
     */
    public Variable addLocal(Type type)
    {
        return locals.current_scope.addVariable(this, type, null);
    }

    /**
     * Add a new local variable (in the current scope).
     * 
     * @param type
     *            type of the new Variable.
     * @param name
     *            name of the new Variable.
     * @return the new Variable.
     */
    public Variable addLocal(Type type, String name)
    {
        return locals.current_scope.addVariable(this, type, name);
    }

    /** Call addLocal for parameters (as implied by method type). */
    public void addParamLocals()
    {
        Method method = getMethod();
        if ((method.access_flags & Access.STATIC) == 0)
            addLocal(method.classfile).setParameter(true);
        int arg_count = method.arg_types.length;
        for (int i = 0; i < arg_count; i++)
            addLocal(method.arg_types[i]).setParameter(true);
    }

    public final void emitPushConstant(int val, Type type)
    {
        switch (type.getSignature().charAt(0)) {
        case 'B':
        case 'C':
        case 'I':
        case 'Z':
        case 'S':
            emitPushInt(val);
            break;
        case 'J':
            emitPushLong((long) val);
            break;
        case 'F':
            emitPushFloat((float) val);
            break;
        case 'D':
            emitPushDouble((double) val);
            break;
        default:
            throw new Error("bad type to emitPushConstant");
        }
    }

    public final void emitPushInt(int i)
    {
        flushGoto();
        if (i >= -1 && i <= 5)
            emitInsn(i + 3); // iconst_m1 .. iconst_5
        else if (i >= -128 && i < 128)
        {
            emitIntInsn(BIPUSH, i);
        } else if (i >= -32768 && i < 32768)
        {
            emitIntInsn(SIPUSH, i);
        } else
        {
            emitLdcInsn(i);
        }
        pushType(Type.intType);
    }

    public void emitPushLong(long i)
    {
        flushGoto();
        if (i == 0 || i == 1)
        {
            emitInsn(9 + (int) i); // lconst_0 .. lconst_1
        } else if ((long) (int) i == i)
        {
            emitPushInt((int) i);
            popType();
            emitInsn(I2L);
        } else
        {
            emitLdcInsn(i);
        }
        pushType(Type.longType);
    }

    public void emitPushFloat(float x)
    {
        flushGoto();
        int xi = (int) x;
        if ((float) xi == x && xi >= -128 && xi < 128)
        {
            if (xi >= 0 && xi <= 2)
            {
                emitInsn(11 + xi); // fconst_0 .. fconst_2
                if (xi == 0 && Float.floatToIntBits(x) != 0) // x == -0.0
                {
                    emitInsn(FNEG);
                }
            } else
            {
                // Saves space in the constant pool
                // Probably faster, at least on modern CPUs.
                emitPushInt(xi);
                popType();
                emitInsn(I2F);
            }
        } else
        {
            emitLdcInsn(x);
        }
        pushType(Type.floatType);
    }

    public void emitPushDouble(double x)
    {
        flushGoto();
        int xi = (int) x;
        if ((double) xi == x && xi >= -128 && xi < 128)
        {
            if (xi == 0 || xi == 1)
            {
                emitInsn(14 + xi); // dconst_0 or dconst_1
                if (xi == 0 && Double.doubleToLongBits(x) != 0L) // x == -0.0
                {
                    emitInsn(DNEG);
                }
            } else
            {
                // Saves space in the constant pool
                // Probably faster, at least on modern CPUs.
                emitPushInt(xi);
                popType();
                emitInsn(I2D);
            }
        } else
        {
            emitLdcInsn(x);
        }
        pushType(Type.doubleType);
    }

    /**
     * Calculate how many CONSTANT_String constants we need for a string. Each
     * CONSTANT_String can be at most 0xFFFF bytes (as a UTF8 string). Returns a
     * String, where each char, coerced to an int, is the length of a substring
     * of the input that is at most 0xFFFF bytes.
     */
    public static final String calculateSplit(String str)
    {
        int strLength = str.length();
        StringBuffer sbuf = new StringBuffer(20);
        // Where the current segments starts, as an index in 'str':
        int segmentStart = 0;
        int byteLength = 0; // Length in bytes of current segment so far.
        for (int i = 0; i < strLength; i++)
        {
            char ch = str.charAt(i);
            int bytes = ch >= 0x0800 ? 3 : ch >= 0x0080 || ch == 0 ? 2 : 1;
            if (byteLength + bytes > 0xFFFF)
            {
                sbuf.append((char) (i - segmentStart));
                segmentStart = i;
                byteLength = 0;
            }
            byteLength += bytes;
        }
        sbuf.append((char) (strLength - segmentStart));
        return sbuf.toString();
    }

    /**
     * Emit code to push the value of a constant String. Uses CONSTANT_String
     * and CONSTANT_Utf8 constant pool entries as needed. Can handle Strings
     * whose UTF8 length is greates than 0xFFFF bytes (the limit of a
     * CONSTANT_Utf8) by generating String concatenation.
     */
    public final void emitPushString(String str)
    {
        flushGoto();
        if (str == null)
            emitPushNull();
        else
        {
            int length = str.length();
            String segments = calculateSplit(str);
            int numSegments = segments.length();
            if (numSegments <= 1)
                emitLdcInsn(str);
            else
            {
                if (numSegments == 2)
                {
                    int firstSegment = (int) segments.charAt(0);
                    emitPushString(str.substring(0, firstSegment));
                    emitPushString(str.substring(firstSegment));
                    Method concatMethod = Type.javalangStringType.getDeclaredMethod("concat", 1);
                    emitInvokeVirtual(concatMethod);
                } else
                {
                    ClassType sbufType = ClassType.make("java.lang.StringBuffer");
                    emitNew(sbufType);
                    emitDup(sbufType);
                    emitPushInt(length);
                    Type[] args1 = { Type.intType };
                    emitInvokeSpecial(sbufType.getDeclaredMethod("<init>", args1));
                    Type[] args2 = { Type.javalangStringType };
                    Method appendMethod = sbufType.getDeclaredMethod("append", args2);
                    int segStart = 0;
                    for (int seg = 0; seg < numSegments; seg++)
                    {
                        emitDup(sbufType);
                        int segEnd = segStart + (int) segments.charAt(seg);
                        emitPushString(str.substring(segStart, segEnd));
                        emitInvokeVirtual(appendMethod);
                        segStart = segEnd;
                    }
                    emitInvokeVirtual(Type.toString_method);
                }
                if (str == str.intern())
                    emitInvokeVirtual(Type.javalangStringType.getDeclaredMethod("intern", 0));
                return;
            }
            pushType(Type.javalangStringType);
        }
    }

    /**
     * Push a class constant pool entry. This is only supported by JDK 1.5 and
     * later.
     */
    public final void emitPushClass(ObjectType ctype)
    {
        flushGoto();
        emitLdcInsn(org.objectweb.asm.Type.getObjectType(ctype.getInternalName()));
        pushType(Type.javalangClassType);
    }

    /**
     * Push a MethodHandle, using an appropriate constant pool entry. This is
     * only supported by JDK 1.6 and later.
     */
    public final void emitPushMethodHandle(Method method)
    {
        flushGoto();
        pushType(Type.javalanginvokeMethodHandleType);

        int kind;
        if ((method.access_flags & Access.STATIC) != 0)
            kind = H_INVOKESTATIC; // REF_invokeStatic
        else if (method.classfile.isInterface())
            kind = H_INVOKEINTERFACE; // REF_invokeInterface
        else if ("<init>".equals(method.getName()))
            kind = H_NEWINVOKESPECIAL; // REF_newInvokeSpecial
        else if ((method.access_flags & Access.PRIVATE) != 0)
            kind = H_INVOKESPECIAL; // REF_invokeSpecial
        else
            kind = H_INVOKEVIRTUAL; // REF_invokeVirtual
        emitLdcInsn(new Handle(kind, method.getDeclaringClass().getInternalName(), method.getName(),
            method.getSignature(), method.getDeclaringClass().isInterface()));
    }

    public void emitPushNull()
    {
        emitPushNull(Type.nullType);
    }

    public void emitPushNull(ObjectType type)
    {
        flushGoto();
        emitInsn(ACONST_NULL);
        pushType(type);
    }

    /** Push zero or null as appropriate for the given type. */
    public void emitPushDefaultValue(Type type)
    {
        type = type.getImplementationType();
        if (type instanceof PrimType)
            emitPushConstant(0, type);
        else
            emitPushNull();
    }

    /** Initialize a variable to zero or null, as appropriate. */
    public void emitStoreDefaultValue(Variable var)
    {
        emitPushDefaultValue(var.getType());
        emitStore(var);
    }

    public final void emitPushThis()
    {
        emitLoad(locals.used[0]);
    }

    public final void emitPushPrimArray(Object value, ArrayType arrayType)
    {
        int len = java.lang.reflect.Array.getLength(value);
        emitPushPrimArray(value, len, len, arrayType);
    }

    /** Emit code to push a constant primitive array.
     * @param value The array value that we want the emitted code to re-create.
     * @param arrayType The ArrayType that matches value.
     */
    public final void emitPushPrimArray(Object value, int len, int count,
                                        ArrayType arrayType)
    {
        Type elementType = arrayType.getComponentType();
        emitPushInt(len);
        emitNewArray(elementType);
        char sig = elementType.getSignature().charAt(0);
        for (int i = 0; i < count; i++) {
            long ival = 0;
            float fval = 0;
            double dval = 0;
            switch (sig) {
            case 'J':
                ival = ((long[]) value)[i];
                if (ival == 0)
                    continue;
                break;
            case 'I':
                ival = ((int[]) value)[i];
                if (ival == 0)
                    continue;
                break;
            case 'S':
                ival = ((short[]) value)[i];
                if (ival == 0)
                    continue;
                break;
            case 'C':
                ival = ((char[]) value)[i];
                if (ival == 0)
                    continue;
                break;
            case 'B':
                ival = ((byte[]) value)[i];
                if (ival == 0)
                    continue;
                break;
            case 'Z':
                ival = ((boolean[]) value)[i] ? 1 : 0;
                if (ival == 0)
                    continue;
                break;
            case 'F':
                fval = ((float[]) value)[i];
                if (fval == 0.0)
                    continue;
                break;
            case 'D':
                dval = ((double[]) value)[i];
                if (dval == 0.0)
                    continue;
                break;
            }
            emitDup(arrayType);
            emitPushInt(i);
            switch (sig) {
            case 'Z':
            case 'C':
            case 'B':
            case 'S':
            case 'I':
                emitPushInt((int) ival);
                break;
            case 'J':
                emitPushLong(ival);
                break;
            case 'F':
                emitPushFloat(fval);
                break;
            case 'D':
                emitPushDouble(dval);
                break;
            }
            emitArrayStore(elementType);
        }
    }

    void emitNewArray(int type_code)
    {
        flushGoto();
        emitIntInsn(NEWARRAY, type_code);
    }

    public final void emitArrayLength()
    {
        flushGoto();
        if (!(popType() instanceof ArrayType))
            throw new Error("non-array type in emitArrayLength");

        emitInsn(ARRAYLENGTH);
        pushType(Type.intType);
    }

    /*
     * Returns an integer in the range 0 (for 'int') through 4 (for object
     * reference) to 7 (for 'short') which matches the pattern of how JVM
     * opcodes typically depend on the operand type.
     */

    private int adjustTypedOp(char sig)
    {
        switch (sig) {
        case 'I':
            return 0; // int
        case 'J':
            return 1; // long
        case 'F':
            return 2; // float
        case 'D':
            return 3; // double
        default:
            return 4; // object
        case 'B':
        case 'Z':
            return 5; // byte or boolean
        case 'C':
            return 6; // char
        case 'S':
            return 7; // short
        }
    }

    private int adjustTypedOp(Type type)
    {
        return adjustTypedOp(type.getSignature().charAt(0));
    }

    private void emitTypedOp(int op, Type type)
    {
        flushGoto();
        emitInsn(op + adjustTypedOp(type));
    }

    private void emitTypedOp(int op, char sig)
    {
        flushGoto();
        emitInsn(op + adjustTypedOp(sig));
    }

    /**
     * Store into an element of an array. Must already have pushed the array
     * reference, the index, and the new value (in that order). Stack:
     * {@literal ..., array, index, value => ...}
     */
    public void emitArrayStore(Type element_type)
    {
        popType(); // Pop new value
        popType(); // Pop index
        popType(); // Pop array reference
        emitTypedOp(79, element_type);
    }

    /**
     * Store into an element of an array. Must already have pushed the array
     * reference, the index, and the new value (in that order). Stack:
     * {@literal ..., array, index, value => ...}
     */
    public void emitArrayStore()
    {
        popType(); // Pop new value
        popType(); // Pop index
        Type arrayType = popType().getImplementationType(); // Pop array
                                                            // reference
        Type elementType = ((ArrayType) arrayType).getComponentType();
        emitTypedOp(79, elementType);
    }

    /**
     * Load an element from an array. Must already have pushed the array and the
     * index (in that order): Stack: {@literal ..., array, index => ..., value}
     */
    public void emitArrayLoad(Type element_type)
    {
        popType(); // Pop index
        popType(); // Pop array reference
        emitTypedOp(46, element_type);
        pushType(element_type);
    }

    /**
     * Load an element from an array. Equivalent to {@code emitArrayLoad(Type)},
     * but element_type is implied. Must already have pushed the array and the
     * index (in that order): Stack: {@literal ..., array, index => ..., value}
     */
    public void emitArrayLoad()
    {
        popType(); // Pop index
        Type arrayType = popType().getImplementationType();
        Type elementType = ((ArrayType) arrayType).getComponentType();
        emitTypedOp(46, elementType);
        pushType(elementType);
    }

    /**
     * Invoke new on a class type. Does not call the constructor!
     * 
     * @param type
     *            the desired new object type
     */
    public void emitNew(ClassType type)
    {
        flushGoto();
        Label label = new Label(this);
        label.defineRaw(this);
        emitTypeInsn(NEW, type.getInternalName());
        pushType(new UninitializedType(type, label));
    }

    /**
     * Compile code to allocate a new array. The size should have been already
     * pushed on the stack.
     * 
     * @param element_type
     *            type of the array elements
     * @param dims
     *            number of dimensions - more than 1 is untested
     */
    public void emitNewArray(Type element_type, int dims)
    {
        flushGoto();
        if (popType().promote() != Type.intType)
            throw new Error("non-int dim. spec. in emitNewArray");

        if (element_type instanceof PrimType)
        {
            int code;
            switch (element_type.getSignature().charAt(0)) {
            case 'B':
                code = 8;
                break;
            case 'S':
                code = 9;
                break;
            case 'I':
                code = 10;
                break;
            case 'J':
                code = 11;
                break;
            case 'F':
                code = 6;
                break;
            case 'D':
                code = 7;
                break;
            case 'Z':
                code = 4;
                break;
            case 'C':
                code = 5;
                break;
            default:
                throw new Error("bad PrimType in emitNewArray");
            }
            emitNewArray(code);
        } else if (element_type instanceof ArrayType && dims > 1) // untested
        {
            emitMultiANewArrayInsn(new ArrayType(element_type).getInternalName(), dims);
            if (dims < 1 || dims > 255)
                throw new Error("dims out of range in emitNewArray");
            while (--dims > 0) // first dim already popped
                if (popType().promote() != Type.intType)
                    throw new Error("non-int dim. spec. in emitNewArray");
        } else if (element_type instanceof ObjectType)
        {
            emitTypeInsn(ANEWARRAY, ((ObjectType) element_type).getInternalName());
        } else
            throw new Error("unimplemented type in emitNewArray");

        pushType(new ArrayType(element_type));
    }

    public void emitNewArray(Type element_type)
    {
        emitNewArray(element_type, 1);
    }

    // We may want to deprecate this, because it depends on popType.
    private void emitBinop(int base_code)
    {
        Type type2 = popType().promote();
        Type type1_raw = popType();
        Type type1 = type1_raw.promote();
        if (type1 != type2 || !(type1 instanceof PrimType))
            throw new Error("non-matching or bad types in binary operation");
        emitTypedOp(base_code, type1);
        pushType(type1_raw);
    }

    private void emitBinop(int base_code, char sig)
    {
        popType();
        popType();
        emitTypedOp(base_code, sig);
        pushType(Type.signatureToPrimitive(sig));
    }

    public void emitBinop(int base_code, Type type)
    {
        popType();
        popType();
        emitTypedOp(base_code, type);
        pushType(type);
    }

    // public final void emitIntAdd () { put1(96); popType();}
    // public final void emitLongAdd () { put1(97); popType();}
    // public final void emitFloatAdd () { put1(98); popType();}
    // public final void emitDoubleAdd () { put1(99); popType();}

    public final void emitAdd(char sig)
    {
        emitBinop(96, sig);
    }

    public final void emitAdd(PrimType type)
    {
        emitBinop(96, type);
    }

    @Deprecated
    public final void emitAdd()
    {
        emitBinop(96);
    }

    public final void emitSub(char sig)
    {
        emitBinop(100, sig);
    }

    public final void emitSub(PrimType type)
    {
        emitBinop(100, type);
    }

    @Deprecated
    public final void emitSub()
    {
        emitBinop(100);
    }

    public final void emitMul()
    {
        emitBinop(104);
    }

    public final void emitDiv()
    {
        emitBinop(108);
    }

    public final void emitRem()
    {
        emitBinop(112);
    }

    public final void emitAnd()
    {
        emitBinop(126);
    }

    public final void emitIOr()
    {
        emitBinop(128);
    }

    public final void emitXOr()
    {
        emitBinop(130);
    }

    public final void emitShl()
    {
        emitShift(120);
    }

    public final void emitShr()
    {
        emitShift(122);
    }

    public final void emitUshr()
    {
        emitShift(124);
    }

    private void emitShift(int base_code)
    {
        Type type2 = popType().promote();
        Type type1_raw = popType();
        Type type1 = type1_raw.promote();

        if (type1 != Type.intType && type1 != Type.longType)
            throw new Error("the value shifted must be an int or a long");

        if (type2 != Type.intType)
            throw new Error("the amount of shift must be an int");

        emitTypedOp(base_code, type1);
        pushType(type1_raw);
    }

    /** Compile 'not', assuming 0 or 1 is on the JVM stack. */
    public final void emitNot(Type type)
    {
        emitPushConstant(1, type);
        emitAdd();
        emitPushConstant(1, type);
        emitAnd();
    }

    public void emitPrimop(int opcode, int arg_count, Type retType)
    {
        flushGoto();
        while (--arg_count >= 0)
            popType();
        emitInsn(opcode);
        pushType(retType);
    }

    /**
     * Compile code to push the contents of a local variable onto the statck.
     * 
     * @param var
     *            The variable whose contents we want to push.
     */
    public final void emitLoad(Variable var)
    {
        flushGoto();
        if (var.dead())
            throw new Error("attempting to push dead variable");
        int offset = var.offset;
        if (offset < 0 || !var.isSimple())
            throw new Error("attempting to load from unassigned variable " + var + " simple:"
                + var.isSimple() + ", offset: " + offset);
        Type type = var.getType().promote();
        int kind = adjustTypedOp(type);
        emitVarInsn(21 + kind, offset);
        pushType(var.getType());
    }

    public void emitStore(Variable var)
    {
        if (!reachableHere())
            return;
        flushGoto();
        int offset = var.offset;
        if (offset < 0 || !var.isSimple())
            throw new Error("attempting to store in unassigned " + var + " simple:" + var.isSimple()
                + ", offset: " + offset);
        Type type = var.getType().promote();
        noteVarType(offset, type);
        popType();
        int kind = adjustTypedOp(type);
        emitVarInsn(54 + kind, offset);
    }

    /**
     * Emit an instruction to increment a variable by some amount. If the
     * increment is zero, do nothing. The variable must contain an integral
     * value - except if increment is zero.
     */
    public void emitInc(Variable var, short inc)
    {
        flushGoto();
        if (var.dead())
            throw new Error("attempting to increment dead variable");
        int offset = var.offset;
        if (offset < 0 || !var.isSimple())
            throw new Error("attempting to increment unassigned variable" + var.getName()
                + " simple:" + var.isSimple() + ", offset: " + offset);

        if (inc == 0)
            return;

        if (var.getType().getImplementationType().promote() != Type.intType)
            throw new Error("attempting to increment non-int variable");

        emitIincInsn(var.offset, inc);
    }

    private final void emitFieldop(Field field, int opcode)
    {
        flushGoto();
        emitFieldInsn(opcode, field.getDeclaringClass().getInternalName(), field.getName(),
            field.getSignature());
    }

    /**
     * Compile code to get a static field value. Stack:
     * {@code ... => ..., value}
     */

    public final void emitGetStatic(Field field)
    {
        pushType(field.getType());
        emitFieldop(field, GETSTATIC);
    }

    /**
     * Compile code to get a non-static field value. Stack:
     * {@code ..., objectref => ..., value}
     */

    public final void emitGetField(Field field)
    {
        popType();
        pushType(field.getType());
        emitFieldop(field, GETFIELD);
    }

    /**
     * Compile code to put a static field value. Stack:
     * {@code ..., value => ...}
     */

    public final void emitPutStatic(Field field)
    {
        popType();
        emitFieldop(field, PUTSTATIC);
    }

    /**
     * Compile code to put a non-static field value. Stack: ..., objectref,
     * value => ...
     */

    public final void emitPutField(Field field)
    {
        popType();
        popType();
        emitFieldop(field, PUTFIELD);
    }

    public void emitInvokeMethod(Method method, int opcode)
    {
        if (!reachableHere())
            return;
        flushGoto();
        int arg_count = method.arg_types.length;
        boolean is_invokestatic = opcode == 184;
        boolean is_init = opcode == 183 && "<init>".equals(method.getName());

        if (is_invokestatic != ((method.access_flags & Access.STATIC) != 0))
            throw new Error(
                "emitInvokeXxx static flag mis-match method.flags=" + method.access_flags);
        if (!is_invokestatic && !is_init)
            arg_count++;
        emitMethodInsn(opcode, method.getDeclaringClass().getInternalName(), method.getName(),
            method.getSignature(), method.getDeclaringClass().isInterface());
        while (--arg_count >= 0)
        {
            Type t = popType();
            if (t instanceof UninitializedType)
                throw new Error("passing " + t + " as parameter");
        }
        if (is_init)
        {
            Type t = popType();
            ClassType ctype;
            if (!(t instanceof UninitializedType))
                throw new Error("calling <init> on already-initialized object");
            ctype = ((UninitializedType) t).ctype;
            getMethod().classfile.addToTypeMap(ctype);
            for (int i = 0; i < SP; i++)
                if (stack_types[i] == t)
                    stack_types[i] = ctype;
            Variable[] used = locals.used;
            for (int i = used == null ? 0 : used.length; --i >= 0;)
            {
                Variable var = used[i];
                if (var != null && var.getType() == t)
                    var.setType(ctype);
            }
            for (int i = local_types == null ? 0 : local_types.length; --i >= 0;)
            {
                if (local_types[i] == t)
                    local_types[i] = ctype;
            }
        }
        if (method.return_type.size != 0)
            pushType(method.return_type);
    }

    public void emitInvoke(Method method)
    {
        int opcode;
        if ((method.access_flags & Access.STATIC) != 0)
            opcode = 184; // invokestatic
        else if (method.classfile.isInterface())
            opcode = 185; // invokeinterface
        else if ("<init>".equals(method.getName()) || (method.access_flags & Access.PRIVATE) != 0)
            opcode = 183; // invokespecial
        else
            opcode = 182; // invokevirtual
        emitInvokeMethod(method, opcode);
    }

    /**
     * Compile a virtual method call. The stack contains the 'this' object,
     * followed by the arguments in order.
     * 
     * @param method
     *            the method to invoke virtually
     */
    public void emitInvokeVirtual(Method method)
    {
        emitInvokeMethod(method, 182); // invokevirtual
    }

    public void emitInvokeSpecial(Method method)
    {
        emitInvokeMethod(method, 183); // invokespecial
    }

    /**
     * Compile a static method call. The stack contains the the arguments in
     * order.
     * 
     * @param method
     *            the static method to invoke
     */
    public void emitInvokeStatic(Method method)
    {
        emitInvokeMethod(method, 184); // invokestatic
    }

    public void emitInvokeInterface(Method method)
    {
        emitInvokeMethod(method, 185); // invokeinterface
    }

    final void emitTransfer(Label label, int opcode)
    {
        flushGoto();
        label.setTypes(this);
        fixupAdd(label);
        emitJumpInsn(opcode, label.asmLabel);
    }

    /**
     * Compile an unconditional branch (goto).
     * 
     * @param label
     *            target of the branch (must be in this method).
     */
    public final void emitGoto(Label label)
    {
        flushGoto();
        label.setTypes(this);
        fixupAdd(label);
        setUnreachable();
        if (label.defined())
            emitJumpInsn(GOTO, label.asmLabel);
        else
            prevGoto = label;
    }

    public final void emitJsr(Label label)
    {
        flushGoto();
        fixupAdd(label);
        emitJumpInsn(JSR, label.asmLabel);
    }

    ExitableBlock currentExitableBlock;
    int exitableBlockLevel;

    /**
     * Enter a block which can be exited. Used to make sure finally-blocks are
     * executed when exiting a block, loop, or method.
     */
    public ExitableBlock startExitableBlock(Type resultType, boolean runFinallyBlocks)
    {
        ExitableBlock bl = new ExitableBlock(resultType, this, runFinallyBlocks);
        bl.outer = currentExitableBlock;
        currentExitableBlock = bl;
        return bl;
    }

    /**
     * End a block entered by a previous startExitableBlock.
     */
    public void endExitableBlock()
    {
        ExitableBlock bl = currentExitableBlock;
        bl.finish();
        currentExitableBlock = bl.outer;
    }

    public final void emitGotoIfCompare1(Label label, int opcode)
    {
        popType();
        emitTransfer(label, opcode);
    }

    public final void emitGotoIfIntEqZero(Label label)
    {
        emitGotoIfCompare1(label, 153);
    }

    public final void emitGotoIfIntNeZero(Label label)
    {
        emitGotoIfCompare1(label, 154);
    }

    public final void emitGotoIfIntLtZero(Label label)
    {
        emitGotoIfCompare1(label, 155);
    }

    public final void emitGotoIfIntGeZero(Label label)
    {
        emitGotoIfCompare1(label, 156);
    }

    public final void emitGotoIfIntGtZero(Label label)
    {
        emitGotoIfCompare1(label, 157);
    }

    public final void emitGotoIfIntLeZero(Label label)
    {
        emitGotoIfCompare1(label, 158);
    }

    public final void emitGotoIfNull(Label label)
    {
        emitGotoIfCompare1(label, 198);
    }

    public final void emitGotoIfNonNull(Label label)
    {
        emitGotoIfCompare1(label, 199);
    }

    public final void emitGotoIfCompare2(Label label, int logop)
    {
        flushGoto();
        Type type2 = popType().promote();
        Type type1 = popType().promote();
        char sig1 = type1.getSignature().charAt(0);
        char sig2 = type2.getSignature().charAt(0);

        boolean cmpg = (logop == 155 || logop == 158); // iflt,ifle

        if (sig1 == 'I' && sig2 == 'I')
            logop += 6; // iflt -> if_icmplt etc.
        else if (sig1 == 'J' && sig2 == 'J')
            emitInsn(LCMP);
        else if (sig1 == 'F' && sig2 == 'F')
            emitInsn(cmpg ? FCMPL : FCMPG);
        else if (sig1 == 'D' && sig2 == 'D')
            emitInsn(cmpg ? DCMPL : DCMPG);
        else if ((sig1 == 'L' || sig1 == '[') && (sig2 == 'L' || sig2 == '[') && logop <= 154)
            logop += 12; // ifeq->if_acmpeq, ifne->if_acmpne
        else
            throw new Error("invalid types to emitGotoIfCompare2");

        emitTransfer(label, logop);
    }

    // binary comparisons
    @Deprecated
    public final void emitGotoIfEq(Label label, boolean invert)
    {
        emitGotoIfCompare2(label, invert ? 154 : 153);
    }

    /** Compile a conditional transfer if 2 top stack elements are equal. */
    public final void emitGotoIfEq(Label label)
    {
        emitGotoIfCompare2(label, 153);
    }

    /** Compile conditional transfer if 2 top stack elements are not equal. */
    public final void emitGotoIfNE(Label label)
    {
        emitGotoIfCompare2(label, 154);
    }

    public final void emitGotoIfLt(Label label)
    {
        emitGotoIfCompare2(label, 155);
    }

    public final void emitGotoIfGe(Label label)
    {
        emitGotoIfCompare2(label, 156);
    }

    public final void emitGotoIfGt(Label label)
    {
        emitGotoIfCompare2(label, 157);
    }

    public final void emitGotoIfLe(Label label)
    {
        emitGotoIfCompare2(label, 158);
    }

    /**
     * Compile start of a conditional:
     * <tt>if (!(<var>x</var> opcode 0)) ...</tt>. The value of <var>x</var>
     * must already have been pushed.
     */
    public final void emitIfCompare1(int opcode)
    {
        if (popType().promote() != Type.intType)
            throw new Error("non-int type to emitIfCompare1");
        emitTransfer(emitIfRaw(), opcode);
    }

    /**
     * Compile start of conditional: <tt>if (x != 0) ...</tt>. Also use this if
     * you have pushed a boolean value: if (b) ...
     */
    public final void emitIfIntNotZero()
    {
        emitIfCompare1(153); // ifeq
    }

    /**
     * Compile start of conditional: <tt>if (x == 0) ...</tt>. Also use this if
     * you have pushed a boolean value: if (!b) ...
     */
    public final void emitIfIntEqZero()
    {
        emitIfCompare1(154); // ifne
    }

    /** Compile start of conditional: {@code if (x <= 0)}. */
    public final void emitIfIntLEqZero()
    {
        emitIfCompare1(157); // ifgt
    }

    /** Compile start of conditional: {@code if (x >= 0)}. */
    public final void emitIfIntGEqZero()
    {
        emitIfCompare1(155); // iflt
    }

    /**
     * Compile start of a conditional: {@code if (!(x opcode null)) ...}. The
     * value of <tt>x</tt> must already have been pushed and must be of
     * reference type.
     */
    public final void emitIfRefCompare1(int opcode)
    {
        if (!(popType() instanceof ObjectType))
            throw new Error("non-ref type to emitIfRefCompare1");
        emitTransfer(emitIfRaw(), opcode);
    }

    /** Compile start of conditional: {@code if (x != null) ...}. */
    public final void emitIfNotNull()
    {
        emitIfRefCompare1(198); // ifnull
    }

    /** Compile start of conditional: {@code if (x == null) ...} */
    public final void emitIfNull()
    {
        emitIfRefCompare1(199); // ifnonnull
    }

    /**
     * Compile start of a conditional: {@code if (!(x OPCODE y)) ...} The value
     * of x and y must already have been pushed.
     */
    public final void emitIfIntCompare(int opcode)
    {
        popType();
        popType();
        emitTransfer(emitIfRaw(), opcode);
    }

    /* Compile start of a conditional: {@code if (x < y) ...} */
    public final void emitIfIntLt()
    {
        emitIfIntCompare(162); // if_icmpge
    }

    /* Compile start of a conditional: {@code if (x >= y) ...} */
    public final void emitIfIntGEq()
    {
        emitIfIntCompare(161); // if_icmplt
    }

    /**
     * Compile start of a conditional: if (x != y) ... The values of x and y
     * must already have been pushed.
     */
    public final void emitIfNEq()
    {
        emitGotoIfEq(emitIfRaw());
    }

    /**
     * Compile start of a conditional: {@code if (x == y) ...} The values of x
     * and y must already have been pushed.
     */
    public final void emitIfEq()
    {
        emitGotoIfNE(emitIfRaw());
    }

    /**
     * Compile start of a conditional: {@code if (x < y) ...} The values of x
     * and y must already have been pushed.
     */
    public final void emitIfLt()
    {
        emitGotoIfGe(emitIfRaw());
    }

    /**
     * Compile start of a conditional: {@code if (x >= y) ...} The values of x
     * and y must already have been pushed.
     */
    public final void emitIfGe()
    {
        emitGotoIfLt(emitIfRaw());
    }

    /**
     * Compile start of a conditional: {@code if (x > y) ...} The values of x
     * and y must already have been pushed.
     */
    public final void emitIfGt()
    {
        emitGotoIfLe(emitIfRaw());
    }

    /**
     * Compile start of a conditional: {@code if (x <= y) ...} The values of x
     * and y must already have been pushed.
     */
    public final void emitIfLe()
    {
        emitGotoIfGt(emitIfRaw());
    }

    /**
     * Emit a 'ret' instruction.
     * 
     * @param var
     *            the variable containing the return address
     */
    public void emitRet(Variable var)
    {
        flushGoto();
        emitVarInsn(RET, var.offset);
    }

    public final void emitThen()
    {
    }

    public final void emitIfThen()
    {
        new IfState(this, null);
    }

    /** Compile start of else clause. */
    public final void emitElse()
    {
        Label else_label = if_stack.end_label;
        if (reachableHere())
        {
            Label end_label = new Label(this);
            if_stack.end_label = end_label;
            emitGoto(end_label);
        } else
            if_stack.end_label = null;
        if (else_label != null)
            else_label.define(this);
        if_stack.doing_else = true;
    }

    /** Compile end of conditional. */
    public final void emitFi()
    {
        if (if_stack.end_label != null)
            if_stack.end_label.define(this);
        // Pop the if_stack.
        if_stack = if_stack.previous;
    }

    /** Convenience for compiling {@code if P1 && P2 then S1 else S2}.
     * Compile that as:
     * <pre>
     * compile P1, including an appropriate emitIfXxx
     * emitAndThen()
     * compile P2, including an appropriate emitIfXxx
     * compile S1
     * emitElse
     * compile S2
     * emitFi
     * </pre>
     */
    public void emitAndThen()
    {
        if (if_stack == null || if_stack.andThenSet)
            throw new InternalError();
        if_stack.andThenSet = true;
    }

    /**
     * Start a new if/then/else block. The caller is responsible for evaluating
     * the condition, and in the "false" case got the returned label. In the
     * "true" case just continue inline.
     * 
     * @return the else/end label to goto if the condition is false. A
     *         subsequent emitElse or emitFi defines the label.
     */
    public Label emitIfRaw()
    {
        if (if_stack != null && if_stack.andThenSet)
        {
            if_stack.andThenSet = false;
        } else
            new IfState(this);
        return if_stack.end_label;
    }

    public final void fixUnsigned(Type stackType)
    {
        flushGoto();
        if (stackType instanceof PrimType && ((PrimType) stackType).isUnsigned())
        {
            char sig1 = stackType.getSignature().charAt(0);
            if (sig1 == 'S')
            {
                emitInsn(I2C);
            } else if (sig1 == 'B')
            {
                emitPushInt(255);
                emitAnd();
            }
        }
    }

    public final void emitConvert(PrimType from, PrimType to)
    {
        flushGoto();
        String to_sig = to.getSignature();
        String from_sig = from.getSignature();
        int op = -1;
        char to_sig0 = to_sig.charAt(0);
        char from_sig0 = from_sig.charAt(0);
        if (from_sig0 == to_sig0)
            return;
        if (from.size < 4)
            from_sig0 = 'I';
        if (to.size < 4)
        {
            emitConvert(from, Type.intType);
            from_sig0 = 'I';
            if (to.isUnsigned())
            {
                if (to_sig0 == 'S')
                    to_sig0 = 'C';
                else if (to_sig0 == 'B')
                {
                    emitPushInt(0xff);
                    emitAnd();
                    return;
                }
            }
        }
        if (from_sig0 == 'J' && from.isUnsigned() && (to_sig0 == 'F' || to_sig0 == 'D'))
        {
            emitPushInt(1);
            emitUshr();
            emitConvert(Type.longType, to);
            emitPushConstant(2, to);
            emitMul();
            return;
        }
        if (from_sig0 == 'I' && from.isUnsigned()
            && (to_sig0 == 'J' || to_sig0 == 'F' || to_sig0 == 'D'))
        {
            emitConvert(Type.intType, Type.longType);
            emitPushLong(0xffffffffL);
            emitAnd();
            from_sig0 = 'J';
        }
        if (from_sig0 == to_sig0)
            return;
        switch (from_sig0) {
        case 'I':
            switch (to_sig0) {
            case 'B':
                op = 145;
                break; // i2b
            case 'C':
                op = 146;
                break; // i2c
            case 'S':
                op = 147;
                break; // i2s
            case 'J':
                op = 133;
                break; // i2l
            case 'F':
                op = 134;
                break; // i2f
            case 'D':
                op = 135;
                break; // i2d
            }
            break;
        case 'J':
            switch (to_sig0) {
            case 'I':
                op = 136;
                break; // l2i
            case 'F':
                op = 137;
                break; // l2f
            case 'D':
                op = 138;
                break; // l2d
            }
            break;
        case 'F':
            switch (to_sig0) {
            case 'I':
                op = 139;
                break; // f2i
            case 'J':
                op = 140;
                break; // f2l
            case 'D':
                op = 141;
                break; // f2d
            }
            break;
        case 'D':
            switch (to_sig0) {
            case 'I':
                op = 142;
                break; // d2i
            case 'J':
                op = 143;
                break; // d2l
            case 'F':
                op = 144;
                break; // d2f
            }
            break;
        }
        if (op < 0)
            throw new Error("unsupported CodeAttr.emitConvert");
        popType();
        emitInsn(op);
        pushType(to);
    }

    private void emitCheckcast(Type type, int opcode)
    {
        flushGoto();
        popType();
        if (!(type instanceof ObjectType))
            throw new Error("unimplemented type " + type + " in emitCheckcast/emitInstanceof");
        emitTypeInsn(opcode, ((ObjectType) type).getInternalName());
    }

    public static boolean castNeeded(Type top, Type required)
    {
        top = top.getRawType();
        for (;;)
        {
            if (top == required)
                return false;
            if (required instanceof ClassType && top instanceof ClassType
                && ((ClassType) top).isSubclass((ClassType) required))
                return false;
            else if (top instanceof ArrayType && required == Type.objectType)
                return false;
            else if (required instanceof ArrayType && top instanceof ArrayType)
            {
                required = ((ArrayType) required).getComponentType();
                top = ((ArrayType) top).getComponentType();
                continue;
            }
            return true;
        }
    }

    public void emitCheckcast(Type type)
    {
        if (castNeeded(topType(), type))
        {
            emitCheckcast(type, CHECKCAST);
            pushType(type);
        }
    }

    public void emitInstanceof(Type type)
    {
        emitCheckcast(type, INSTANCEOF);
        pushType(Type.booleanType);
    }

    public final void emitThrow()
    {
        flushGoto();
        popType();
        emitInsn(ATHROW);
        setUnreachable();
    }

    public final void emitMonitorEnter()
    {
        flushGoto();
        popType();
        emitInsn(MONITORENTER);
    }

    public final void emitMonitorExit()
    {
        flushGoto();
        popType();
        emitInsn(MONITOREXIT);
    }

    /**
     * Compile a method return. If inside a 'catch' clause, first call 'finally'
     * clauses. The return value (unless the return type is void) must be on the
     * stack, and have the correct type.
     */
    public final void emitReturn()
    {
        if (try_stack != null)
            new Error();
        emitRawReturn();
    }

    final void emitRawReturn()
    {
        if (!reachableHere())
            return;
        flushGoto();
        if (getMethod().getReturnType().size == 0)
        {
            emitInsn(RETURN);
        } else
            emitTypedOp(172, popType().promote());
        setUnreachable();
    }

    /**
     * Add an exception handler. Low-level routine; {@link #emitCatchStart} is
     * preferred.
     */
    public void addHandler(Label start_try, Label end_try, ClassType catch_type)
    {
        fixupAdd(end_try);
        Label handler = new Label();
        fixupAdd(handler);
        handler.localTypes = start_try.localTypes;
        handler.stackTypes = new Type[1];
        ClassType handler_class = catch_type == null ? Type.javalangThrowableType : catch_type;
        handler.stackTypes[0] = handler_class;
        setTypes(handler);
        setReachable(true);
        handler.defineRaw(this);
        mvisitor.visitTryCatchBlock(start_try.asmLabel, end_try.asmLabel, handler.asmLabel,
            handler_class.getInternalName());
    }

    /**
     * Beginning of code that has a cleanup handler. This is similar to a
     * try-finally, but the cleanup is only done in the case of an exception.
     * Alternatively, the try clause has to manually do the cleanup with code
     * duplication. Equivalent to:
     * <code>try <var>body</var> catch (Throwable ex) { <var>cleanup</var>; throw ex; }</code>
     * Call <code>emitWithCleanupStart</code> before the
     * <code><var>body</var></code>.
     */
    public void emitWithCleanupStart()
    {
        int savedSP = SP;
        SP = 0; // Hack to disable emitTryStart needlessly saving the stack.
        emitTryStart(false, null);
        SP = savedSP;
    }

    /**
     * Called after a <code><var>body</var></code> that has a
     * <code><var>cleanup</var></code> clause. Followed by the
     * <code><var>cleanup</var></code> code.
     */
    public void emitWithCleanupCatch(Variable catchVar)
    {
        emitTryEnd(false);
        try_stack.saved_result = catchVar;
        emitCatchStart(catchVar);
    }

    /** Called after generating a <code><var>cleanup</var></code> handler. */

    public void emitWithCleanupDone()
    {
        Variable catchVar = try_stack.saved_result;
        try_stack.saved_result = null;
        if (catchVar != null)
            emitLoad(catchVar);
        emitThrow();
        emitCatchEnd();
        emitTryCatchEnd();
    }

    public void emitTryStart(boolean has_finally, Type result_type)
    {
        if (result_type != null && result_type.isVoid())
            result_type = null;
        Variable[] savedStack = null;
        if (result_type != null || SP > 0)
            pushScope();
        if (SP > 0)
        {
            savedStack = new Variable[SP];
            int i = 0;
            while (SP > 0)
            {
                Variable var = addLocal(topType());
                emitStore(var);
                savedStack[i++] = var;
            }
        }
        TryState try_state = new TryState(this);
        try_state.savedStack = savedStack;

        int usedLocals = local_types == null ? 0 : local_types.length;
        for (; usedLocals > 0; usedLocals--)
        {
            Type last = local_types[usedLocals - 1];
            if (last != null)
                break;
        }

        Type[] startLocals;
        if (usedLocals == 0)
            startLocals = Type.typeArray0;
        else
        {
            startLocals = new Type[usedLocals];
            System.arraycopy(local_types, 0, startLocals, 0, usedLocals);
        }
        try_state.start_try.localTypes = startLocals;

        if (result_type != null)
            try_state.saved_result = addLocal(result_type);
        if (has_finally)
            try_state.finally_subr = new Label();
    }

    @Deprecated
    public void emitTryEnd()
    {
    }

    private void emitTryEnd(boolean fromFinally)
    {
        if (try_stack.tryClauseDone)
            return;
        try_stack.tryClauseDone = true;
        if (try_stack.finally_subr != null)
            try_stack.exception = addLocal(Type.javalangThrowableType);
        gotoFinallyOrEnd(fromFinally);
        try_stack.end_try = getLabel();
    }

    public void emitCatchStart(Variable var)
    {
        if (var == null)
            emitCatchStart((ClassType) null);
        else
        {
            emitCatchStart((ClassType) var.getType());
            emitStore(var);
        }
    }

    public void emitCatchStart(ClassType type)
    {
        emitTryEnd(false);
        setTypes(try_stack.start_try.localTypes, Type.typeArray0);
        if (try_stack.try_type != null)
            emitCatchEnd();
        try_stack.try_type = type;
        addHandler(try_stack.start_try, try_stack.end_try, type);
        setReachable(true);
    }

    public void emitCatchEnd()
    {
        gotoFinallyOrEnd(false);
        try_stack.try_type = null;
    }

    private void gotoFinallyOrEnd(boolean fromFinally)
    {
        if (reachableHere())
        {
            if (try_stack.saved_result != null)
                emitStore(try_stack.saved_result);
            if (try_stack.end_label == null)
                try_stack.end_label = new Label();
            if (try_stack.finally_subr == null || useJsr())
            {
                if (try_stack.finally_subr != null)
                    emitJsr(try_stack.finally_subr);
                emitGoto(try_stack.end_label);
            } else
            {
                if (try_stack.exitCases != null)
                    emitPushInt(0);
                emitPushNull(); // No caught Throwable.
                if (!fromFinally)
                    emitGoto(try_stack.finally_subr);
            }
        }
    }

    public void emitFinallyStart()
    {
        emitTryEnd(true);
        if (try_stack.try_type != null)
            emitCatchEnd();
        try_stack.end_try = getLabel();

        pushScope();
        if (useJsr())
        {
            SP = 0;
            emitCatchStart((ClassType) null);
            emitStore(try_stack.exception);
            emitJsr(try_stack.finally_subr);
            emitLoad(try_stack.exception);
            emitThrow();
        } else
        {
            if (reachableHere())
                emitGoto(try_stack.finally_subr);
            addHandler(try_stack.start_try, try_stack.end_try, Type.javalangThrowableType);
            if (try_stack.saved_result != null)
                emitStoreDefaultValue(try_stack.saved_result);
            if (try_stack.exitCases != null)
            {
                emitPushInt(-1); // Return switch case code.
                emitSwap();
            }
        }
        try_stack.finally_subr.define(this);

        if (useJsr())
        {
            Type ret_addr_type = Type.objectType;
            try_stack.finally_ret_addr = addLocal(ret_addr_type);
            pushType(ret_addr_type);
            emitStore(try_stack.finally_ret_addr);
        } else
        {
            // Stack contents at the start of the finally block:
            // an integer exit code, but only if (exitCases != null).
            // a Throwable or null
        }
    }

    public void emitFinallyEnd()
    {
        if (!reachableHere())
            try_stack.end_label = null;
        else if (useJsr())
            emitRet(try_stack.finally_ret_addr);
        else if (try_stack.end_label == null && try_stack.exitCases == null)
        {
            emitThrow();
        } else
        {
            emitStore(try_stack.exception);
            emitLoad(try_stack.exception);
            emitIfNotNull();
            emitLoad(try_stack.exception);
            emitThrow();
            emitElse();

            ExitableBlock exit = try_stack.exitCases;

            if (exit != null)
            {
                SwitchState sw = startSwitch();

                while (exit != null)
                {
                    ExitableBlock next = exit.nextCase;
                    TryState nextTry = TryState.outerHandler(try_stack.previous,
                        exit.initialTryState);
                    if (nextTry == exit.initialTryState) // Optimization
                    {
                        sw.addCaseGotoOnly(exit.switchCase, this, exit.endLabel);
                    } else
                    {
                        sw.addCaseOnly(exit.switchCase, this);
                    }
                    exit = next;
                }

                exit = try_stack.exitCases;
                while (exit != null)
                {
                    ExitableBlock next = exit.nextCase;
                    exit.nextCase = null;
                    exit.currentTryState = null;
                    TryState nextTry = TryState.outerHandler(try_stack.previous,
                        exit.initialTryState);
                    if (nextTry == exit.initialTryState) // Optimization
                    {
                        sw.addCaseGoto(exit.switchCase, this, exit.endLabel);
                    } else
                    {
                        sw.addCase(exit.switchCase, this);
                        exit.exit(nextTry);
                    }
                    exit = next;
                }
                try_stack.exitCases = null;

                sw.addDefault(this);
                sw.finish(this);
            }
            emitFi();

            setUnreachable();
        }
        popScope();
        try_stack.finally_subr = null;
    }

    public void emitTryCatchEnd()
    {
        if (try_stack.finally_subr != null)
            emitFinallyEnd();
        Variable[] vars = try_stack.savedStack;
        if (try_stack.end_label == null)
            setUnreachable();
        else
        {
            setTypes(try_stack.start_try.localTypes, Type.typeArray0);
            try_stack.end_label.define(this);
            if (vars != null)
            {
                for (int i = vars.length; --i >= 0;)
                {
                    Variable v = vars[i];
                    if (v != null)
                    {
                        emitLoad(v);
                    }
                }
            }
            if (try_stack.saved_result != null)
                emitLoad(try_stack.saved_result);
        }
        if (try_stack.saved_result != null || vars != null)
            popScope();
        try_stack = try_stack.previous;
    }

    public final TryState getCurrentTry()
    {
        return try_stack;
    }

    public final boolean isInTry()
    {
        // This also return true if we're in a catch clause, but that is
        // good enough for now.
        return try_stack != null;
    }

    /**
     * Start a new switch statment or expression. The switch value must have
     * been calculated and left on the stack.
     */
    public SwitchState startSwitch()
    {
        SwitchState sw = new SwitchState(this);
        sw.switchValuePushed(this);
        return sw;
    }

    /**
     * Compile a tail-call to position 0 of the current procedure.
     * 
     * @param pop_args
     *            if true, copy argument registers (except this) from stack.
     * @param start
     *            the Label to jump back to.
     */
    public void emitTailCall(boolean pop_args, Label start)
    {
        if (pop_args)
        {
            Method meth = getMethod();
            int arg_slots = ((meth.access_flags & Access.STATIC) != 0) ? 0 : 1;
            for (int i = meth.arg_types.length; --i >= 0;)
                arg_slots += meth.arg_types[i].size > 4 ? 2 : 1;
            for (int i = meth.arg_types.length; --i >= 0;)
            {
                arg_slots -= meth.arg_types[i].size > 4 ? 2 : 1;
                emitStore(locals.used[arg_slots]);
            }
        }
        emitGoto(start);
    }

    /**
     * Compile a tail-call to position 0 of the current procedure.
     * 
     * @param pop_args
     *            if true, copy argument registers (except this) from stack.
     * @param scope
     *            Scope whose start we jump back to.
     */
    public void emitTailCall(boolean pop_args, Scope scope)
    {
        emitTailCall(pop_args, scope.start);
    }

    void fixupAdd(Label l)
    {
        if (l != null)
            l.needsStackMapEntry = true;
    }

    /**
     * This causes a later processFixup to rearrange the code. The code at
     * target comes here, instead of the following instructions. Fuctionally
     * equivalent to: <code>goto target; here:</code>, but implemented by code
     * re-arranging. Therefore there should be at some later point a
     * <code>goto here; target:</code>.
     */
    public void fixupChain(Label here, Label target)
    {
        fixupAdd(target);
        emitGoto(target);
        here.define(this);
    }

    public int beginFragment(Label after)
    {
        return beginFragment(new Label(), after);
    }

    java.util.ArrayList<Label> fragmentAfter = new java.util.ArrayList<Label>();

    public int beginFragment(Label start, Label after)
    {
        emitGoto(after);
        start.define(this);
        fragmentAfter.add(after);
        return fragmentAfter.size() - 1;
    }

    /**
     * End a fragment.
     * 
     * @param cookie
     *            the return value from the previous beginFragment.
     */
    public void endFragment(int cookie)
    {
        fragmentAfter.get(cookie).define(this);
    }

    java.util.List<Fragment> fragments = new java.util.ArrayList<Fragment>();

    public void addFragment(Fragment f)
    {
        fragments.add(f);
    }
}
