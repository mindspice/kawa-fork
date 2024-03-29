// Copyright (c) 1999, 2000, 2001, 2002, 2003, 2004, 2007, 2008, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.lists.LList;
import gnu.kawa.functions.Convert;
import gnu.kawa.io.OutPort;
import gnu.kawa.lispexpr.LangObjType;
import java.util.*;
import java.lang.annotation.ElementType;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

/**
 * Class used to implement Scheme lambda expressions.
 * @author	Per Bothner
 */

public class LambdaExp extends ScopeExp {
    public Expression body;

    /** Minimum number of actual arguments.
     * Does not count implicit isThisParameter(). */
    public int min_args;

    /** Maximum number of actual arguments;  -1 if variable.
     * Does not count keyword arguments. */
    public int max_args;

    /** Number of optional arguments, not counting keyword arguments. */
    public int opt_args;

    /** Set of visible top-level LambdaExps that need apply methods. */
    ArrayList<LambdaExp> applyMethods;

    public Keyword[] keywords;

    /** A list of Declarations, chained using Declaration's nextCapturedVar.
     * All the Declarations are allocated in the current heapFrame. */
    Declaration capturedVars;

    /** Linked link of references to sibling declarations.
     * Chained using {@see ReferenceExp#siblingReferencesNext} links.
     * I.e. References to declarations external to this LambdaExp,
     * but local to the outer LambdaExp.
     */
    ReferenceExp siblingReferences;

    /** The location for function start, before arguments are stored. */
    Label startForInlining;
    /** Queue of deferred inline class.
     * The items are (deferred-lambda, Target) pairs.
     */
    LinkedList<Object> pendingInlines;

    public void capture(Declaration decl) {
        if (decl.isSimple()) {
            if (capturedVars == null
                && ! decl.isStatic()
                && ! isClassGenerated()) {
                heapFrame = new gnu.bytecode.Variable("$heapFrame");
            }
            decl.setSimple(false);
            if (! decl.isPublic()) {
                decl.nextCapturedVar = capturedVars;
                capturedVars = decl;
            }
        }
    }

    public Declaration addParameter(Object name) {
        min_args++;
        max_args++;
        return super.addDeclaration(name);
    }

    /** A local variable that points to the heap-allocated part of the frame.
     * Each captured variable is a field in the heapFrame.  A procedure has
     * a heapFrame iff if has a parameter or local variable that is
     * referenced ("captured") by a non-inline inferior procedure.
     * (I.e there is a least one non-inline procedure that encloses the
     * reference but not the definition.)  Note that an inline procedure may
     * have a heapFrame if it encloses a non-inline procedure.  This is
     * necessary because we represent loops as tail-recursive inline procedures.
     */
    Variable heapFrame;

    public LambdaExp firstChild;
    public LambdaExp nextSibling;

    /** A magic value to indicate there is no unique return continuation. */
    final static ApplyExp unknownContinuation
        = new ApplyExp ((Expression) null, (Expression[]) null);

    /** The unique call site that calls this lambda.
     * The value is null if no callers have been seen.
     * A value of unknownContinuation means there are multiple call sites.
     * Tail-recursive calls do not count as multiple call sites.
     * This is used to see if we can inline the function at its unique call site.
     * Usually this is an ApplyExp, but it can also be the "tail position"
     * for some outer expression, such as an IfExp.  This allows inlining f
     * in the call 'if (cond) f(x) else f(y)' since both calls have the same
     * return point.
     */
    public Expression returnContinuation;

    /** If non-null, set of functions that tail-call this function. */
    java.util.Set<LambdaExp> tailCallers;

    /** If this lambda gets inlined this is the containing lambda.
        Otherwise this is null. */
    public LambdaExp inlineHome;

    /** Expressions that name classes that may be thrown. */
    Expression[] throwsSpecification;

    public void setExceptions(Expression[] exceptions) {
        throwsSpecification = exceptions;
    }

    /** If non-null, a Declaration whose value is (only) this LambdaExp. */
    public Declaration nameDecl;

    public static final String CLOSURE_ENV_NAME = "$closureEnv";

    /** If non-null, this is a Field that is used for implementing lexical closures.
     * If getName() is CLOSURE_ENV_NAME, it is our parent's heapFrame,
     * which is an instance of one of our siblings.
     * (Otherwise, we use "this" as the implicit "closureEnv" field.) */
    public Field closureEnvField;

    /** Field in heapFrame.getType() that contains the static link.
     * It is used by child functions to get to outer environments.
     * Its value is this function's closureEnv value. */
    public Field staticLinkField;

    /** A variable that points to the closure environment passed in.
     * It can be any one of:
     * null, if no closure environment is needed;
     * this, if this object is its parent's heapFrame;
     * a local variable initialized from this.closureEnv;
     * a parameter (only if !getCanRead()); or
     * a copy of our caller's closureEnv or heapFrame (only if getInlineOnly()).
     * See declareClosureEnv and closureEnvField. */
    Variable closureEnv;

    static final int CAN_READ = Expression.NEXT_AVAIL_FLAG; // 2
    static final int INLINE_ONLY = 4;
    static final int IMPORTS_LEX_VARS = 8;
    static final int NEEDS_STATIC_LINK = 16;
    /* Used (future) by FindTailCalls. */
    static final int CANNOT_INLINE = 32;
    static final int CLASS_METHOD = 64;
    static final int METHODS_COMPILED = 128;
    public static final int NO_FIELD = 256;
    /** True if any parameter default expression captures a parameter. */
    static final int DEFAULT_CAPTURES_ARG = 512;
    public static final int SEQUENCE_RESULT = 1024;
    public static final int OVERLOADABLE_FIELD = 2048;
    public static final int ATTEMPT_INLINE = 4096;
    public static final int IN_EXPWALKER = 0x2000;
    /** Treat as inlined in outer lambda when determining tailcalls. */
    public static final int PASSES_TAILCALLS = 0x4000;

    /** True of emitted method should be public.
     * Needed if PrimProcedure.getMethodFor shold be able to find it.
     */
    public static final int PUBLIC_METHOD = 0x4000;
    public static final int ALLOW_OTHER_KEYWORDS = 0x8000;

    protected static final int HAS_NONTRIVIAL_PATTERN = 0x10000;
    /** True if IS_SUPPLIED_PARAMETER is set on a parameter.
     * In that case we generate an extra foo$P method for the actual body.
     * (It might make sense to also do this whenever a default expression
     * is non-literal, but we current don't do that.)
     */
    protected static final int HAS_NONTRIVIAL_DEFAULT = 0x20000;
    protected static final int NEXT_AVAIL_FLAG = 0x40000;

    /** True iff this lambda is only "called" inline. */
    public final boolean getInlineOnly() { return (flags & INLINE_ONLY) != 0; }
    public final void setInlineOnly(boolean inlineOnly)
    { setFlag(inlineOnly, INLINE_ONLY); }

    public final boolean inlinedInCheckMethod() {
        return (flags & HAS_NONTRIVIAL_PATTERN) != 0;
    }

    /** True if no primitive method is created for this procedure.
        Specifically if {@code getInlineOnly() || inlinedInCheckMethod()} .*/
    public boolean inlinedInCallerOrCheckMethodOnly() {
        return (flags & (INLINE_ONLY|HAS_NONTRIVIAL_PATTERN)) != 0;
    }

    /** Note this function is inlined in a give context.
     * This is meant to be used during validate-apply processing,
     * for procedures that will be inlined in a compile method.
     */
    public final void setInlineOnly(Expression returnContinuation,
                                    LambdaExp caller) {
        setInlineOnly(true);
        this.returnContinuation = returnContinuation;
        this.inlineHome = caller;
    }

    public final boolean getNeedsClosureEnv() {
        return (flags & (NEEDS_STATIC_LINK|IMPORTS_LEX_VARS)) != 0;
    }

    /** True if a child lambda uses lexical variables from outside.
        Hence, a child heapFrame needs a staticLink to outer frames. */
    public final boolean getNeedsStaticLink()
    { return (flags & NEEDS_STATIC_LINK) != 0; }

    public final void setNeedsStaticLink(boolean needsStaticLink) {
        if (needsStaticLink) flags |= NEEDS_STATIC_LINK;
        else flags &= ~NEEDS_STATIC_LINK;
    }

    /** True iff this lambda "captures" (uses) lexical variables from outside. */
    public final boolean getImportsLexVars() {
        return (flags & IMPORTS_LEX_VARS) != 0;
    }

    public final void setImportsLexVars(boolean importsLexVars) {
        if (importsLexVars) flags |= IMPORTS_LEX_VARS;
        else flags &= ~IMPORTS_LEX_VARS;
    }

    public final void setImportsLexVars() {
        int old = flags;
        flags |= IMPORTS_LEX_VARS;

        // If this needs an environment (closure), then its callers do too.
        if ((old & IMPORTS_LEX_VARS) == 0 && nameDecl != null)
            setCallersNeedStaticLink();
    }

    public final void setNeedsStaticLink() {
        int old = flags;
        flags |= NEEDS_STATIC_LINK;

        // If this needs an environment (closure), then its callers do too.
        if ((old & NEEDS_STATIC_LINK) == 0 && nameDecl != null)
            setCallersNeedStaticLink();
    }

    void setCallersNeedStaticLink() {
        LambdaExp outer = nameDecl.getContext().currentLambda();
        for (ApplyExp app = nameDecl.firstCall;  app != null;
             app = app.nextCall) {
            LambdaExp caller = app.context;
            for (; caller != outer && !(caller instanceof ModuleExp); 
                 caller = caller.outerLambda())
                caller.setNeedsStaticLink();
        }
    }

    public final boolean getCanRead() {
        return (flags & CAN_READ) != 0;
    }
    public final void setCanRead(boolean read) {
        if (read) flags |= CAN_READ;
        else flags &= ~CAN_READ;
    }

    /** True if this is a method in an ClassExp. */
    public final boolean isClassMethod() {
        return (flags & CLASS_METHOD) != 0;
    }

    public final void setClassMethod(boolean isMethod) {
        if (isMethod) flags |= CLASS_METHOD;
        else flags &= ~CLASS_METHOD;
    }

    /** True iff this is the dummy top-level function of a module body. */
    public final boolean isModuleBody() { return this instanceof ModuleExp; }

    public boolean isAbstract() {
        return body == QuoteExp.abstractExp;
    }

    public boolean isNative() {
        return body == QuoteExp.nativeExp;
    }

    int callConvention;
    /** The calling convention used for this function.
     * It is derived from Compilation's currentCallConvention.
     * @return One of the CALL_WITH_xxx values in Compilation. */
    public int getCallConvention() { return callConvention; }
    public void setCallConvention(Compilation comp) {
        if (isClassMethod()
            || (this instanceof ModuleExp
                && ((ModuleExp) this).staticInitRun()))
            callConvention = Compilation.CALL_WITH_RETURN;
        else {
            int defaultConvention = comp.currentCallConvention();
            callConvention =
                (defaultConvention < Compilation.CALL_WITH_CONSUMER
                 && isModuleBody())
                ? Compilation.CALL_WITH_CONSUMER
                : defaultConvention == Compilation.CALL_WITH_UNSPECIFIED
                ? Compilation.CALL_WITH_RETURN
                : defaultConvention;
        }
    }
    public boolean usingCallContext()
    { return getCallConvention() >= Compilation.CALL_WITH_CONSUMER; }

    /** This function can finish if specified functions can finish.
     * I.e. calling this function can complete normally is there is a bit i
     * such that for all LambdaExp l the mask canFinishCondition.get(l)
     * bit a zero value for bit i.
     * May be null if there is no dependency yet in the current execution
     * path fork, in which case PushApply.canFinishDeps will realize it.
     * This value is calculated during PushApply and used in InlineCalls.
     */
    CanFinishMap canFinishCondition;

    /** Set of functions whose canFinishCondition may depend on this. */
    Set<LambdaExp> canFinishListeners;

    void notifyCanFinish() {
        Set<LambdaExp> listeners = canFinishListeners;
        if (listeners != null) {
            canFinishListeners = null;
            for (LambdaExp f : listeners) {
                f.checkCanFinish();
            }
        } 
    }

    void checkCanFinish() {
        CanFinishMap cond = canFinishCondition;
        if (cond != null && ! getFlag(LambdaExp.IN_EXPWALKER)) {
            // See if we can simplify exp.canFinishCondition.
            // I.e. if any dependencies are now CAN_FINISH.
            if (cond.canFinish()) {
                canFinishCondition = CanFinishMap.CAN_FINISH;
                notifyCanFinish();
            }
        }
    }



    public final boolean isHandlingTailCalls() {
        return isModuleBody()
            || (getCallConvention() >= Compilation.CALL_WITH_TAILCALLS
                && ! isClassMethod());
    }

    public final boolean variable_args () { return max_args < 0; }

    ClassType compiledType = Compilation.typeProcedure;

    /** Return the ClassType of the Procedure this is being compiled into. */
    protected ClassType getCompiledClassType(Compilation comp) {
        if (compiledType == Compilation.typeProcedure)
            throw new Error("internal error: getCompiledClassType");
        return compiledType;
    }

    protected Type calculateType() {
        return compiledType;
    }

    /** The ClassType generated for this class.
     * Only used for ClassExp (which overrides this method) or ModuleExp.
     */
    public ClassType getClassType() { return compiledType; }

    public void setType (ClassType type) {
        this.compiledType = type;
        this.type = type;
    }

    /** Number of argument variable actually passed by the caller.
     * For functions that accept more than 4 argument, or take a variable number,
     * this is 1, since in that all arguments are passed in a single array. */
    public int incomingArgs() {
        // The max_args > 0 is a hack to handle LambdaProcedure, which
        // currently always uses a single array argument.
        return min_args == max_args && max_args <= 4 && max_args > 0 ? max_args : 1;
    }

    /* * If non-zero, the selector field of the ModuleMethod for this. * /
    //int selectorValue;

    int getSelectorValue(Compilation comp) {
        int s = selectorValue;
        if (s == 0) {
            s = comp.maxSelectorValue;
            comp.maxSelectorValue = s + primMethods.length;
            selectorValue = ++s;
        }
        return s;
    }
    */

    Method checkMethod;
    /*
    public Method getCheckMethod() {
        Method m = checkMethod;
        if (m == null) {
            m = 
            checkMethod = m;
        }
        return m;
    }
    */

    /** Methods used to implement this functions.
     * primMethods[0] is used if the argument count is min_args;
     * primMethods[1] is used if the argument count is min_args+1;
     * primMethods[primMethods.length-1] is used otherwise.
     * If HAS_NONTRIVIAL_DEFAULT there is one extra.
     */
    Method[] primMethods;
    /** If in a ClassExp which isMakingClassPair, the static body methods.
     * Otherwise, same as primMethods. */
    Method[] primBodyMethods;

    /** Select the method used given an argument count. */
    public final Method getMethod(int nonSpliceCount, int spliceCount) {
        if (primMethods == null || (max_args >= 0 && nonSpliceCount > max_args))
            return null;
        if (keywords != null || (opt_args > 0 && primMethods.length == 1))
            return null;
        int index = nonSpliceCount - min_args;
        if (index < 0)
            return null; // Too few arguments.
        int length = primMethods.length;
        if (spliceCount > 0)
            return length == 1 ? primMethods[0] : null;
        if (getFlag(HAS_NONTRIVIAL_DEFAULT))
            length--;
        return primMethods[index < length ? index : length - 1];
    }

    /** Get the method that contains the actual body of the procedure.
     * (The other methods are just stubs that call that method.) */
    public final Method getMainMethod() {
        Method[] methods = primBodyMethods;
        return methods == null ? null
            : methods[methods.length-(getFlag(HAS_NONTRIVIAL_DEFAULT)?2:1)];
    }

    /** Return the parameter type of the "keyword/rest" parameters. */
    public final Type restArgType() {
        if (min_args == max_args)
            return null;
        if (primMethods == null)
            throw new Error("internal error - restArgType");
        Method[] methods = primMethods;
        if (max_args >= 0 && methods.length > max_args - min_args)
            return null;
        Method method = methods[methods.length-1];
        Type[] types = method.getParameterTypes();
        int ilast = types.length-1;
        if (method.getName().endsWith("$X"))
            ilast--;
        return types[ilast];
    }

    public LambdaExp outerLambda() {
        return getOuter() == null ? null : getOuter().currentLambda ();
    }

    public LambdaExp outerLambdaOrCaller() {
        return getInlineOnly() ? inlineHome : outerLambda();
    }

    /** Return the closest outer non-inlined LambdaExp. */

    public LambdaExp outerLambdaNotInline() {
        for (ScopeExp exp = this; (exp = exp.getOuter()) != null; ) {
            if (exp instanceof LambdaExp) {
                LambdaExp result = (LambdaExp) exp;
                if (! result.getInlineOnly())
                    return result;
            }
        }
        return null;
    }

    /** True if given LambdaExp is inlined in this function, perhaps indirectly.
     * Is false if this is not inline-only or if getCaller() is not inlined is
     * outer.  Usually the same as (this.outerLambdaNotInline()==outer),
     * except in the case that outer.getInlineOnly(). */
    boolean inlinedIn(LambdaExp outer) {
        for (LambdaExp exp = this; ; exp = exp.getCaller()) {
            if (exp == outer)
                return true;
            if (! exp.getInlineOnly())
                return false;
        }
    }

    /** For an INLINE_ONLY function, return the function it gets inlined in. */
    public LambdaExp getCaller() {
        return inlineHome;
    }

    Variable thisVariable;

    public Variable declareThis(ClassType clas) {
        if (thisVariable == null) {
            thisVariable = new Variable("this");
            getVarScope().addVariableAfter(null, thisVariable);
            thisVariable.setParameter (true);
        }
        if (thisVariable.getType() == null)
            thisVariable.setType(clas);
        if (decls != null && decls.isThisParameter())
            decls.var = thisVariable;
        return thisVariable;
    }

    public Variable declareClosureEnv() {
        if (closureEnv == null && getNeedsClosureEnv()) {
            LambdaExp parent = outerLambdaOrCaller();
            if (parent instanceof ClassExp)
                parent = parent.outerLambda();
            if (isClassMethod() && ! "*init*".equals(getName()))
                closureEnv = declareThis(compiledType);
            else if (parent.heapFrame == null && ! parent.getNeedsStaticLink()
                     && ! (parent instanceof ModuleExp))
                closureEnv = null;
            else if (! isClassGenerated() && ! getInlineOnly()) {
                Method primMethod = getMainMethod();
                boolean isInit = "*init*".equals(getName());
                if (primMethod != null && ! primMethod.getStaticFlag()
                    && ! isInit)
                    closureEnv = declareThis(primMethod.getDeclaringClass());
                else if (inlinedInCheckMethod()) {
                    Type envType = getOwningLambda().getHeapFrameType();
                    closureEnv = new Variable(CLOSURE_ENV_NAME, envType);
                    getVarScope().addVariable(closureEnv);
                } else {
                    Type envType = primMethod.getParameterTypes()[0];
                    closureEnv = new Variable(CLOSURE_ENV_NAME, envType);
                    Variable prev;
                    if (isInit)
                        prev = declareThis(primMethod.getDeclaringClass());
                    else
                        prev = null;
                    getVarScope().addVariableAfter(prev, closureEnv);
                    closureEnv.setParameter(true);
                }
            } else {
                if (inlineHome != null)
                    inlineHome.declareClosureEnv();
                closureEnv =
                    parent.heapFrame != null && parent == outerLambda()
                    ? parent.heapFrame
                    : parent.closureEnv;
            }
        }
        return closureEnv;
    }

    public LambdaExp() {
    }

    public LambdaExp(int args) {
        min_args = args;
        max_args = args;
    }


    public LambdaExp(Expression body) {
        this.body = body;
    }

    /** Generate code to load heapFrame on the JVM stack. */
    public void loadHeapFrame (Compilation comp) {
        LambdaExp curLambda = comp.curLambda;
        while (curLambda != this && curLambda.getInlineOnly())
            curLambda = curLambda.getCaller();

        gnu.bytecode.CodeAttr code = comp.getCode();
        if (curLambda.heapFrame != null && this == curLambda) {
            code.emitLoad(curLambda.heapFrame);
            return;
        }
        ClassType curType;
        if (curLambda.closureEnv != null) {
            code.emitLoad(curLambda.closureEnv);
            curType = (ClassType) curLambda.closureEnv.getType();
        } else {
            code.emitPushThis();
            curType = comp.curClass;
        }
        while (curLambda != this) {
            Field link = curLambda.staticLinkField;
            if (link != null && link.getDeclaringClass() == curType) {
                code.emitGetField(link);
                curType = (ClassType) link.getType();
            }
            curLambda = curLambda.outerLambdaOrCaller();
        }
    }

    /** Get the i'the formal parameter. */
    Declaration getArg(int i) {
        for (Declaration var = firstDecl();  ; var = var.nextDecl()) {
            if (var == null)
                throw new Error ("internal error - getArg");
            if (i == 0)
                return var;
            --i;
        }
    }

    public void compileEnd (Compilation comp) {
        gnu.bytecode.CodeAttr code = comp.getCode();
        HashMap<String,Variable> varMap = new HashMap<String,Variable>();

        Label endLabel = new Label(code);
        while (pendingInlines != null && ! pendingInlines.isEmpty()) {
            LambdaExp child = (LambdaExp) pendingInlines.remove();
            Target ctarget = (Target) pendingInlines.remove();
            if (child.getInlineOnly()
                && ! child.getFlag(LambdaExp.METHODS_COMPILED)
                && child.startForInlining != null) {
                if (code.reachableHere())
                    code.emitGoto(endLabel);
                child.compileAsInlined(comp, ctarget);
            }
        }
        if (endLabel.isUsed())
            endLabel.define(code);
        code.getCurrentScope().fixParamNames(varMap);
        popScope(code);        // Undoes enterScope in allocParameters

        if (! inlinedInCallerOrCheckMethodOnly()) {
            if (comp.method.reachableHere()
                && (getCallConvention() < Compilation.CALL_WITH_TAILCALLS
                    || isModuleBody() || isClassMethod() || isHandlingTailCalls())) {
                code.emitReturn();
            }
            code.getCurrentScope().fixParamNames(varMap);
            code.popScope(); // Undoes pushScope in method.initCode.
        }

        for (LambdaExp child = firstChild;  child != null; ) {
            if (! child.getCanRead() && ! child.getInlineOnly()
                && child.getFlag(Expression.VALIDATED)) {
                child.compileAsMethod(comp);
            }
            else if (child instanceof ClassExp) {
                ((ClassExp) child).compileMembers(comp);
            }
            child = child.nextSibling;
        }

        if (heapFrame != null)
            comp.generateConstructor(this); 
        generateApplyMethods(comp);
    }

    public void generateApplyMethods(Compilation comp) {
        //comp.generateCheckMethods(this);
    }

    Field allocFieldFor(Compilation comp) {
        if (nameDecl != null && nameDecl.getField() != null
            && nameDecl.getValueRaw() == this)
            return nameDecl.getField();
        boolean needsClosure = getNeedsClosureEnv();
        ClassType frameType = needsClosure ? getOwningLambda().getHeapFrameType()
            : comp.mainClass;
        String name = getName();
        String fname
            = name == null ? "lambda" : Mangling.mangleField(name);
        int fflags = Access.FINAL;
        if (nameDecl != null && nameDecl.context instanceof ModuleExp) {
            boolean external_access = nameDecl.needsExternalAccess();
            if (external_access)
                fname = Declaration.PRIVATE_PREFIX + fname;
            if (nameDecl.getFlag(Declaration.STATIC_SPECIFIED)) {
                fflags |= Access.STATIC;
                // A static field in a non-static module is
                // initialized in <init>, not <clinit>,
                // which is bad for a "static final" field.
                if (! ((ModuleExp) nameDecl.context).isStatic())
                    fflags &= ~Access.FINAL;
            }
            // In immediate mode we may need to access the field from a future
            // command in a different "runtime package" (see JVM spec) because it
            // gets loaded by a different class loader.  So make the field public.
            if (! nameDecl.isPrivate() || external_access || comp.immediate)
                fflags |= Access.PUBLIC;
            if ((flags & OVERLOADABLE_FIELD) != 0) {
                String fname0 = fname;
                int suffix = min_args == max_args ? min_args : 1;
                do { fname = fname0 + '$' + suffix++; }
                while (frameType.getDeclaredField(fname) != null);
            }
        } else {
            fname = fname + "$Fn" + ++comp.localFieldIndex;
            if (! needsClosure)
                fflags |= Access.STATIC;
        }
        Field field =
            frameType.addField(fname, Compilation.typeCompiledProc, fflags);
        if (nameDecl != null)
            nameDecl.setField(field);
        return field;
    }

    final void addApplyMethod(Compilation comp, Field field) {
        LambdaExp owner = this;
        if (field != null && field.getStaticFlag())
            owner = comp.getModule();
        else {
            // Similar to getOwningLambda(), but we can't add apply methods
            // to a ClassExp - at least not unless it extends ModuleBody.
            for (;;) {
                owner = owner.outerLambda();
                if (owner instanceof ModuleExp
                    || owner.heapFrame != null)
                    break;
            }
            ClassType frameType = owner.getHeapFrameType();
            if (! (frameType.getSuperclass().isSubtype(Compilation.typeCompiledProc)))
                owner = comp.getModule();
        }
        if (owner.applyMethods == null)
            owner.applyMethods = new ArrayList<LambdaExp>();
        owner.applyMethods.add(this);
        checkMethod = comp.generateCheckMethod(this, owner);
    }

    public Field compileSetField(Compilation comp) {
        if (primMethods == null
            && ! inlinedInCheckMethod())
            allocMethod(outerLambda(), comp);
        Field field = allocFieldFor(comp);
        if (comp.usingCPStyle())
            compile(comp, Type.objectType);
        else {
            if (! inlinedInCheckMethod())
                compileAsMethod(comp);
            addApplyMethod(comp, field);
        }
        if (nameDecl != null)
            nameDecl.compileAnnotations(field, ElementType.FIELD);
        return (new ProcInitializer(this, comp, field)).field;
    }

    public void compile(Compilation comp, Target target) {
        if (target instanceof IgnoreTarget)
            return;
        if (getInlineOnly()) {
            // Normally this shouldn't happen.  One case where it does
            // is when passing an inline-only lambda as a parameter to a function
            // that doesn't get fully inlined.  Cleaner would be to elide
            // the ignored parameter.
            QuoteExp.nullExp.compile(comp, target);
            return;
        }
        Type rtype;
        CodeAttr code = comp.getCode();

        /*
        if (comp.usingCPStyle()) {
            //	Label func_start = new Label(code);
            Label func_end = new Label(code);
            LambdaExp saveLambda = comp.curLambda;
            comp.curLambda = this;
            type = saveLambda.type;
            closureEnv = saveLambda.closureEnv;
            // if (comp.usingCPStyle()) {
            //     heapFrame = comp.thisDecl;
            //     for (Declaration var = firstDecl();
            // 	 var != null; var = var.nextDecl())
            //       var.assignField(comp);
            //   }
            gnu.bytecode.SwitchState fswitch = comp.fswitch;
            int pc = comp.fswitch.getMaxValue() + 1;
            code.emitGoto(func_end);
            Type[] stackTypes = code.saveStackTypeState(true);

            fswitch.addCase(pc, code);
            // code.emitPushThis();
            // code.emitGetField(comp.argsCallContextField);
            // code.emitStore(comp.argsArray);
            allocParameters(comp);
            enterFunction(comp);

            compileBody(comp);
            comp.curLambda = saveLambda;
            func_end.define(code);
            code.restoreStackTypeState(stackTypes);
            ClassType ctype = comp.curClass;
            rtype = ctype;
            // code.emitNew(ctype);
            // code.emitDup(ctype);
            // code.emitInvokeSpecial(ctype.constructor);
            // code.emitDup(ctype);
            // code.emitPushInt(pc);
            // code.emitPutField(comp.saved_pcCallFrameField);
            // if (isHandlingTailCalls())
            //   {
            //     // Set name field.
            //     if (name != null)
            //       {
            // 	code.emitDup(ctype);
            // 	code.emitPushString(name);
            //  Method setNameMethod =
            //      Compilation.typeProcedure.getDeclaredMethod("setName", 1);
            // 	code.emitInvokeVirtual(comp.setNameMethod);
            //       }
            //     // Set numArgs field.
            //     code.emitDup(ctype);
            //     code.emitPushInt(min_args | (max_args << 12));
            //     code.emitPutField(comp.numArgsCallFrameField);
            //     // Set static link field to this CallFrame.
            //     code.emitDup(ctype);
            //     code.emitPushThis();
            //     code.emitPutField(comp.callerCallFrameField);
            //   }
          } else
          */
        {
            LambdaExp outer = outerLambda();
            rtype = Compilation.typeCompiledProc;
            if ((flags & NO_FIELD) != 0
                || comp.dumpingInitializers
                || (comp.immediate && outer instanceof ModuleExp
                    && comp.mainClass == comp.moduleClass)) {
                compileAsMethod(comp);
                addApplyMethod(comp, null);
                Variable savedInstance = comp.moduleInstanceVar;
                comp.moduleInstanceVar = null;
                ProcInitializer.emitLoadModuleMethod(this, comp);
                comp.moduleInstanceVar = savedInstance;
            } else {
                Field field = compileSetField(comp);
                if (field.getStaticFlag())
                    code.emitGetStatic(field);
                else {
                    LambdaExp parent = comp.curLambda;
                    while (parent.getInlineOnly() && parent.heapFrame == null)
                        parent = parent.outerLambda();
                    Variable frame
                        = parent.heapFrame != null ? parent.heapFrame
                        : parent.closureEnv;
                    code.emitLoad(frame);
                    code.emitGetField(field);
                }
            }
        }
        target.compileFromStack(comp, rtype);
    }

    public ClassType getHeapFrameType() {
        if (isClassGenerated())
            return (ClassType) getType();
        else
            return (ClassType) heapFrame.getType();
    }


    public LambdaExp getOwningLambda() {
        ScopeExp exp = getOuter();
        for (;; exp = exp.getOuter()) {
            if (exp == null)
                return null;
            if (exp instanceof ModuleExp
                || (exp instanceof ClassExp && getNeedsClosureEnv())
                || (exp instanceof LambdaExp 
                    && ((LambdaExp) exp).heapFrame != null))
                return (LambdaExp) exp;
        }
    }

    String primMethodName;
    String getMethodName(Compilation comp) {
        if (primMethodName == null) {
            StringBuilder nameBuf = new StringBuilder(60);
            LambdaExp outer = outerLambda();
            String name = getName();
           if (! (outer.isModuleBody() || outer instanceof ClassExp)
                || name == null) {
                nameBuf.append("lambda");
                nameBuf.append(+(++comp.method_counter));
            }
            if (outer instanceof ClassExp
                && this == ((ClassExp) outer).clinitMethod)
                nameBuf.append("<clinit>");
            else if (getSymbol() != null)
                nameBuf.append(Mangling.mangleName(name));
            primMethodName = nameBuf.toString();
        }
        return primMethodName;
    }

    void addMethodFor(Compilation comp, ObjectType closureEnvType) {
        ScopeExp sc = this;
        while (sc != null && ! (sc instanceof ClassExp))
            sc = sc.getOuter();
        ClassType ctype;
        // If this is nested inside a Class, then create the method in that
        // class - in case it references a private field/method.
        if (sc != null)
            ctype = ((ClassExp) sc).instanceType;
        else
            ctype = getOwningLambda().getHeapFrameType();
        addMethodFor(ctype, comp, closureEnvType);
    }

    void addMethodFor(ClassType ctype, Compilation comp,
                      ObjectType closureEnvType) {
        // generate_unique_name (new_class, child.getName());
        String name = getName();
        LambdaExp outer = outerLambda();

        int key_args = keywords == null ? 0 : keywords.length;
        boolean simpleMatch = true;
        int numStubs =
            ((flags & DEFAULT_CAPTURES_ARG) != 0) ? 0 : opt_args;
        // check for complications
        if (key_args > 0
            || getFlag(HAS_NONTRIVIAL_PATTERN)) { simpleMatch = false; numStubs = 0; } // For simplicity - FIXME?
        boolean varArgs = max_args < 0 || min_args + numStubs < max_args;

        boolean handleSuppliedArg = numStubs > 0 && getFlag(HAS_NONTRIVIAL_DEFAULT);
        if (handleSuppliedArg)
            numStubs++;
        Method[] methods = new Method[numStubs + 1];
        // We assume that for "pair" class methods that ClassExp.declareParts first
        // calls this method to create the interface method, and then calls us
        // to create the static implementation method.
        primBodyMethods = methods;
        if (primMethods == null)
            primMethods = methods;

        boolean isStatic;
        // 'I' if initMethod ($finit$); 'C' if clinitMethod (<clinit>).
        char isInitMethod = '\0';
        if (nameDecl != null
            && nameDecl.getFlag(Declaration.NONSTATIC_SPECIFIED))
            isStatic = false;
        else if (nameDecl != null
                 && nameDecl.getFlag(Declaration.STATIC_SPECIFIED))
            isStatic = true;
        else if (isClassMethod()) {
            if (outer instanceof ClassExp) {
                ClassExp cl = (ClassExp) outer;
                isStatic = cl.isMakingClassPair() && closureEnvType != null;
                if (this == cl.initMethod)
                    isInitMethod = 'I';
                else if (this == cl.clinitMethod) {
                    isInitMethod = 'C';
                    isStatic = true;
                }
            } else
                isStatic = false;
        } else if (thisVariable != null || closureEnvType == ctype)
            isStatic = false;
        else if (nameDecl != null && nameDecl.context instanceof ModuleExp) {
            ModuleExp mexp = (ModuleExp) nameDecl.context;
            isStatic = mexp.getSuperType() == null && mexp.getInterfaces() == null;
        } else
            isStatic = true;

        int mflags = isStatic ? Access.STATIC : 0;
        if (nameDecl != null) {
            if (nameDecl.needsExternalAccess())
                mflags |= Access.PUBLIC;
            else {
                short defaultFlag = nameDecl.isPrivate() ? 0 : Access.PUBLIC;
                if (isClassMethod())
                    defaultFlag = nameDecl.getAccessFlags(defaultFlag);
                mflags |= defaultFlag;
            }
        }
        if (getFlag(PUBLIC_METHOD))
            mflags |= Access.PUBLIC;
        StringBuilder nameBuf = new StringBuilder(getMethodName(comp));
        if (getFlag(SEQUENCE_RESULT))
            nameBuf.append("$C");
        boolean withContext
            = (getCallConvention() >= Compilation.CALL_WITH_CONSUMER
               && isInitMethod == '\0');
        if (isInitMethod != '\0') {
            if (isStatic) {
                // if cl.isMakingClassPair() - i.e. defining a non-simple class:
                // In this case the $finit$ method needs to be explicitly called
                // by sub-class constructors.  See Compilation.callInitMethods.
                mflags = (mflags & ~Access.PROTECTED+Access.PRIVATE)+Access.PUBLIC;
            } else {
                // if ! cl.isMakingClassPair() - i.e. defining a simple class:
                // Make it private to prevent inherited $finit$ from overriding
                // the current one - and thus preventing its execution.
                mflags = (mflags & ~Access.PUBLIC+Access.PROTECTED)+Access.PRIVATE;
            }
        }
        if (ctype.isInterface() || isAbstract())
            mflags |= Access.ABSTRACT;
        if (isNative())
            mflags |= Access.NATIVE;

        // If a class method has unspecified parameter types, see if we
        // can "inherit" the parameter types from an inherited method.
        if (isClassMethod() && outer instanceof ClassExp
            && min_args == max_args) {
            Method[] inherited = null;
            int iarg = 0;
            param_loop:
            for (Declaration param = firstDecl(); ;
                 param = param.nextDecl(), iarg++) {
                if (param == null) {
                    if (returnType != null)
                        break;
                } else if (param.isThisParameter()) {
                    iarg--;
                    continue;
                } else if (param.getFlag(Declaration.TYPE_SPECIFIED))
                    continue;
                if (inherited == null) {
                    final String mangled = nameBuf.toString();
                    gnu.bytecode.Filter filter
                        = new gnu.bytecode.Filter() {
                                public boolean select(Object value) {
                                    gnu.bytecode.Method method = (gnu.bytecode.Method) value;
                                    if (! method.getName().equals(mangled))
                                        return false;
                                    Type[] ptypes = method.getParameterTypes();
                                    return ptypes.length == min_args;
                                }
                            };
                    inherited = ctype.getMethods(filter, 2);
                }
                Type type = null;
                for (int i = inherited.length;  --i >= 0; ) {
                    Method method = inherited[i];
                    Type ptype = param == null ? method.getReturnType()
                        : method.getParameterTypes()[iarg];
                    if (type == null)
                        type = ptype;
                    else if (ptype != type) {
                        // More than one method with inconsistent parameter type.
                        if (param == null)
                            break param_loop;
                        else
                            continue param_loop;
                    }
                }
                if (type != null) {
                    type = comp.getLanguage().getLangTypeFor(type);
                    if (param != null)
                        param.setType(type);
                    else
                        setCoercedReturnType(type);
                }
                if (param == null)
                    break param_loop;
            }
        }

        Type rtype
            = (getFlag(SEQUENCE_RESULT)
               || getCallConvention () >= Compilation.CALL_WITH_CONSUMER)
            ? Type.voidType
            : getReturnType().promoteIfUnsigned().getImplementationType();
        int extraArg = (closureEnvType != null && closureEnvType != ctype) ? 1 : 0;

        String rtypeEnc = comp.getLanguage().encodeType(getReturnType());

        int ctxArg = 0;
        if (getCallConvention () >= Compilation.CALL_WITH_CONSUMER
            && isInitMethod == '\0')
            ctxArg = 1;

        int nameBaseLength = nameBuf.length();
        for (int i = 0;  i <= numStubs;  i++) {
            nameBuf.setLength(nameBaseLength);
            ArrayList<Type> argTypes = new ArrayList<Type>();
            if (extraArg > 0)
                argTypes.add(closureEnvType);
            Stack<String> encTypes = new Stack<String>();
            int encTypesSize = rtypeEnc == null /* || not interesting */ ? 0 : 1;
            encTypes.add(encTypesSize == 0 ? "" : rtypeEnc);
            Declaration var = firstDecl();
            if (var != null && var.isThisParameter())
                var = var.nextDecl();
            int argi = 0;
            for (; var != null; var = var.nextDecl()) {
                if (var.getFlag(Declaration.IS_SUPPLIED_PARAMETER)
                    && ! var.getFlag(Declaration.IS_PARAMETER )
                    && i < numStubs)
                    continue;
                if (var.getFlag(Declaration.IS_REST_PARAMETER)) {
                Type lastType = var.getType();
                String lastTypeName = lastType.getName();
                if (! simpleMatch)
                    ;
                else if (handleSuppliedArg && i == numStubs)
                    nameBuf.append("$P");
                else if (lastType instanceof ArrayType)
                    mflags |= Access.VARARGS;
                else 
                    nameBuf.append("$V");
                } else if (numStubs > 0 && argi >= min_args + i)
                    break;
            
                if (var.parameterForMethod())
                    argTypes.add(var.getType().promoteIfUnsigned().getImplementationType());
                String encType = comp.getLanguage().encodeType(var.getType());
                if (encType == null /* || not interesting */)
                    encType = "";
                else
                    encTypesSize = encTypes.size()+1;
                encTypes.add(encType);
                if (var.getFlag(Declaration.IS_PARAMETER))
                    argi++;
            }
            if (ctxArg != 0)
                argTypes.add(Compilation.typeCallContext);
            if (withContext)
                nameBuf.append("$X");

            boolean classSpecified
                = (outer instanceof ClassExp
                   || (outer instanceof ModuleExp
                       && (((ModuleExp) outer)
                           .getFlag(ModuleExp.SUPERTYPE_SPECIFIED))));
            if (! simpleMatch)
                nameBuf.append("$P");
            name = nameBuf.toString();
            Type[] atypes = argTypes.toArray(new Type[argTypes.size()]);

            // Rename the method if an existing method has the same
            // name and type in this class.
            // Additionally, if the base class or interfaces were not explicitly
            // specified, then search super-classes for conflicting methods
            // (such as "run" or "apply").
            int renameCount = 0;
            int len = nameBuf.length();
            String suffix = nameBuf.substring(nameBaseLength, len);
            retry:
            for (;;) {
                for (ClassType t = ctype;  t != null; t = t.getSuperclass()) {
                    if (t.getDeclaredMethod(name, atypes) != null) {
                        nameBuf.setLength(nameBaseLength);
                        nameBuf.append('$');
                        nameBuf.append(++renameCount);
                        nameBuf.append(suffix);
                        name = nameBuf.toString();
                        continue retry;
                    }
                    if (classSpecified)
                        // Do not search in super-classes
                        break;
                }
                break;
            }

            Method method = ctype.addMethod(name, atypes, rtype, mflags);
            // Maybe emit kawa.SourceMethodType annotation.
            if (encTypesSize > 0
                && ! (nameDecl != null
                      && nameDecl.getAnnotation(kawa.SourceMethodType.class) != null)) {
                AnnotationEntry ae =
                    new AnnotationEntry(ClassType.make("kawa.SourceMethodType"));
                while (encTypes.size() > encTypesSize)
                    encTypes.pop();
                ae.addMember("value", encTypes,
                             ArrayType.make(Type.javalangStringType));
                RuntimeAnnotationsAttr.maybeAddAnnotation(method, ae);
            }

            methods[i] = method;

            if (throwsSpecification != null && throwsSpecification.length > 0) {
                int n = throwsSpecification.length;
                ClassType[] exceptions = new ClassType[n];
                for (int j = 0;  j < n;  j++) {
                    ClassType exception = null;
                    Expression throwsExpr = throwsSpecification[j];
                    String msg = null;
                    if (throwsExpr instanceof ReferenceExp) {
                        ReferenceExp throwsRef = (ReferenceExp) throwsExpr;
                        Declaration decl = throwsRef.getBinding();
                        if (decl != null) {
                            Expression declValue = decl.getValue();
                            if (declValue instanceof ClassExp)
                                exception
                                    = ((ClassExp) declValue).getCompiledClassType(comp);
                            else
                                msg = "throws specification "+decl.getName()
                                    + " has non-class lexical binding";
                        }
                        else
                            msg = "unknown class "+throwsRef.getName();
                    }
                    else if (throwsExpr instanceof QuoteExp) {
                        Object value = ((QuoteExp) throwsExpr).getValue();
                        if (value instanceof Class)
                            value = Type.make((Class) value);
                        if (value instanceof ClassType)
                            exception = (ClassType) value;
                        if (exception != null
                            && ! exception.isSubtype(Type.javalangThrowableType))
                            msg = exception.getName() + " does not extend Throwable";
                    }
                    if (exception == null && msg == null)
                        msg = "invalid throws specification";
                    if (msg != null) {
                        comp.error('e', msg, throwsExpr);
                        exception = Type.javalangThrowableType;
                    }
                    exceptions[j] = exception;
                }
                ExceptionsAttr attr = new ExceptionsAttr(method);
                attr.setExceptions(exceptions);
            }
        }
    }

    // Can we merge this with allocParameters?
    public void allocChildClasses(Compilation comp) {
        Method main = getMainMethod();

        if (main != null && ! main.getStaticFlag()) {
            CodeAttr code = comp.getCode();
            // So this gets declared in the parameter_scope.
            scope = code.getCurrentScope();
            declareThis(main.getDeclaringClass());
            thisVariable.allocateLocal(code);
            scope = null;
        }

        Declaration decl = firstDecl();
        for (;;) {
            if (! getInlineOnly() && ! inlinedInCheckMethod()
                && getCallConvention() >= Compilation.CALL_WITH_CONSUMER
                && decl == null) {
                Variable var =
                    getVarScope().addVariable(null,
                                              Compilation.typeCallContext,
                                              "$ctx");
                var.setParameter(true);
            } 
            if (decl == null)
                break;
            Variable var = decl.var;
            // i is the register to use for the current parameter
            if (var != null
                || (getInlineOnly() && decl.ignorable())
                || ! decl.parameterForMethod())
                ;
            else if (decl.isSimple () && ! decl.isIndirectBinding()) {
                // For a simple parameter not captured by an inferior lambda,
                // just allocate it in the incoming register.
                var = decl.allocateVariable(null);
            } else {
                // This variable was captured by an inner lambda.
                // Its home location is in the heapFrame.
                // Later, we copy it from its incoming register
                // to its home location heapFrame.  Here we just create and
                // assign a Variable for the incoming (register) value.
                String vname = decl.getName();
                if (vname != null)
                    vname = Mangling.mangleName(vname).intern();
                Type vtype = decl.getType().promoteIfUnsigned().getImplementationType();
                var = decl.var = getVarScope().addVariable(null, vtype, vname);
                //getVarScope().addVariableAfter(var, decl);
                var.setParameter (true);
                //var.allocateLocal(code);
            }
            decl = decl.nextDecl();
        }

        declareClosureEnv();

        allocFrame(comp);

        allocChildMethods(comp);
    }

    void allocMethod(LambdaExp outer, Compilation comp) {
        ObjectType closureEnvType;
        if (currentModule().info != null) {
            int state = currentModule().info.getState();
            if (state>=Compilation.COMPILED && state != Compilation.ERROR_SEEN)
                comp.error('f', "internal error - allocate method for "+this
                           +" in module "+currentModule()
                           +" that has already been compiled\n(Try removing all class files and doing a full re-compile.)");
        }
        if (! getNeedsClosureEnv())
            closureEnvType = null;
        else if (outer.isClassGenerated())
            closureEnvType = outer.getCompiledClassType(comp);
        else {
            LambdaExp owner = outer;
            while (owner.heapFrame == null)
                owner = owner.outerLambda();
            closureEnvType = (ClassType) owner.heapFrame.getType();
        }
        addMethodFor(comp, closureEnvType);
    }

    public void pushChild(LambdaExp child) {
        child.nextSibling = firstChild;
        firstChild = child;
    }

    public void reverseChildList() {
        LambdaExp prev = null, child = firstChild;
        while (child != null) {
            LambdaExp next = child.nextSibling;
            child.nextSibling = prev;
            prev = child;
            child = next;
        }
        firstChild = prev;
    }

    void allocChildMethods(Compilation comp) {
        for (LambdaExp child = firstChild;  child != null;
             child = child.nextSibling) {
            if (child instanceof ClassExp) {
                ClassExp cl = (ClassExp) child;
                if (cl.getNeedsClosureEnv())  {
                    ClassType parentFrameType;
                    if (isClassGenerated())
                        parentFrameType = (ClassType) getType();
                    else {
                        Variable parentFrame = this.heapFrame != null
                            ? this.heapFrame
                            : this.closureEnv;
                        parentFrameType = (ClassType) parentFrame.getType();
                    }
                    cl.closureEnvField = cl.staticLinkField
                        = cl.instanceType.setOuterLink(parentFrameType);
                }
            }
        }
    }

    public void allocFrame(Compilation comp) {
        if (heapFrame != null)  {
            ClassType frameType;
            if (isClassGenerated())
                frameType = getCompiledClassType(comp);
            else  {
                frameType = new ClassType(comp.generateClassName("frame"));
                frameType.setSuper(comp.getModuleType()); // FIXME - not needed?
                comp.addClass(frameType);
            }
            heapFrame.setType(frameType);
        }
    }

    void allocParameters(Compilation comp) {
        //compile parameter annotations

        //first figure if any of parameters even have annotations
        Declaration decl = firstDecl();
        boolean hasParameterAnnotation = false;
        if (!this.isModuleBody() && decl != null) {
            if (this.isClassMethod())
                decl = decl.nextDecl(); //skip implicit class param
            while (decl != null) {
                if (decl.numAnnotations() > 0) {
                    hasParameterAnnotation = true;
                    break;
                }
                decl = decl.nextDecl();
            }
        }

        //if at least one of params had an annotation, loop again
        //and compile them
        decl = firstDecl();
        if (decl != null && hasParameterAnnotation) {
            if (this.isClassMethod())
                decl = decl.nextDecl(); //skip implicit class param
            int i = 0;
            while (decl != null) {
                if (decl.isThisParameter()) {
                    decl = decl.nextDecl();
                    continue;
                }
                decl.compileParameterAnnotations(comp.method, i);
                i++;
                decl = decl.nextDecl();
            }
        }

        CodeAttr code = comp.getCode();
        Scope sc = getVarScope();
        code.locals.enterScope(sc);
        int line = getLineNumber();
        if (line > 0)
            code.putLineNumber(getFileName(), line);
    }

    /** Rembembers stuff to do in <init> of this class. */
    Initializer initChain;

    void enterFunction(Compilation comp) {
        CodeAttr code = comp.getCode();

        // Tail-calls loop back to here!
        if (! getFlag(LambdaExp.HAS_NONTRIVIAL_PATTERN))
            getVarScope().noteStartFunction(code);

        if (closureEnv != null && ! closureEnv.isParameter()
            && ! comp.usingCPStyle()) {
            if (! inlinedInCallerOrCheckMethodOnly()) {
                code.emitPushThis();
                Field field = closureEnvField;
                if (field == null)
                    field = outerLambda().closureEnvField;
                code.emitGetField(field);
                code.emitStore(closureEnv);
            } else if (inlinedInCheckMethod()) {
                 comp.loadModuleRef(getOwningLambda().getHeapFrameType());
                 
                 code.emitStore(closureEnv);
            } else if (! inlinedIn(outerLambda())) {
                outerLambdaOrCaller().loadHeapFrame(comp);
                code.emitStore(closureEnv);
            }
        }
        if (! comp.usingCPStyle()) {
            ClassType frameType = heapFrame == null
                ? currentModule().getCompiledClassType(comp)
                : (ClassType) heapFrame.getType();
            for (Declaration decl = capturedVars; decl != null;
                 decl = decl.nextCapturedVar) {
                if (decl.getField() != null)
                    continue;
                decl.makeField(frameType, comp, null);
            }
        }
        if (heapFrame != null && ! comp.usingCPStyle()) {
            ClassType frameType = (ClassType) heapFrame.getType();
            if (closureEnv != null && ! (this instanceof ModuleExp))
                staticLinkField = frameType.addField("staticLink",
                                                     closureEnv.getType());
            if (! isClassGenerated()) {
                frameType.setEnclosingMember(comp.method);
                code.emitNew(frameType);
                code.emitDup(frameType);
                Method constructor = Compilation.getConstructor(frameType, this);
                code.emitInvokeSpecial(constructor);

                if (staticLinkField != null) {
                    code.emitDup(frameType);
                    code.emitLoad(closureEnv);
                    code.emitPutField(staticLinkField);
                }
                heapFrame.allocateLocal(code);
                code.emitStore(heapFrame);
                code.pushAutoPoppableScope().addVariable(heapFrame);
            }
        }

        if (! inlinedInCheckMethod() && ! (this instanceof ModuleExp)) {
            // For each non-artificial parameter, copy it from its incoming
            // location (a local variable register, or the argsArray) into
            // its home location, if they are different.

            for (Declaration param = firstDecl();  param != null;
                 param = param.nextDecl()) {
                saveParameter(param, comp);
            }
        }
    }

    void saveParameter(Declaration param, Compilation comp) {
            if (! param.isSimple() && ! param.ignorable()
                || param.isIndirectBinding()) {
                CodeAttr code = comp.getCode();
                Type paramType = param.getType();
                Type stackType = paramType;
                // If the parameter is captured by an inferior lambda,
                // then the incoming parameter needs to be copied into its
                // slot in the heapFrame.
                if (!param.isSimple())
                    param.loadOwningObject(null, comp);
                // This part of the code pushes the incoming argument.
                code.emitLoad(param.getVariable());
                if (param.isIndirectBinding())
                    param.pushIndirectBinding(comp);
                if (param.isSimple()) {
                    Variable var = param.getVariable();
                    if (param.isIndirectBinding())
                        var.setType(Compilation.typeLocation);
                    code.emitStore(var);
                }
                else
                    code.emitPutField(param.getField());
            }
    }

    void compileAsInlined(Compilation comp, Target target) {
        flags |= LambdaExp.METHODS_COMPILED;
	LambdaExp saveLambda = comp.curLambda;
	comp.curLambda = this;
	allocChildClasses(comp);
	allocParameters(comp);
        CodeAttr code = comp.getCode();
        if (startForInlining == null)
            startForInlining = new Label(code);
        startForInlining.define(code);
	ApplyExp.popParams(code, this, null, false);
	enterFunction(comp);
	body.compileWithPosition(comp, target);
	compileEnd(comp);
	comp.curLambda = saveLambda;
    }

    void compileAsMethod(Compilation comp) {
        if ((flags & METHODS_COMPILED) != 0 || isAbstract() || isNative()
            || inlinedInCheckMethod())
            return;
        flags |= METHODS_COMPILED;
        if (primMethods == null)
            allocMethod(outerLambda(), comp);
        Method save_method = comp.method;
        LambdaExp save_lambda = comp.curLambda;
        comp.curLambda = this;

        Method method = primMethods[0];
        boolean isStatic = method.getStaticFlag();
        int numStubs = primMethods.length - 1;
        Type restArgType = restArgType();

        long[] saveDeclFlags = null;
        if (numStubs > 0) {
            saveDeclFlags = new long[countDecls()];
            int k = 0;
            for (Declaration decl = firstDecl();
                 decl != null; decl = decl.nextDecl())
                saveDeclFlags[k++] = decl.flags;
        }

        boolean ctxArg = getCallConvention () >= Compilation.CALL_WITH_CONSUMER;

        for (int i = 0;  i <= numStubs;  i++) {
            comp.method = primMethods[i];
            if (nameDecl != null && ! isClassMethod()) // Only if i == numStubs ???
                nameDecl.compileAnnotations(comp.method, ElementType.METHOD);

            if (i < numStubs) {
                CodeAttr code = comp.method.startCode();
                Declaration decl;
                Variable callContextSave = comp.callContextVar;
                Variable var = code.getArg(0);
                if (! isStatic) {
                    code.emitPushThis();
                    if (getNeedsClosureEnv())
                        closureEnv = var;
                    var = code.getArg(1);
                }
                decl = firstDecl();
                int copied = min_args + i;
                if (getFlag(HAS_NONTRIVIAL_DEFAULT)
                    && i == numStubs-1
                    && restArgType != null)
                    copied++;
                for (int j = 0;  j < copied;
                     j++, decl = decl.nextDecl()) {
                    decl.flags |= Declaration.IS_SIMPLE;
                    decl.var = var;
                    code.emitLoad(var);
                    var = var.nextVar();
                    if (decl.getFlag(Declaration.IS_SUPPLIED_PARAMETER)) {
                        code.emitPushInt(1);
                        decl = decl.nextDecl();
                    }
                }
                comp.callContextVar = ctxArg ? var : null;
                boolean suppliedFlags = getFlag(HAS_NONTRIVIAL_DEFAULT);
                int toCall = suppliedFlags ? numStubs : i + 1;
                for (int j = i; decl != null && j < toCall && ! decl.getFlag(Declaration.IS_REST_PARAMETER); ) {
                    Expression defaultArg = decl.getInitValue();
                    if (decl.getFlag(Declaration.IS_SUPPLIED_PARAMETER)
                        && ! (defaultArg instanceof QuoteExp)) {
                        code.emitPushDefaultValue(decl.getType());
                    } else {
                        Target paramTarget =
                            StackTarget.getInstance(decl.getType());
                        defaultArg.compile(comp, paramTarget);
                    }
                    if (decl.getFlag(Declaration.IS_SUPPLIED_PARAMETER)) {
                        code.emitPushInt(0);
                        decl = decl.nextDecl();
                    }
                    decl = decl.nextDecl();
                    j++;
                    // Minor optimization: Normally stub[i] calls stub[i+1],
                    // which calls stub[i+2] etc until we get to stub[numStubs].
                    // That way any given default argument expression is only
                    // compiled into a single stub.  However, if the default is a
                    // constant it makes sense to call stub[j] (where j>i+1) directly.
                    if (toCall < numStubs
                        && decl.getInitValue() instanceof QuoteExp)
                        toCall++;
                }
                boolean varArgs = toCall == numStubs && i < opt_args && restArgType != null;
                if (varArgs) {
                    Expression arg;
                    String lastTypeName = restArgType.getName();
                    if (restArgType == LangObjType.listType || "gnu.lists.LList".equals(lastTypeName))
                        arg = QuoteExp.emptyExp;
                    else if ("java.lang.Object[]".equals(lastTypeName))
                        arg = new QuoteExp(Values.noArgs);
                    else // FIXME
                        throw new Error("unimplemented #!rest type "+lastTypeName);
                    arg.compile(comp, restArgType);
                }
                if (ctxArg)
                    code.emitLoad(var);
                if (isStatic)
                    code.emitInvokeStatic(primMethods[toCall]);
                else
                    code.emitInvokeVirtual(primMethods[toCall]);
                code.emitReturn();
                closureEnv = null;
                comp.callContextVar = callContextSave;
            } else {
                if (saveDeclFlags != null) {
                    int k = 0;
                    for (Declaration decl = firstDecl();
                         decl != null; decl = decl.nextDecl()) {
                        decl.flags = saveDeclFlags[k++];
                        decl.var = null;
                    }
                }
                comp.method.initCode();
                allocChildClasses(comp);
                allocParameters(comp);
                if (getFlag(HAS_NONTRIVIAL_DEFAULT)) {
                    for (Declaration decl = firstDecl();
                         decl != null; decl = decl.nextDecl()) {
                        Expression defaultArg = decl.getInitValue();
                        if (decl.getFlag(Declaration.IS_SUPPLIED_PARAMETER)) {
                            Declaration supp = decl.nextDecl();
                            if (! (defaultArg instanceof QuoteExp)) {
                                CodeAttr code = comp.method.getCode();
                                supp.load(null, 0, comp,
                                          Target.pushValue(Type.booleanType));
                                Label doneDefault = new Label(code);
                                code.emitGotoIfIntNeZero(doneDefault);
                                code.emitIfThen();
                                Target paramTarget =
                                    StackTarget.getInstance(decl.getType());
                                defaultArg.compile(comp, paramTarget);
                                decl.compileStore(comp);
                                doneDefault.define(code);
                            }
                            decl = supp;
                        }
                    }
                }
                enterFunction(comp);

                compileBody(comp);
            }
        }

        comp.method = save_method;
        comp.curLambda = save_lambda;
    }

    public void compileBody(Compilation comp) {
        Target target;
        Variable callContextSave = comp.callContextVar;
        comp.callContextVar = null;
        if (getCallConvention() >= Compilation.CALL_WITH_CONSUMER) {
            Variable var = getVarScope().lookup("$ctx");
            if (var != null && var.getType() == Compilation.typeCallContext)
                comp.callContextVar = var;
            target = ConsumerTarget.makeContextTarget(comp, getReturnType());
        }
        else
            target = Target.pushValue(getReturnType());
        ScopeExp savedScope = comp.currentScope();
        comp.current_scope = this;
        body.compileWithPosition(comp, target,
                                 body.getLineNumber() > 0 ? body : this);
        comp.current_scope = savedScope;
        compileEnd(comp);
        comp.callContextVar = callContextSave;
    }

    /** A cache if this has already been evaluated. */
    Procedure thisValue;

    protected <R,D> R visit(ExpVisitor<R,D> visitor, D d) {
        Compilation comp = visitor.getCompilation();
        LambdaExp saveLambda;
        if (comp == null)
            saveLambda = null;
        else {
            saveLambda = comp.curLambda;
            comp.curLambda = this;
        }
        try {
            return visitor.visitLambdaExp(this, d);
        } finally {
            if (comp != null)
                comp.curLambda = saveLambda;
        }
    }

    protected <R,D> void visitChildren(ExpVisitor<R,D> visitor, D d) {
        visitChildrenOnly(visitor, d);
        visitProperties(visitor, d);
    }

    protected final <R,D> void visitChildrenOnly(ExpVisitor<R,D> visitor, D d) {
        LambdaExp save = visitor.currentLambda;
        visitor.currentLambda = this;
        try {
            throwsSpecification = visitor.visitExps(throwsSpecification, d);
            visitor.visitDefaultArgs(this, d);
            if (visitor.exitValue == null && body != null)
                body = visitor.update(body, visitor.visit(body, d));
        } finally {
            visitor.currentLambda = save;
        }
    }

    protected final <R,D> void visitProperties(ExpVisitor<R,D> visitor, D d) {
        if (properties != null) {
            int len = properties.length;
            for (int i = 1;  i < len;  i += 2) {
                Object val = properties[i];
                if (val instanceof Expression) {
                    properties[i] = visitor.visitAndUpdate((Expression) val, d);
                }
            }
        }
    }

    protected boolean mustCompile() {
        if (keywords != null && keywords.length > 0)
            return true;
        if (opt_args != 0) {
            for (Declaration p = firstDecl(); p != null; p = p.nextDecl()) {
                Expression defaultArg = p.getInitValue();
                // Non-constant default arguments require care with scoping.
                if (defaultArg != null && ! (defaultArg instanceof QuoteExp))
                    return true;
            }
        }
        return false;
    }

    @Override
    public void apply(CallContext ctx) throws Throwable {
        // It would be better to call setIndexes at compile-time, but that
        // doesn't work if we're called as a syntax expander at rewrite time.
        // Better, if this is a top-level eval, to create a "compile-time" module,
        // but I haven't figured out how to do that.  FIXME.
        setIndexes();
        ctx.writeValue(new Closure(this, ctx));
    }

    Object evalDefaultArg(Declaration param, CallContext ctx) {
        try {
            return param.getInitValue().eval(ctx);
        } catch (Error ex) {
            throw ex;
        } catch (Throwable ex) {
            throw new WrappedException("error evaluating default argument", ex);
        }
    }

    public Expression validateApply(ApplyExp exp, InlineCalls visitor,
                                    Type required, Declaration decl) {
        Expression[] args = exp.getArgs();
        if (! exp.isSimple()) {
            // We might be unable to inline this function. If we need to
            // call it using apply, it needs to be readable.
            // FIXME better to use pattern-matching:
            // Given: (define (fun a b c d) ...)
            // Translate: (fun x @y z) to:
            // (let (([t1 t2 t3 t4] [x @y z])) (fun t1 t2 t3 t4])
            setCanRead(true);
            if (nameDecl != null)
                nameDecl.setCanRead(true);
        }
        if ((flags & ATTEMPT_INLINE) != 0) {
            Expression inlined = InlineCalls.inlineCall(this, exp, true);
            if (inlined != null)
                return visitor.visit(inlined, required);
        }
        exp.visitArgs(visitor, this);
        int args_length = exp.args.length;
        int spliceCount = exp.spliceCount();
        int nonSpliceCount = args_length - spliceCount;
        int nonSpliceNonKeyCount = nonSpliceCount - 2 * exp.numKeywordArgs;
        String msg = WrongArguments.checkArgCount(getName(),
                                                  spliceCount > 0 ? 0 : min_args,
                                                  max_args,
                                                  nonSpliceNonKeyCount);
        if (msg != null) {
            return visitor.noteError(msg);
        }
        return exp;
    }

    public void print(OutPort out) {
        out.startLogicalBlock("(Lambda/", ")", 2);
        Object sym = getSymbol();
        if (sym != null) {
            out.print(sym);
            out.print('/');
        }
        out.print(id);
        out.print('/');
        out.print("fl:");  out.print(Integer.toHexString(flags));
        out.writeSpaceFill();
        printLineColumn(out);
        out.startLogicalBlock("(", false, ")");
        Special prevMode = null;
        int i = -1;
        int key_args = keywords == null ? 0 : keywords.length;
        Declaration decl = firstDecl();
        if (decl != null && decl.isThisParameter())
            i = -2;
        for (; decl != null;  decl = decl.nextDecl()) {
            if (decl != firstDecl())
                out.writeSpaceFill();
            Special mode = prevMode;
            if (decl.getFlag(Declaration.IS_PARAMETER)) {
                i++;
            if (i < min_args
                || (i == min_args && decl.getFlag(Declaration.PATTERN_NESTED)))
                mode = null;
            else if (i < min_args + opt_args)
                mode = Special.optional;
            else if (decl.getFlag(Declaration.IS_REST_PARAMETER))
                mode = Special.rest;
            else
                mode = Special.key;
            }
            if (mode != prevMode) {
                out.print(mode);
                out.writeSpaceFill();
            }
            Expression defaultArg = decl.getInitValue();
            if (defaultArg != null)
                out.startLogicalBlock("(", false, ")");
            if (decl.getFlag(Declaration.IS_SUPPLIED_PARAMETER)
                && ! decl.getFlag(Declaration.IS_PARAMETER))
                out.print("supplied:");
            decl.printInfo(out);
            if (defaultArg != null) {
                if (defaultArg != QuoteExp.falseExp) {
                    out.writeSpaceFill();
                    out.print("default:");
                    out.writeSpaceFill();
                    defaultArg.print(out);
                }
                out.endLogicalBlock(")");
            }
            prevMode = mode;
        }
        out.endLogicalBlock(")");
        if (properties != null) {
            int plen = properties.length;
            for (int j = 0; j < plen; j += 2) {
                Object key = properties[j];
                if (key == null)
                    continue;
                out.writeSpaceFill();
                out.startLogicalBlock("", false, "");
                out.print(key);
                out.print(":");
                out.writeSpaceFill();
                out.print(properties[j+1]);
                out.endLogicalBlock("");
            }
        }
        out.writeSpaceLinear();
        if (body == null)
            out.print("<null body>");
        else
            body.print(out);
        out.endLogicalBlock(")");
    }

    protected final String getExpClassName() {
        String cname = getClass().getName();
        int index = cname.lastIndexOf('.');
        if (index >= 0)
            cname = cname.substring(index+1);
        return cname;
    }

    public boolean side_effects () { return false; }

    public String toString() {
        String str = getExpClassName()+':'+getSymbol()+'/'+id+'/';

	int l = getLineNumber();
	if (l <= 0 && body != null)
            l = body.getLineNumber();
	if (l > 0)
            str = str + "l:" + l;

        return str;
    }

    /** If non-null, a sequence of (key, value)-pairs.
     * These will be used to call setProperty at run-time. */
    Object[] properties;

    public Object getProperty(Object key, Object defaultValue) {
        if (properties != null) {
            for (int i = properties.length;  (i -= 2) >= 0; ) {
                if (properties[i] == key)
                    return properties[i + 1];
            }
        }
        return defaultValue;
    }

    public synchronized void setProperty(Object key, Object value) {
        properties = PropertySet.setProperty(properties, key, value);
    }

    /** If non-null, the type of values returned by this function.
     * If null, the return type has not been set or calculated yet. */
    public Type returnType;

    /** The return type of this function, i.e the type of its returned values. */
    public final Type getReturnType() {
        if (returnType == null) {
            returnType = Type.objectType;  // To guard against cycles.
            // body may not be set if define scan'd but not yet rewritten.
            if (body != null && ! isAbstract() && ! isNative()
                && body.getFlag(Expression.VALIDATED))
                returnType = body.getType();
        }
        return returnType;
    }

    /* Set the return type of this function. */
    public final void setReturnType(Type returnType) {
        this.returnType = returnType;
    }

    public final void setCoercedReturnType(Type returnType) {
        this.returnType = returnType;
        if (returnType != null
            && returnType != Type.objectType
            && returnType != Type.voidType
            && body != QuoteExp.abstractExp
            && body != QuoteExp.nativeExp) {
            Expression value = body;
            body = Compilation.makeCoercion(value, returnType);
            body.setLine(value);
        }
    }

    public static void maybeSetReturnType(LambdaExp lexp, Type type) {
        if (lexp.returnType == null && type != null
            && ! (type instanceof InlineCalls.LenientExpectedType)
            && ! (type instanceof InlineCalls.ValueNeededType))
            lexp.setCoercedReturnType(type);
    }

    /** Modify LambdaExp so result is coerced to given type. */
    public final void setCoercedReturnValue(Expression type,
                                            Language language) {
        if (! isAbstract() && ! isNative()) {
            Expression value = body;
            body = Compilation.makeCoercion(value, type);
            body.setLine(value);
        }
        gnu.bytecode.Type rtype = language.getTypeFor(type);
        if (rtype != null)
            setReturnType(rtype);
    }

    /** Get the first expression/statement in the body.
     * It dives down into {@code BeginExp}s.
     * Used to check for {@code invoke-special} calls in {@code @init} methods.
     */
    public Expression getBodyFirstExpression() {
        Expression bodyFirst = body;
        for (;;) {
            if (bodyFirst instanceof BeginExp) {
                BeginExp bbody = (BeginExp) bodyFirst;
                if (bbody.length == 0)
                    bodyFirst = null;
                else
                    bodyFirst = bbody.exps[0];
            } else if (bodyFirst instanceof LetExp) {
                bodyFirst = ((LetExp) bodyFirst).getBody();
            } else
                break;
        }
        return bodyFirst;
    }

    /** Check if argument is a this(...) or super(...) initializtion.
     * If so, return return the corresponding this or super class.
     */
    public ClassType checkForInitCall(Expression bodyFirst) {
        ClassType calledInit = null;
        if (bodyFirst instanceof ApplyExp) {
            Expression exp = ((ApplyExp) bodyFirst).func;
            if (exp instanceof QuoteExp) {
                Object value = ((QuoteExp) exp).getValue();
                if (value instanceof PrimProcedure) {
                    PrimProcedure pproc = (PrimProcedure) value;
                    Method meth = pproc.getMethod();
                    if (pproc.isSpecial()
                        && ("<init>".equals(meth.getName())))
                        calledInit = meth.getDeclaringClass();
                }
            }
        }
        return calledInit;
    }

    public static class Closure extends MethodProc {
        Object[][] evalFrames;
        LambdaExp lambda;

        public int numArgs() { return lambda.min_args | (lambda.max_args << 12); }

        public Closure(LambdaExp lexp, CallContext ctx) {
            super(true, applyToConsumer);
            this.lambda = lexp;

            Object[][] oldFrames = ctx.evalFrames;
            if (oldFrames != null) {
                int n = oldFrames.length;
                while (n > 0 && oldFrames[n-1] == null)
                    n--;

                evalFrames = new Object[n][];
                System.arraycopy(oldFrames, 0, evalFrames, 0, n);
            }
            setSymbol(lambda.getSymbol());
        }

        public static Object applyToConsumer(Procedure proc, CallContext ctx)
                throws Throwable {
            Closure closure = (Closure) proc;
            LambdaExp lambda = closure.lambda;
            Object[] args = ctx.getArgs();
            Object[][] evalFrames = closure.evalFrames;
            int num = proc.numArgs();
            int nargs = ctx.getArgCount();
            int min = num & 0xFFF;
            if (nargs < min) {
                ctx.matchError(MethodProc.NO_MATCH_TOO_FEW_ARGS|min);
                return ctx;
            }
            int max = num >> 12;
            if (nargs > max && max >= 0) {
                ctx.matchError(MethodProc.NO_MATCH_TOO_MANY_ARGS|max);
                return ctx;
            }
            Object[] evalFrame = new Object[lambda.frameSize];
            int key_args = lambda.keywords == null ? 0 : lambda.keywords.length;
            int opt_args = lambda.opt_args;
            int i = 0;
            int key_i = 0;
            int min_args = lambda.min_args;
            for (Declaration decl = lambda.firstDecl(); decl != null;
                 decl = decl.nextDecl()) {
                Object value;
                if (i < min_args)
                    value = args[i++];
                else if (i < min_args + opt_args) {
                    if (i < nargs)
                        value = args[i++];
                    else
                        value = lambda.evalDefaultArg(decl, ctx);
                } else if (lambda.max_args < 0 && i == min_args + opt_args) {
                    if (decl.type instanceof ArrayType) {
                        int rem = nargs - i;
                        Type elementType = ((ArrayType) decl.type).getComponentType();
                        if (elementType == Type.objectType) {
                            Object[] rest = new Object[rem];
                            System.arraycopy(args, i, rest, 0, rem);
                            value = rest;
                        } else {
                            Class elementClass = elementType.getReflectClass();
                            value
                                = java.lang.reflect.Array.newInstance(elementClass, rem);
                            for (int j = 0;  j < rem;  j++) {
                                Object el;
                                try {
                                    el = elementType.coerceFromObject(args[i+j]);
                                } catch (ClassCastException ex) {
                                    ctx.matchError(NO_MATCH_BAD_TYPE|(i+j));
                                    return ctx;
                                }
                                java.lang.reflect.Array.set(value, j, el);
                            }
                        }
                    }
                    else
                        value = LList.makeList(args, i);
                }
                else {
                    // Keyword argument.
                    Keyword keyword = lambda.keywords[key_i++];
                    int key_offset = min_args + opt_args;
                    value = Keyword.searchForKeyword(args, key_offset, keyword);
                    if (value == Special.dfault)
                        value = lambda.evalDefaultArg(decl, ctx);
                }
                if (decl.type != null) {
                    try {
                        value = decl.type.coerceFromObject(value);
                    } catch (ClassCastException ex) {
                        ctx.matchError(NO_MATCH_BAD_TYPE|i);
                        return ctx;
                    }
                }
                if (decl.isIndirectBinding()) {
                    gnu.mapping.Location loc = decl.makeIndirectLocationFor();
                    loc.set(value);
                    value = loc;
                }
                evalFrame[decl.evalIndex] = value;
            }

            ctx.next = ctx.numArguments();
            if (ctx.checkDone() != 0)
                return ctx;

            int level = ScopeExp.nesting(lambda);
            Object[][] saveFrames = ctx.evalFrames;

            int numFrames = evalFrames == null ? 0 : evalFrames.length;
            if (level >= numFrames)
                numFrames = level;
            numFrames += 10;
            Object[][] newFrames = new Object[numFrames][];
            if (evalFrames != null)
                System.arraycopy(evalFrames, 0, newFrames, 0, evalFrames.length);
            newFrames[level] = evalFrame;
            ctx.evalFrames = newFrames;

            try {
                if (lambda.body == null) {
                    // This can happen if a syntax-case macro calls a function
                    // in the same compilation unit.  FIXME.
                    StringBuffer sbuf = new StringBuffer("procedure ");
                    String name = lambda.getName();
                    if (name == null)
                        name = "<anonymous>";
                    sbuf.append(name);
                    int line = lambda.getLineNumber();
                    if (line > 0) {
                        sbuf.append(" at line ");
                        sbuf.append(line);
                    }
                    sbuf.append(" was called before it was expanded");
                    throw new RuntimeException(sbuf.toString());
                }
                lambda.body.apply(ctx);
            } finally {
                ctx.evalFrames = saveFrames;
            }
            return ctx;
        }

        public Object getProperty(Object key, Object defaultValue) {
            Object value = super.getProperty(key, defaultValue);
            if (value == null)
                value = lambda.getProperty(key, defaultValue);
            return value;
        }
    }

    public static final MethodHandle applyToConsumer
        = lookupApplyHandle(Closure.class , "applyToConsumer");

}
