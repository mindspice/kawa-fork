// Copyright (c) 1999, 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/** A primitive Procedure implemented by a plain Java method. */

public class PrimProcedure extends MethodProc implements gnu.expr.Inlineable
{
  Type retType;
  Type[] argTypes;
  Method method;
  int op_code;
  boolean is_special = false;

  /** If non-null, the LambdaExp that this PrimProcedure implements. */
  LambdaExp source;

  java.lang.reflect.Member member;

  public final int opcode() { return op_code; }

  public Type getReturnType () { return retType; }
  public void setReturnType (Type retType) { this.retType = retType; }

  public boolean isSpecial() { return is_special; }

  public Type getReturnType (Expression[] args) { return retType; }

  /** Return true iff the last parameter is a "rest" argument. */
  public boolean takesVarArgs()
  {
    if (method != null)
      {
	String name = method.getName();
	return name.endsWith("$V") || name.endsWith("$V$X");
      }
    return false;
  }

  public boolean takesContext()
  {
    return method != null && takesContext(method);
  }

  public static boolean takesContext(Method method)
  {
    return method.getName().endsWith("$X");
  }

  public int numArgs()
  {
    int num = argTypes.length;
    if (! getStaticFlag())
      num++;
    if (takesContext())
      num--;
    return takesVarArgs() ? (num - 1) + (-1 << 12) : num + (num << 12);
  }

  public int match0 (CallContext ctx)
  {
    return matchN(ProcedureN.noArgs, ctx);
  }

  public int match1 (Object arg1, CallContext ctx)
  {
    Object[] args = { arg1 };
    return matchN(args, ctx);
  }

  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    Object[] args = { arg1, arg2 };
    return matchN(args, ctx);
  }

  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    Object[] args = { arg1, arg2, arg3 };
    return matchN(args, ctx);
  }

  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
		     CallContext ctx)
  {
    Object[] args = { arg1, arg2, arg3, arg4 };
    return matchN(args, ctx);
  }

  public int matchN (Object[] args, CallContext ctx)
  {
    int nargs = args.length;
    boolean takesVarArgs = takesVarArgs();
    int mlength = minArgs() + (takesVarArgs ? 1 : 0);
    int fixArgs = takesVarArgs ? mlength - 1 : mlength;

    if (takesVarArgs)
      {
        if (nargs < fixArgs)
          return NO_MATCH_TOO_FEW_ARGS|fixArgs;
      }
    else
      {
        if (nargs != mlength)
          if (nargs < mlength)
            return NO_MATCH_TOO_FEW_ARGS|fixArgs;
          else
            return NO_MATCH_TOO_MANY_ARGS|fixArgs;
      }
    int arg_count = argTypes.length;
    Type elementType = null;
    Object[] restArray = null;
    int this_count = getStaticFlag() ? 0 : 1;
    boolean takesContext = takesContext();
    int rlen = mlength - this_count;
    Object[] rargs = new Object[rlen + (takesContext ? 1 : 0)];
    if (takesContext)
      rargs[rlen] = ctx;
    Object thisValue;
    if (takesVarArgs)
      {
	Type restType = argTypes[arg_count-1];
	if (restType == Compilation.scmListType)
	  { // FIXME
	    rargs[rlen-1] = gnu.lists.LList.makeList(args, fixArgs);
	    nargs = fixArgs;
	  }
	else
	  {
	    ArrayType restArrayType = (ArrayType) restType;
	    elementType = restArrayType.getComponentType();
	    Class elementClass = elementType.getReflectClass();
	    restArray = (Object[])
	      java.lang.reflect.Array.newInstance(elementClass, nargs-fixArgs);
	    rargs[rlen-1] = restArray;
	  }
      }
    if (this_count != 0)
      {
	try
	  {
	    thisValue = method.getDeclaringClass().coerceFromObject(args[0]);
	  }
	catch (ClassCastException ex)
          {
            return NO_MATCH_BAD_TYPE|1;
          }
      }
    else
      thisValue = null;
    for (int i = this_count;  i < nargs; i++)
      {
        try
          {
            Object arg = args[i];
            Type type = i < fixArgs ? argTypes[i-this_count] : elementType;
            if (type != Type.pointer_type)
              arg = type.coerceFromObject(arg);
            if (i < fixArgs)
              rargs[i-this_count] = arg;
            else
              restArray[i - fixArgs] = arg;
          }
        catch (ClassCastException ex)
          {
            return NO_MATCH_BAD_TYPE|(i+1);
          }
      }
    ctx.value1 = thisValue;
    ctx.values = rargs;
    ctx.proc = this;
    return 0;
  }

  public void apply (CallContext ctx) throws Throwable
  {
    int arg_count = argTypes.length;
    boolean is_constructor = op_code == 183;

    try
      {
	if (member == null)
	  {
	    Class clas = method.getDeclaringClass().getReflectClass();
	    Class[] paramTypes = new Class[arg_count];
	    for (int i = arg_count; --i >= 0; )
	      paramTypes[i] = argTypes[i].getReflectClass();
	    if (is_constructor)
	      member = clas.getConstructor(paramTypes);
	    else
	      member = clas.getMethod(method.getName(), paramTypes);
	  }
	Object result;
	if (is_constructor)
	  result = (((java.lang.reflect.Constructor) member)
		    .newInstance(ctx.values));
	else
	  result = retType.coerceToObject(((java.lang.reflect.Method) member)
					  .invoke(ctx.value1, ctx.values));
	ctx.consumer.writeObject(result);
      }
    catch (java.lang.reflect.InvocationTargetException ex)
      {
	throw ex.getTargetException();
      }
  }

  public PrimProcedure(java.lang.reflect.Method method, Language language)
  {
    this(((ClassType) language.getTypeFor(method.getDeclaringClass()))
         .getMethod(method), language);
  }

  public PrimProcedure(Method method)
  {
    init(method);
    this.retType = method.getName().endsWith("$X") ? Type.pointer_type
      : method.getReturnType();
  }

  public PrimProcedure(Method method, Language language)
  {
    this(method, false, language);
  }

  public PrimProcedure(Method method, boolean is_special, Language language)
  {
    init(method);

    // This stuff deals with that a language may have its own mapping
    // from Java types to language types, for coercions and other reasons.
    Type[] pTypes = method.getParameterTypes();
    int nTypes = pTypes.length;
    argTypes = null;
    for (int i = nTypes;  --i >= 0; )
      {
	Type javaType = pTypes[i];
        if (javaType instanceof ClassType
            && ! ((ClassType) javaType).isExisting())
          continue;
	Type langType = language.getTypeFor(javaType.getReflectClass());
	if (javaType != langType)
	  {
	    if (argTypes == null)
	      {
		argTypes = new Type[nTypes];
		System.arraycopy(pTypes, 0, argTypes, 0, nTypes); 
	      }
	    argTypes[i] = langType;
	  }
      }
    if (argTypes == null)
      argTypes = pTypes;
    if (op_code == 183 && ! is_special)
      retType = method.getDeclaringClass();
    else if (method.getName().endsWith("$X"))
      retType = Type.pointer_type;
    else
      {
        retType = method.getReturnType();
        if (! (retType instanceof ClassType)
            || ((ClassType) retType).isExisting())
          retType = language.getTypeFor(retType.getReflectClass());

        // Kludge - tostring_type doesn't have methods.
        // It shouldn't be used as the "type" of anything -
        // it's just a type with a coercion.  FIXME.
        if (retType == Type.tostring_type)
          retType = Type.string_type;
      }
    if (is_special) {
      this.is_special = true;
      op_code = 183;
    }
  }
  
  private void init(Method method)
  {
    this.method = method;
    this.argTypes = method.getParameterTypes();
    int flags = method.getModifiers();
    if ((flags & Access.STATIC) != 0)
      this.op_code = 184;  // invokestatic
    else
      {
	ClassType mclass = method.getDeclaringClass();
	if ((mclass.getModifiers() & Access.INTERFACE) != 0)
	  this.op_code = 185;  // invokeinterface
	else if ("<init>".equals(method.getName()))
	  this.op_code = 183;  // invokespecial
	else
	  this.op_code = 182;  // invokevirtual
      }
  }

  public PrimProcedure(Method method, LambdaExp source)
  {
    this(method);
    this.retType = source.getReturnType();
    this.source = source;
  }

  public PrimProcedure(int opcode, Type retType, Type[] argTypes)
  {
    this.op_code = opcode;
    this.retType = retType;
    this.argTypes= argTypes;
  }

  public static PrimProcedure makeBuiltinUnary(int opcode, Type type)
  {
    // FIXME - should cache!
    Type[] args = new Type[1];
    args[0] = type;
    return new PrimProcedure(opcode, type, args);
  }

  public static PrimProcedure makeBuiltinBinary(int opcode, Type type)
  {
    // FIXME - should cache!
    Type[] args = new Type[2];
    args[0] = type;
    args[1] = type;
    return new PrimProcedure(opcode, type, args);
  }

  public PrimProcedure(int op_code, ClassType classtype, String name,
		       Type retType, Type[] argTypes)
  {
    this.op_code = op_code;
    method = classtype.addMethod (name, op_code == 184 ? Access.STATIC : 0,
				  argTypes, retType);
    this.retType = retType;
    this.argTypes= argTypes;
  }

  /** Use to compile new followed by constructor. */
  public PrimProcedure(ClassType classtype, Type[] argTypes)
  {
    this(183, classtype, "<init>", Type.void_type, argTypes);
    this.retType = classtype;
  }

  public final boolean getStaticFlag()
  {
    return method == null 
      || method.getStaticFlag()
      || (op_code == 183 && ! is_special);
  }

  public final Type[] getParameterTypes() { return argTypes; }

  /** Compile arguments and push unto stack.
   * @param args arguments to evaluate and push.
   * @param thisType If we are calling a non-static function,
   *   then args[0] is the receiver and thisType is its expected class.
   *   If thisType==Type.void_type, ignore argTypes[0].  (It is used to to
   *   pass a link to a closure environment, which was pushed by our caller.)
   *   If this_type==null, no special handling of args[0] or argTypes[0].
   */
  void compileArgs(Expression[] args, Type thisType, Compilation comp)
 {
    boolean variable = takesVarArgs();
    String name = getName();
    Type arg_type = null;
    gnu.bytecode.CodeAttr code = comp.getCode();
    int skipArg = thisType == Type.void_type ? 1 : 0;
    int arg_count = argTypes.length - skipArg;
    if (takesContext())
      arg_count--;
    boolean is_static = thisType == null || skipArg != 0;
    int fix_arg_count = variable ? arg_count - 1 : args.length;
    Declaration argDecl = source == null ? null : source.firstDecl();
    if (argDecl != null && argDecl.isThisParameter())
      argDecl = argDecl.nextDecl();
    for (int i = 0; ; ++i)
      {
        if (variable && i == fix_arg_count)
          {
            arg_type = argTypes[arg_count-1+skipArg];
	    if (arg_type == Compilation.scmListType)
	      {
		gnu.kawa.functions.MakeList.compile(args, i, comp);
		break;
	      }
            code.emitPushInt(args.length - fix_arg_count);
            arg_type = ((ArrayType) arg_type).getComponentType();
            code.emitNewArray(arg_type);
          }
        if (i >= args.length)
          break;
        if (i >= fix_arg_count)
          {
            code.emitDup(1); // dup array.
            code.emitPushInt(i - fix_arg_count);
          }
        else
          arg_type = argDecl != null && (is_static || i > 0) ? argDecl.getType()
	    : is_static ? argTypes[i + skipArg]
            : i==0 ? thisType
            : argTypes[i-1];
	comp.usedClass(arg_type);
	Target target =
	  source == null ? CheckedTarget.getInstance(arg_type, name, i+1)
	  : CheckedTarget.getInstance(arg_type, source, i);
	args[i].compileNotePosition(comp, target, args[i]);
        if (i >= fix_arg_count)
          code.emitArrayStore(arg_type);
	if (argDecl != null && (is_static || i > 0))
	  argDecl = argDecl.nextDecl();
      }
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    ClassType mclass = method == null ? null :  method.getDeclaringClass();

    // invokespecial == primitive-constructor
    if (opcode() == 183 && ! is_special) 
      {
        code.emitNew(mclass);
        code.emitDup(mclass);
      }
    

    Expression[] args = exp.getArgs();
    String arg_error = WrongArguments.checkArgCount(this, args.length);
    if (arg_error != null)
      comp.error('e', arg_error);

    compile(getStaticFlag() ? null : mclass, exp, comp, target);
  }

  public void compile (Type thisType, ApplyExp exp,
		       Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    gnu.bytecode.CodeAttr code = comp.getCode();
    Type stackType = retType;
    compileArgs(args, thisType, comp);

    if (method == null)
      {
        code.emitPrimop (opcode(), args.length, retType);
        target.compileFromStack(comp, stackType);
      }
    else
      {
        compile(comp, method, target, exp.isTailCall(), op_code, stackType);
      }
  }

  public static void compile (Compilation comp, Method method, Target target,
                              boolean isTailCall, int op_code, Type stackType)
  {
    CodeAttr code = comp.getCode();
    comp.usedClass(method.getDeclaringClass());
    comp.usedClass(method.getReturnType());
    if (! takesContext(method))
      {
        code.emitInvokeMethod(method, op_code);
      }
    else if (target instanceof IgnoreTarget
             || (target instanceof ConsumerTarget
                 && ((ConsumerTarget) target).isContextTarget()))
      {
        comp.loadCallContext();
        code.emitInvokeMethod(method, op_code);
        if (isTailCall)
          {
            comp.loadCallContext();
            code.emitInvoke(Compilation.typeCallContext
                            .getDeclaredMethod("runUntilDone", 0));
          }
        return;
      }
    else
      {
        comp.loadCallContext();
        stackType = Type.pointer_type;
        code.pushScope();
        Variable saveIndex = code.addLocal(Type.int_type);
        comp.loadCallContext();
        code.emitInvokeVirtual(Compilation.typeCallContext.
                               getDeclaredMethod("startFromContext", 0));
        code.emitStore(saveIndex);
        code.emitWithCleanupStart();
        code.emitInvokeMethod(method, op_code);
        code.emitWithCleanupCatch(null);
        comp.loadCallContext();
        code.emitLoad(saveIndex);
        code.emitInvokeVirtual(Compilation.typeCallContext.
                               getDeclaredMethod("cleanupFromContext", 1));
        code.emitWithCleanupDone();
        comp.loadCallContext();
        code.emitLoad(saveIndex);
        code.emitInvokeVirtual(Compilation.typeCallContext.
                               getDeclaredMethod("getFromContext", 1));
        code.popScope();
      }
    target.compileFromStack(comp, stackType);
  }

  public Type getParameterType(int index)
  {
    if (! getStaticFlag())
      {
        if (index == 0)
          return method.getDeclaringClass();
        index--;
      }
    int lenTypes = argTypes.length;
    if (index < lenTypes - 1)
      return argTypes[index];
    boolean varArgs = takesVarArgs();
    if (index < lenTypes && ! varArgs)
      return argTypes[index];
    // if (! varArgs) ERROR;
    Type restType = argTypes[lenTypes - 1];
    if (restType instanceof ArrayType)
      return ((ArrayType) restType).getComponentType();
    else // Should be LList or some other Sequence class.
      return Type.pointer_type;
  }

  // This is null in JDK 1.1 and something else in JDK 1.2.
  private static ClassLoader systemClassLoader
  = PrimProcedure.class.getClassLoader();

  public static PrimProcedure getMethodFor (Procedure pproc, Expression[] args)
  {
    return getMethodFor(pproc, null, args, Language.getDefaultLanguage());
  }

  /** Search for a matching static method in a procedure's class.
   * @return a PrimProcedure that is suitable, or null. */
  public static PrimProcedure getMethodFor (Procedure pproc, Declaration decl,
					    Expression[] args,
					    Language language)
  {
    int nargs = args.length;
    Type[] atypes = new Type[nargs];
    for (int i = nargs;  --i >= 0;) atypes[i] = args[i].getType();
    return getMethodFor(pproc, decl, atypes, language);
  }

  public static PrimProcedure getMethodFor (Procedure pproc, Declaration decl,
					    Type[] atypes, Language language)
  {
    if (pproc instanceof GenericProc)
      {
	GenericProc gproc = (GenericProc) pproc;
	MethodProc[] methods = gproc.methods;
	pproc = null;
	for (int i = gproc.count;  --i >= 0; )
	  {
	    int applic = methods[i].isApplicable(atypes);
	    if (applic < 0)
	      continue;
	    if (pproc != null)
	      return null; // Ambiguous.
	    pproc = methods[i];
	  }
	if (pproc == null)
	  return null;
      }
    if (pproc instanceof PrimProcedure)
      {
	PrimProcedure prproc = (PrimProcedure) pproc;
	if (prproc.isApplicable(atypes) >= 0)
	  return prproc;
      }
    Class pclass = getProcedureClass(pproc);
    if (pclass == null)
      return null;
    return getMethodFor((ClassType) Type.make(pclass), pproc.getName(),
			decl, atypes, language);
  }

  public static Class getProcedureClass (Object pproc)
  {
    Class procClass;
    if (pproc instanceof ModuleMethod)
      procClass = ((ModuleMethod) pproc).module.getClass();
    else
      procClass = pproc.getClass();
    try
      {
	if (procClass.getClassLoader() == systemClassLoader)
	  return procClass;
      }
    catch (SecurityException ex)
      {
      }
    return null;
  }

  /** Get PrimProcedure for matching method in given class. */
  public static PrimProcedure
  getMethodFor (Class procClass, String name, Declaration decl,
                Expression[] args, Language language)
  {
    return getMethodFor((ClassType) Type.make(procClass),
			name, decl, args, language);
  }

  public static PrimProcedure
  getMethodFor (ClassType procClass, String name, Declaration decl,
                Expression[] args, Language language)
  {
    int nargs = args.length;
    Type[] atypes = new Type[nargs];
    for (int i = nargs;  --i >= 0;) atypes[i] = args[i].getType();
    return getMethodFor(procClass, name, decl, atypes, language);
  }

  public static PrimProcedure
  getMethodFor (ClassType procClass, String name, Declaration decl,
		Type[] atypes, Language language)
  {
    PrimProcedure best = null;
    int bestCode = -1;
    boolean bestIsApply = false;
    try
      {
        if (name == null)
          return null;
        String mangledName = Compilation.mangleName(name);
        String mangledNameV = mangledName + "$V";
        String mangledNameVX = mangledName + "$V$X";
        String mangledNameX = mangledName + "$X";
	boolean applyOk = true; // Also look for "apply" and "apply$V".
	for (Method meth = procClass.getDeclaredMethods();
	   meth != null;  meth = meth.getNext())
          {
            int mods = meth.getModifiers();
            if ((mods & (Access.STATIC|Access.PUBLIC))
                != (Access.STATIC|Access.PUBLIC))
	      {
		if (decl == null || decl.base == null)
		  continue;
	      }
            String mname = meth.getName();
	    boolean isApply;
	    if (mname.equals(mangledName)
		|| mname.equals(mangledNameV)
		|| mname.equals(mangledNameX)
		|| mname.equals(mangledNameVX))
	      {
		isApply = false;
	      }
	    else if (applyOk
		     && (mname.equals("apply") || mname.equals("apply$V")))
	      {
		isApply = true;
	      }
            else
              continue;
	    if (! isApply)
	      {
		// If we saw a real match, ignore "apply".
		applyOk = false;
		if (bestIsApply)
		  {
		    best = null;
		    bestCode = -1;
		    bestIsApply = false;
		  }
	      }
	    PrimProcedure prproc = new PrimProcedure(meth, language);
	    prproc.setName(name);
	    int code = prproc.isApplicable(atypes);
	    if (code < 0 || code < bestCode)
	      continue;
	    if (code > bestCode)
	      {
		best = prproc;
	      }
	    else if (best != null)
	      {
		best = (PrimProcedure) MethodProc.mostSpecific(best, prproc);
		if (best == null)
		  { // Ambiguous.
		    if (bestCode > 0)
		      return null;
		  }
	      }
	    bestCode = code;
	    bestIsApply = isApply;
          }
      }
    catch (SecurityException ex)
      {
      }
    return best;
  }

  public String getName()
  {
    String name = super.getName();
    if (name != null)
      return name;
    name = getVerboseName();
    setName(name);
    return name;
  }

  public String getVerboseName()
  {
    StringBuffer buf = new StringBuffer(100);
    if (method == null)
      {
	buf.append("<op ");
	buf.append(op_code);
	buf.append('>');
      }
    else
      {
	buf.append(method.getDeclaringClass().getName());
	buf.append('.');
	buf.append(method.getName());
      }
    buf.append('(');
    for (int i = 0; i < argTypes.length; i++)
      {
	if (i > 0)
	  buf.append(',');
	buf.append(argTypes[i].getName());
      }
    buf.append(')');
    return buf.toString();
  }


  public String toString()
  {
    StringBuffer buf = new StringBuffer(100);
    buf.append(retType.getName());
    buf.append(' ');
    buf.append(getVerboseName());
    return buf.toString();
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<primitive procedure ");
    ps.print(toString());
    ps.print ('>');
  }
}
