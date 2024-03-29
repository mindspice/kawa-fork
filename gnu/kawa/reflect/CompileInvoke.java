package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.functions.CompileArith;
import gnu.kawa.functions.CompilationHelpers;
import gnu.kawa.functions.MakeSplice;
import gnu.kawa.lispexpr.LangObjType;
import gnu.math.IntNum;
import java.lang.reflect.Array;

public class CompileInvoke {
    public static Expression validateApplyInvoke
        (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc) {
        Invoke iproc = (Invoke) proc;
        char kind = iproc.kind;
        Compilation comp = visitor.getCompilation();
        Expression[] args = exp.getArgs();
        int nargs = args.length;
        if (nargs == 0 || ((kind == 'V' || kind == '*') && nargs == 1)) {
            // This should never happen, as InlineCalls.visitApplyExp
            // checks the number of arguments before inline is called.
            exp.visitArgs(visitor);
            return exp;
        }
        ObjectType type;
        Expression arg0 = visitor.visit(args[0], null);
        args[0] = arg0;
        Type type0 = (kind == 'V' || kind == '*' ? arg0.getType() : iproc.language.getTypeFor(arg0));
        if (type0 instanceof PairClassType && kind == 'N')
            type = ((PairClassType) type0).instanceType;
        else if (type0 instanceof LangObjType && kind == 'N')
            type = ((LangObjType) type0).getConstructorType();
        else if (type0 instanceof ObjectType)
            type = (ObjectType) type0;
        else
            type = null;
        if (kind == 'P') {
            if (type0 == null)
                comp.error('e', "unknown class for invoke-special", arg0);
            else if (! (type instanceof ClassType) || type.isInterface())
                comp.error('e', "invalid class for invoke-special", arg0);
        }
        String name = getMethodName(args, kind);

        if (kind == 'N' && type == LangObjType.constVectorType
            && required instanceof ArrayType)
            type = (ObjectType) required;
        if (kind == 'N' && type instanceof ArrayType) {
            ArrayType atype = (ArrayType) type;
            Type elementType = atype.getComponentType();
            Expression sizeArg = null;
            boolean lengthSpecified = false;
            for (int i = 0; i < exp.numKeywordArgs; i++) {
                int ia = 2 * i + exp.firstKeywordArgIndex - 1;
                String k =
                    ((Keyword) ((QuoteExp) args[ia]).getValue()).getName();
                if ("length".equals(k) || "size".equals(k)) {
                    sizeArg = args[ia+1];
                    lengthSpecified = true;
                }
            }
            if (sizeArg == null)
                sizeArg = QuoteExp.getInstance(Integer.valueOf(args.length-1));
            sizeArg = visitor.visit(sizeArg, Type.intType);
            Object constantValue = null;
            if (visitor.processingAnnotations()
                && sizeArg instanceof QuoteExp) {
                try {
                    int sz = ((Number) sizeArg.valueIfConstant()).intValue();
                    constantValue = Array.newInstance(elementType.getReflectClass(), sz);
                } catch (Exception ex) {
                    comp.error('e', "bad array size: "+ex.getMessage());
                }
            }
            boolean useArrayMake = exp.numKeywordArgs == 0 && constantValue == null;
            ApplyExp alloc = new ApplyExp(new ArrayNew(elementType),
                                          new Expression[] { sizeArg } );
            alloc.setType(type);
            if (lengthSpecified && args.length == 3)
                return alloc;
            Declaration adecl;
            if (useArrayMake)
                adecl = null;
            else {
                comp.letStart();
                adecl = comp.letVariable((String) null, type, alloc);
                adecl.setCanRead(true);
            }
            BeginExp begin = new BeginExp();
            int index = 0;
            for (int i = lengthSpecified ? 3 : 1; i < args.length;  i++) {
                Expression arg = args[i];
                if (lengthSpecified && i+1 < args.length
                    && arg instanceof QuoteExp
                    && arg.getFlag(QuoteExp.IS_KEYWORD)) {
                    Object key = ((QuoteExp) arg).getValue();
                    if (key instanceof Keyword) {
                        String kname = ((Keyword) key).getName();
                        try {
                            index = Integer.parseInt(kname);
                            arg = args[++i];
                        } catch (Exception ex) {
                            comp.error('e', "non-integer keyword '"+kname+"' in array constructor");
                            return exp;
                        }
                    }
                }
                boolean isSplice = MakeSplice.argIfSplice(arg) != null;
                arg = visitor.visit(arg, isSplice ? null : elementType);
                args[i] = arg;
                if (! (arg instanceof QuoteExp))
                    constantValue = null;
                else if (constantValue != null && ! useArrayMake) {
                    try {
                        Array.set(constantValue, index, arg.valueIfConstant());
                    } catch (Exception ex) {
                        constantValue = null;
                    }
                }
                if (! useArrayMake)
                    begin.add(new ApplyExp(new ArraySet(elementType),
                                           new Expression[] {
                                               new ReferenceExp(adecl),
                                               QuoteExp.getInstance(index),
                                               arg}));
                index++;
            }

            if (constantValue != null)
                return new QuoteExp(constantValue, type);

            if (useArrayMake) {
                Expression[] xargs = new Expression[args.length-1];
                System.arraycopy(args, 1, xargs, 0, xargs.length);
                ApplyExp xexp 
                    = new ApplyExp(ArrayMake.getInstance(elementType), xargs);
                xexp.adjustSplice(exp, -1);
                xexp.setType(atype);
                return xexp;
            }

            begin.add(new ReferenceExp(adecl));
            LetExp let = comp.letDone(begin);
            return let;
        }
        else if (type != null && name != null)
            return validateNamedInvoke(exp, visitor, type, name, null, iproc, required);
        exp.visitArgs(visitor);
        return exp;
    }

    public static Expression validateNamedInvoke(ApplyExp exp, InlineCalls visitor, ObjectType type, String name, PrimProcedure[] methods, Invoke iproc, Type required) {
        Expression[] args = exp.getArgs();
        int nargs = args.length;
        Compilation comp = visitor.getCompilation();
        char kind = iproc.kind;
        int margsLength, argsStartIndex, objIndex;
        if (kind == 'V' || kind == '*') {     // Invoke virtual
            margsLength = nargs - 1;
            argsStartIndex = 2; // Skip receiver and method name.
            objIndex = 0;
        } else if (kind == 'N') {           // make new
            margsLength = nargs;
            argsStartIndex = 0; // Include class specifier
            objIndex = -1;
        } else if (kind == 'S' || kind == 's') { // Invoke static
            margsLength = nargs - 2;
            argsStartIndex = 2; // Skip class and method name.
            objIndex = -1;
        } else if (kind == 'P') {         // Invoke special
            margsLength = nargs - 2;
            argsStartIndex = 3;
            objIndex = 1;
        } else {
            exp.visitArgs(visitor);
            return exp;
        }
        ClassType caller = comp == null ? null
            : comp.curClass != null ? comp.curClass
            : comp.mainClass;
        ObjectType ctype = (ObjectType) type;
        int numCode;
        int keywordStart = kind == 'N' && exp.numKeywordArgs > 0 ? exp.firstKeywordArgIndex - 1 : nargs;
        int tailArgs = nargs - keywordStart;
        int spliceCount = exp.spliceCount();
        boolean none = false;
        try {
            if (methods == null)
                methods = getMethods(ctype, name, caller, iproc);
            if (kind == 'S' || kind == 's') {
                int n = methods.length;
                for (int i = 0; i < n; ) {
                    if (methods[i].getStaticFlag()) {
                        i++;
                    } else {
                        PrimProcedure tmp = methods[n-1];
                        methods[n-1] = methods[i];
                        methods[i] = tmp;
                        n--;
                    }
                }
                if (n == 0)
                    none = true;
                if (n > 0 && n != methods.length) {
                    PrimProcedure[] tmp = new PrimProcedure[n];
                    System.arraycopy(methods, 0, tmp, 0, n);
                    methods = tmp;
                } else if (n == 0 && kind == 's')
                    comp.error('w', "no static method '"+name+"' in "+type.getName());
            }
            numCode = ClassMethods.selectApplicable(methods,
                                                    margsLength - tailArgs - spliceCount,
                                                    spliceCount > 0);
        } catch (Exception ex) {
            comp.error('w', "unknown class: " + type.getName());
            return exp;
        }

        if (kind == 'N') {
            boolean usingConstVector = false;
            if (type == LangObjType.constVectorType) {
                Method defcons;
                ClassType creq;
                if (tailArgs == 0
                    && (required instanceof ClassType || required instanceof ParameterizedType)
                    && (creq = (ClassType) required.getRawType()).isSubclass(Compilation.typeList)
                    && (defcons = creq.getDefaultConstructor()) != null
                    && exp.isSimple()) {
                    ctype = creq;
                    type = ctype;
                    usingConstVector = true;
                    keywordStart = 1;
                    numCode = MethodProc.NO_MATCH_TOO_MANY_ARGS;
                    tailArgs = nargs-1;
                    methods[0] = new PrimProcedure(defcons, iproc.language);
                    args[0] = new QuoteExp(ctype.getReflectClass());
                    if (required instanceof ParameterizedType) {
                        Type[] paramTypes = ((ParameterizedType) required).getTypeArgumentTypes();
                        if (paramTypes.length == 1) {
                            for (int i = args.length; --i > 0; ) {
                                args[i] = Compilation.makeCoercion(args[i], paramTypes[0]);
                            }
                        }
                    }
                }
            }
            if (type instanceof TypeValue) {
                Procedure constructor = ((TypeValue) type).getConstructor();
                if (constructor != null) {
                    Expression[] xargs = new Expression[nargs-1];
                    System.arraycopy(args, 1, xargs, 0, nargs-1);
                    ApplyExp xapp = new ApplyExp(constructor, xargs);
                    xapp.adjustSplice(exp, -1);
                    return visitor.visit(xapp.setLine(exp), required);
                }
            }
            if (exp.firstSpliceArg >= 0) {// FIXME
                exp.visitArgs(visitor);
                return exp;
            }
            CompileBuildObject builder = CompileBuildObject.make(exp, visitor, required, ctype, caller);
            if (usingConstVector) {
                builder.setDefaultConstructor(methods[0]);  
                return builder.build();
            }
            else if (builder.useBuilder(numCode, visitor))
                return builder.build();
        }

        int okCount = 0, maybeCount = 0;
	if (kind == 'N' && tailArgs > 0) {
	    comp.error('w', "args following keyword args but no 'add' method");
        } else if (numCode >= 0) {
            for (int i = 1;  i < nargs; i++) {
                Type atype = null;
                boolean last = i == nargs-1;
                if ((kind == 'P' && i == 2) || (kind != 'N' && i == 1))
                    atype = null; // actually string or symbol
                else if (kind == 'P' && i == 1)
                    atype = ctype;
                else if (numCode > 0 && ! none) {
                    int pi = i - (kind == 'N' ? 1 : argsStartIndex);
                    for (int j = 0;  j < numCode;  j++) {
                        PrimProcedure pproc = methods[j];
                        int pii = pi+(kind!='S'&&pproc.takesTarget()?1:0);
                        // KLUDGE - see explicitArrayAsVarArgsAllowed
                        if (PrimProcedure.explicitArrayAsVarArgsAllowed
                            && last && pproc.takesVarArgs()
                            && pii == pproc.minArgs())
                            atype = null;
                        else {
                            Type ptype = pproc.getParameterType(pii);
                            if (j==0)
                                atype = ptype;
                            else if ((ptype instanceof PrimType) != (atype instanceof PrimType))
                                atype = null;
                            else {
                                atype = Language.unionType(atype, ptype);
                            }
                        }
                        if (atype == null)
                            break;
                    }
                }
                args[i] = visitor.visit(args[i], atype);
            }
            long num = selectApplicable(methods, ctype, args, 
                                        margsLength, argsStartIndex, objIndex);
            okCount = (int) (num >> 32);
            maybeCount = (int) num;
        }
        int nmethods = methods.length;
        if (okCount + maybeCount == 0 && kind == 'N') {
            methods = getMethods(ctype, "valueOf", caller, Invoke.invokeStatic);
            argsStartIndex = 1;
            margsLength = nargs - 1;
            long num = selectApplicable(methods, ctype, args,
                                        margsLength, argsStartIndex, -1);
            okCount = (int) (num >> 32);
            maybeCount = (int) num;
        }
        int index = -1;
        if (okCount + maybeCount == 0) {
            if (kind == 'P' || comp.warnInvokeUnknownMethod()) {
                if (kind=='N')
                    name = name+"/valueOf";
                int min = Integer.MAX_VALUE, max = 0;
                for (int i = methods.length; --i >= 0; ) {
                    int mini = methods[i].minArgs();
                    int maxi = methods[i].maxArgs();
                    if (mini < min)
                        min = mini;
                    if (maxi == -1 || (maxi > max && max != -1))
                        max = maxi;
                }
                int margs = margsLength - exp.spliceCount();
                StringBuilder sbuf = new StringBuilder();
                if (nmethods + methods.length == 0)
                    sbuf.append("no accessible method '");
                else if (numCode == MethodProc.NO_MATCH_TOO_FEW_ARGS) {
                    sbuf.append("too few arguments (");
                    sbuf.append(margs);
                    sbuf.append("; must be ");
                    sbuf.append(min);
                    sbuf.append(") for method '");
                } else if (numCode == MethodProc.NO_MATCH_TOO_MANY_ARGS) {
                    sbuf.append("too many arguments (");
                    sbuf.append(margs);
                    if (exp.spliceCount() > 0)
                        sbuf.append(" or more");
                    sbuf.append("; must be ");
                    sbuf.append(max);
                    sbuf.append(") for method '");
                } else
                    sbuf.append("no possibly applicable method '");
                sbuf.append(name);
                sbuf.append("' in ");
                sbuf.append(type.getName());
                comp.error(kind == 'P' ? 'e' : 'w', sbuf.toString());
            }
        }
        else if (okCount == 1 || (okCount == 0 && maybeCount == 1))
            index = 0;
        else if (okCount > 0) {
            index = PrimProcedure.mostSpecific(methods, okCount);
            if (index < 0
                && (kind == 'P' || comp.warnInvokeUnknownMethod())) {
                StringBuffer sbuf = new StringBuffer();
                sbuf.append("more than one definitely applicable method `");
                sbuf.append(name);
                sbuf.append("' in ");
                sbuf.append(type.getName());
                append(methods, okCount, sbuf);
                comp.error(kind == 'P' ? 'e' : 'w', sbuf.toString());
            }
        } else if (kind == 'P' || comp.warnInvokeUnknownMethod()) {
            StringBuffer sbuf = new StringBuffer();
            sbuf.append("more than one possibly applicable method '");
            sbuf.append(name);
            sbuf.append("' in ");
            sbuf.append(type.getName());
            append(methods, maybeCount, sbuf);
            comp.error(kind == 'P' ? 'e' : 'w', sbuf.toString());
        }
        if (index >= 0) {
            Expression[] margs = new Expression[margsLength];
            PrimProcedure method = methods[index];
            boolean variable = method.takesVarArgs();
            int dst = 0;
            int spdelta = -argsStartIndex;
            if (objIndex >= 0) {
                if (exp.firstSpliceArg == objIndex)
                    spdelta = -objIndex;
                else
                    spdelta++;
                margs[dst++] = args[objIndex];
            }
            for (int src = argsStartIndex; 
                 src < args.length && dst < margs.length; 
                 src++, dst++) {
                margs[dst] = args[src];
            }
            ApplyExp e = new ApplyExp(method, margs);
            e.adjustSplice(exp, spdelta);
            e.setLine(exp);
            if (method.canCompile(e))
                return visitor.visitApplyOnly(e, required);
        }
        exp.visitArgs(visitor);
        return exp;
    }


    private static String getMethodName(Expression[] args, char kind) {
        if (kind == 'N')
            return "<init>";
        int nameIndex = (kind == 'P' ? 2 : 1);
        if (args.length >= nameIndex + 1)
            return ClassMethods.checkName(args[nameIndex], false);
        return null;
    }

    private static void append (PrimProcedure[] methods, int mcount, StringBuffer sbuf) {
        for (int i = 0;  i < mcount;  i++) {
            sbuf.append("\n  candidate: ");
            sbuf.append(methods[i]);
        }
    }

    protected static PrimProcedure[] getMethods(ObjectType ctype, String mname,
                                                ClassType caller, Invoke iproc) {
        int kind = iproc.kind;
        return ClassMethods.getMethods(ctype, mname,
                                       kind == 'P' ? 'P'
                                       : kind == '*' || kind == 'V' ? 'V'
                                       : '\0',
                                       caller, iproc.language);
    }

    private static long selectApplicable(PrimProcedure[] methods,
                                         ObjectType ctype,
                                         Expression[] args, int margsLength, 
                                         int argsStartIndex, int objIndex) {
        Type[] atypes = new Type[margsLength];

        int dst = 0;
        if (objIndex >= 0)
            atypes[dst++] = ctype;
        Type restType = null;
        for (int src = argsStartIndex; 
             src < args.length && dst < atypes.length; 
             src++, dst++) {
            Expression arg = args[src];
            Expression spliceArg = MakeSplice.argIfSplice(arg);
            if (spliceArg != null) {
                restType = Type.objectType; // Could be smarter.  FIXME
                Type[] xtypes = new Type[dst];
                System.arraycopy(atypes, 0, xtypes, 0, dst);
                atypes = xtypes;
                break;
            }
            Type atype = null;
            // Treat IntNum constant argument in int/long range as int/long.
            if (InlineCalls.checkIntValue(arg) != null)
                atype = Type.intType;
            else if (InlineCalls.checkLongValue(arg) != null)
                atype = Type.longType;
            else if (atype == null)
                atype = arg.getType();
            atypes[dst] = atype;
        }
        return ClassMethods.selectApplicable(methods, atypes, restType);
    }

    public static synchronized PrimProcedure
        getStaticMethod(ClassType type, String name, Expression[] args) {
        PrimProcedure[] methods
            = getMethods(type, name, null, Invoke.invokeStatic);
        long num = selectApplicable(methods, type, args, args.length, 0, -1);
        int okCount = (int) (num >> 32);
        int maybeCount = (int) num;
        int index;
        if (methods == null)
            index = -1;
        else if (okCount > 0)
            index = PrimProcedure.mostSpecific(methods, okCount);
        else if (maybeCount == 1)
            index = 0;
        else
            index = -1;
        return index < 0 ? null : methods[index];
    }
}
