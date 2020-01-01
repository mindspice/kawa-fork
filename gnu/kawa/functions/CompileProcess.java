package gnu.kawa.functions;

import gnu.bytecode.ClassType;
import gnu.bytecode.Type;
import gnu.expr.*;
import gnu.mapping.Procedure;

public class CompileProcess {

    /** Type-check and optimize RunProcess (i.e. 'run-process'). */
    public static Expression validateApplyRunProcess
        (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc) {
        if (exp.firstSpliceArg >= 0)
            return exp;
        exp.visitArgs(visitor);
        Expression[] args = exp.getArgs();
        int nargs = args.length;
        int inArg = -1;
        boolean returnBlob = true;
        for (int i = 0;  i < nargs; i++) {
            Expression arg = args[i];
            Keyword key = arg.checkLiteralKeyword();
            if (key != null && i+1 < nargs) {
                String name = key.getName();
                if ("in".equals(name))
                    inArg = i+1;
                if (name.startsWith("out"))
                    returnBlob = false;
                i++;
            } else if (inArg < 0 && i+1 < nargs)
                inArg = i;
        }
        if (inArg >= 0) {
            if (args[inArg] instanceof ApplyExp) {
                ApplyExp inApp = (ApplyExp) args[inArg];
                Object inFun = inApp.getFunction().valueIfConstant();
                if (inFun instanceof RunProcess
                    && inApp.firstKeywordArgIndex <= 1) {
                    Expression[] inArgs = inApp.getArgs();
                    Expression[] xargs = new Expression[inArgs.length+2];
                    xargs[0] = QuoteExp.getInstance(Keyword.make("out-to"));
                    xargs[1] = QuoteExp.getInstance(RunProcess.pipeSymbol);
                    System.arraycopy(inArgs, 0, xargs, 2, inArgs.length);
                    inApp.setArgs(xargs);
                    inApp.adjustSplice(inApp, 2);
                    inApp.firstKeywordArgIndex = 1;
                    inApp.numKeywordArgs += 1;
                }
            }
        }
        /* FUTURE (causes type-checking problems for now)
        // If returnBlob, use a new LangObjType with custom compare method.
        Type t = ClassType.make(returnBlob ? "gnu.kawa.functions.LProcess"
                                : "java.lang.Process");
        exp.setType(t);
        */
        return exp;
    }
}
