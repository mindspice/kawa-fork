// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.reflect.OccurrenceType;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

public class AppendValues extends MethodProc implements Inlineable
{
    public static final MethodHandle applyToConsumerAV =
        Procedure.lookupApplyHandle(AppendValues.class, "applyToConsumerAV");
     public static final AppendValues appendValues = new AppendValues();

    public AppendValues() {
        super();
        applyToConsumerMethod = applyToConsumerAV;
        setProperty(Procedure.validateApplyKey,
                    "gnu.kawa.functions.CompileMisc:validateApplyAppendValues");
    }

    public static Object applyToConsumerAV(Procedure proc, CallContext ctx) throws Throwable {
        Object endMarker = Special.dfault;
        for (;;) {
            Object arg = ctx.getNextArg(endMarker);
            if (arg == endMarker)
                break;
            if (arg instanceof Consumable)
                ((Consumable) arg).consume(ctx.consumer);
            else
                ctx.writeValue(arg);
        }
        return null;
    }

    public void compile(ApplyExp exp, Compilation comp, Target target) {
        Expression[] args = exp.getArgs();
        int nargs = args.length;

        int nonVoid = -1; // Index of unique non-void argument.
        for (int i = 0;  i < nargs;  i++) {
            if (! args[i].getType().isVoid()) {
                nonVoid = nonVoid == -1 ? i : -2;
            }
        }
        if (nonVoid == -1)
            nonVoid = nargs - 1;
        if (nonVoid >= 0) {
            for (int i = 0;  i < nargs;  i++)
                args[i].compileWithPosition(comp,
                                            i==nonVoid ? target
                                            : Target.Ignore);
            return;
        }

        boolean simple;
        if (target instanceof IgnoreTarget)
            simple = true;
        else if (target instanceof ConsumerTarget) {
            Type type = target.getType();
            simple = type == Type.objectType
                || (type instanceof OccurrenceType
                    && ((OccurrenceType) type).minOccurs() == 0);
        } else
            simple = false;

        if (simple) {
            for (int i = 0;  i < nargs;  i++)
                args[i].compileWithPosition(comp, target);
        } else {
            Expression nexp;
            if (target instanceof ConsumerTarget) {
                nexp = new BeginExp(new Expression[] { exp } );
                nexp.setType(Type.objectType);
            } else
                nexp = exp;
            ConsumerTarget.compileUsingValues(nexp, comp, target);
        }
    }
}
