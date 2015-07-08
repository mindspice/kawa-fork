package gnu.expr;

import gnu.bytecode.ClassType;
import gnu.bytecode.Type;
import gnu.expr.continuations.Continuation;
import gnu.expr.continuations.Helpers.ContinuationException;
import gnu.expr.continuations.Helpers.ContinuationFrame;
import static gnu.expr.continuations.TopLevelHandler.topLevelHandler;
import static gnu.expr.continuations.CallCC.callcc;
import gnu.kawa.functions.GetNamedPart;
import static gnu.kawa.reflect.Throw.primitiveThrow;
import java.util.HashMap;
import kawa.standard.Scheme;
import kawa.standard.SchemeCompilation;

/**
 * A visitor that fragments the code in a sequence of function calls
 * and performs instrumentation of computation steps to capture and
 * resume continuations.
 *
 * <p>
 * This transformation works on code previously a-normalized (see
 * {@link gnu.expr.ANormalize}). Each let-bind expression is enclosed in
 * a lambda closure that accepts one argument. The argument is an other
 * lambda closure that has in the body the call to the next code fragment.
 * In this way the original source is rewrited as a sequence of function
 * calls, each call representing a computation step.
 * </p>
 *
 * <p>
 * Beside fragmentation, instrumentation is performed using exception
 * handlers. A try-catch expression is created around each computation
 * to capture a possible ContinuationException. The installed exception
 * handler add a new Frame (An invokable object representing the next
 * computation step) to the list of Frames included inside the Exception
 * object, than rethrows the exception.
 * </p>
 *
 * <p>
 * This implementation is inspired to the technique described by
 * Pettyjohn, et al. in "Continuations from Generalized Stack Inspection"
 * and in http://www.ccs.neu.edu/racket/pubs/stackhack4.html
 * </p>
 *
 * <p>
 * An example of the entire transformation is showed below:
 * </p>
 *
 * <p>
 * <blockquote>
 *
 * 1. original source
 * <pre>
 * (define incr #f)
 *
 * (+ (call/cc
 *       (lambda (k)
 *           (set! incr k)
 *           0))
 *   1) ; => 1
 * </pre>
 *
 * 2. after a-normalization
 * <pre>
 * (let ((v1 (lambda (k)
 * 	    (let ((v0 (set! incr k)))
 * 	      0))))
 *  (let ((v2 (call/cc v1)))
 *    (+ v2 1))))
 * </pre>
 *
 * 3. after fragmentation
 * <pre>
 * ((lambda (incr_an1)
 *   (let ((v1 (lambda (k)
 * 	       (let ((v0 (set! incr k)))
 * 		 0))))
 *      (incr_an1 v1)))
 *  (lambda (v1)
 *    ((lambda (incr_an2)
 *       (let ((v2 (call/cc v1)))
 * 	(incr_an2 v2)))
 *     (lambda (v2)
 *       (+ v2 1)))))
 * </pre>
 *
 * 4. after instrumentation
 * <pre>
 * ((lambda (incr_an1)
 *   (let ((v1 (lambda (k)
 * 	       (let ((v0 (set! incr k)))
 * 		 0))))
 *      (incr_an1 v1)))
 *  (lambda (v1)
 *    ((lambda (incr_an2)
 *       (let ((v2 (try-catch (call/cc v1)
 * 		 (cex &lt;ContinuationException&gt;
 * 		      (let ((f (lambda (continue-value)
 * 					(incr_an2 continue-value))))
 * 			(cex:extend (&lt;ContinuationFrame&gt; f))
 * 			(throw cex))))))
 * 	(incr_an2 v2)))
 *     (lambda (v2)
 *       (+ v2 1)))))
 * </pre>
 * </blockquote>
 * </p>
 *
 * @author Andrea Bernardini <andrebask@gmail.com>
 */
public class FragmentAndInstrument extends ExpExpVisitor<Void> {

    HashMap<Declaration, Declaration> renameTable = new HashMap<Declaration, Declaration>();

    static final Expression mapProc;
    static final Expression forEachProc;
    static final Type contExpceptionType;
    static final Expression contFrameClass;
    static final Expression extendPart;

    static {
        mapProc = new ApplyExp(GetNamedPart.getNamedPart,
                                       new QuoteExp(ClassType.make("kawa.lib.normalized_map")),
                                       new QuoteExp("map/cc"));
        forEachProc = new ApplyExp(GetNamedPart.getNamedPart,
                                       new QuoteExp(ClassType.make("kawa.lib.normalized_map")),
                                       new QuoteExp("for-each/cc"));
        contExpceptionType = ClassType.make(ContinuationException.class);
        contFrameClass = new QuoteExp(ContinuationFrame.class);
        extendPart = new QuoteExp("extend");
    }

    public static void fragmentCode(Expression exp, Compilation comp) {
        FragmentAndInstrument visitor = new FragmentAndInstrument();
        visitor.setContext(comp);

        visitor.visit(exp, null);
    }


    protected Expression visitModuleExp(ModuleExp exp, Void ignored) {

        // install the top level handlers to catch ContinuationException
        // and resume the current continuation.
        if (exp.body instanceof ApplyExp
            && ((ApplyExp)exp.body).isAppendValues()) {
            ApplyExp body = ((ApplyExp)exp.body);
            for (int i = 0; i < body.args.length; i++) {
                body.args[i] = installTopLevelHandler(visit(body.args[i], ignored));
            }
            return exp;
        }

        exp.body = installTopLevelHandler(visit(exp.body, ignored));

        return exp;
    }

    private Expression installTopLevelHandler(Expression exp) {
        if (exp instanceof SetExp) {
            SetExp sexp = ((SetExp) exp);
            Expression nv = sexp.new_value;
            if (! (nv instanceof LambdaExp)
                && ! (nv instanceof QuoteExp)
                && ! (nv instanceof ReferenceExp)
                && ! (ANormalize.isDefineProc(nv)))
                  sexp.new_value = wrapInToplevelHandler(sexp.new_value);
            return exp;
        }

        return wrapInToplevelHandler(exp);
    }

    /**
     * Generates the call of the passed expression inside the top
     * level exception handler.
     */
    public Expression wrapInToplevelHandler(Expression exp) {

        Expression initFrame = createFrame(new Declaration("ignored"),
                                          exp);

        ApplyExp init = new ApplyExp(topLevelHandler,
                                     initFrame);
        return init;
    }

    /**
     * Creates a LambdaExp, whose invocation executes the passed expression.
     * argDecl will be the parameter of the lambda (that can be referred in
     * the passed expression).
     */
    protected Expression createFrame(Declaration argDecl, Expression exp) {
        LambdaExp frame = new LambdaExp(1);
        frame.addDeclaration(argDecl);
        frame.body = exp;
        return frame;
    }

    ReferenceExp applyRef = new ReferenceExp(SchemeCompilation.applyFieldDecl);

    protected Expression visitLetExp(LetExp exp, Void ignored) {
        Declaration letDecl = exp.firstDecl();
        Expression nextExp = exp.body;

        if (letDecl == null) return exp;

        Expression continueValue = letDecl.getInitValue();

        // handle a define-procedure
        if (ANormalize.isDefineProc(exp)) {
            BeginExp body = (BeginExp) exp.body;
            for (int i = 0; i < body.exps.length; i++) {
                if (body.exps[i] instanceof ApplyExp)
                    body.exps[i] = visit(body.exps[i], ignored);
            }
            return exp;
        }

        // avoid to fragment and instrument code that cannot throw
        // ContinuationExceptions
        if (continueValue instanceof QuoteExp
            || continueValue instanceof SetExp
            || (continueValue instanceof ReferenceExp
                  && ((ReferenceExp) continueValue).getDontDereference())
            || (continueValue instanceof ReferenceExp
                  && !(exp.body instanceof CaseExp))
            // commented because of "Absent Code attribute ..." error
            //|| continueValue instanceof LambdaExp
            ) {
            letDecl.setInitValue(visit(continueValue, ignored));
            exp.body = visit(exp.body, ignored);
            return exp;
        }

        // avoid to fragment and instrument an apply of a primitive procedure,
        // that cannot throw ContinuationExceptions
        if (continueValue instanceof ApplyExp) {
            Expression f = ANormalize.getApplyFunc((ApplyExp) continueValue);
            if (f instanceof QuoteExp) {
                QuoteExp q = (QuoteExp) f;
                if (q.getValue() instanceof PrimProcedure
                    && !((PrimProcedure)q.getValue()).getDeclaringClass()
                        .toString().contains("kawa.standard.location")) {
                    letDecl.setInitValue(visit(continueValue, ignored));
                    exp.body = visit(exp.body, ignored);
                    return exp;
                }
            }
        }

        // Create the declaration that maps to the next fragment to call.
        Declaration nextFragmentDecl = new Declaration("continue-fragment");
        nextFragmentDecl.setCanCall();
        nextFragmentDecl.setType(Compilation.typeProcedure);

        // Create the current fragment.
        LambdaExp fragment = new LambdaExp(1);
        fragment.body = exp;
        fragment.addDeclaration(nextFragmentDecl);

        // replace the let body with the call to the next fragment.
        exp.body = new ApplyExp(applyRef,
                                new ReferenceExp(nextFragmentDecl),
                                new ReferenceExp(letDecl));
        letDecl.setCanRead();

        // Create the declaration that maps to the value that will be passed
        // to the next fragment.
        Declaration continueValueDecl = new Declaration("continue-value");

        // Create the next fragment
        LambdaExp nextFragment = new LambdaExp(1);
        nextFragment.body = nextExp;
        nextFragment.addDeclaration(continueValueDecl);

        // Create the call of the current fragment, with the next fragment as
        // argument.
        ApplyExp fragmentCall = new ApplyExp(fragment,
                                             nextFragment);

        // visit and wrap in the try-catch the computation of the current fragment.
        Expression annotatedExp = visitAndAnnotate(continueValue, nextFragmentDecl);
        letDecl.setInitValue(annotatedExp);

        // we will need to rename this declaration in the following fragments,
        // so we put it in the rename table.
        renameTable.put(letDecl, continueValueDecl);

        // visit the rest of the code.
        nextFragment.body = visit(nextFragment.body, ignored);

        return fragmentCall;
    }

    /**
     * visit the passed expression, enclosing it, if needed, in a try-catch expression.
     */
    private Expression visitAndAnnotate(Expression exp, Declaration nextFragmentDecl) {

        // expressions in the condition are excluded, they will be NOT wrapped
        // in a try-catch.
        if (!(exp instanceof QuoteExp
              || (exp instanceof ReferenceExp)
              || (exp instanceof LambdaExp))) {

            // Create the TryExp and the handler that catches ContinuationExceptions
            TryExp annotatedExp = new TryExp(exp, null);
            Declaration handlerDecl = new Declaration((Object) null,
                                                      contExpceptionType);
            ReferenceExp handlerDeclRef = new ReferenceExp(handlerDecl);

            // Create the frame with the call to the next fragment.
            Declaration argDecl = new Declaration("continue-value");
            ApplyExp nextFragmentCall = new ApplyExp(applyRef,
                                                     new ReferenceExp(nextFragmentDecl),
                                                     new ReferenceExp(argDecl));
            Expression frame = createFrame(argDecl, nextFragmentCall);

            // Generate the code to create a ContinuationFrame with the frame
            // created above.
            ApplyExp cframe = new ApplyExp(applyRef,
                                           contFrameClass,
                                           frame);
            ApplyExp extend = new ApplyExp(new PrimProcedure("gnu.expr.continuations.Helpers", "extend", 2),
                                           handlerDeclRef,
                                           cframe);

            // Generate the re-throw of the catched Exception.
            ApplyExp throwApply = new ApplyExp(primitiveThrow,
                                               handlerDeclRef);
            throwApply.setType(Type.neverReturnsType);
            Expression begin = new BeginExp(extend, throwApply);

            annotatedExp.addCatchClause(handlerDecl, begin);

            // visit the wrapped expression
            annotatedExp.try_clause = visit(annotatedExp.try_clause, null);
            return annotatedExp;
        } else
            return visit(exp, null);
    }

    public static Declaration followChain(Declaration decl) {
        while (decl != null) {
            Expression declValue = decl.getValue();
            if (!(declValue instanceof ReferenceExp))
                break;
            ReferenceExp rexp = (ReferenceExp) declValue;
            Declaration orig = rexp.binding;
            if (orig == null)
                break;
            decl = orig;
        }
        return decl;
    }

    protected boolean isCallCC(Expression exp) {
        if (exp instanceof ApplyExp) {
            ApplyExp aexp = (ApplyExp) exp;
            if (aexp.getFunctionValue() == gnu.kawa.functions.CallCC.callcc)
                return true;
            if (aexp.args.length > 0
                && aexp.args[0] instanceof ReferenceExp) {
                ReferenceExp ref = (ReferenceExp) aexp.args[0];
                Declaration decl = followChain(ref.getBinding());
                if (decl != null
                    && (decl.getValue() instanceof QuoteExp
                        && ((QuoteExp)decl.getValue()).getValue()
                              == gnu.kawa.functions.CallCC.callcc)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * If the expression is an apply of the map function, replace it with
     * an instrumented version in which continuations can be captured.
     */
    protected void replaceWhenMap(ApplyExp exp) {
        if (exp.args.length > 0
            && exp.args[0] instanceof ReferenceExp) {
            ReferenceExp ref = (ReferenceExp) exp.args[0];
            if (ref.getBinding() != null
                    && ref.getBinding().getValue() instanceof QuoteExp) {
                Object func = ((QuoteExp) ref.getBinding().getValue()).getValue();
                if (func == Scheme.map)
                    exp.args[0] = mapProc;
                else if (func == Scheme.forEach)
                    exp.args[0] = forEachProc;
            }
        }
    }

    protected Expression visitApplyExp(ApplyExp exp, Void ignored) {

        if (isCallCC(exp)) {

            exp.setFunction(callcc);

            setContinuationType(exp.args[1]);

            Expression arg = visit(exp.args[1], ignored);

            exp.args = new Expression[1];

            exp.args[0] = arg;

            return exp;
        }

        if (ANormalize.isGenericProcAdd(exp)) {
            exp.args[1] = visit(exp.args[1], ignored);
            return exp;
        }

        replaceWhenMap(exp);

        return super.visitApplyExp(exp, ignored);
    }

    /**
     * If exp is a LambdaExp or a ReferenceExp pointing to a LambdaExp,
     * sets the lambda argument type to the Continuation Type.
     */
    protected Expression setContinuationType(Expression exp) {
        LambdaExp lexp = null;
        if (exp instanceof ReferenceExp) {
            Declaration d = Declaration.followAliases(((ReferenceExp) exp).getBinding());
            Expression val = (d != null) ? d.getInitValue() : null;
            if (val instanceof LambdaExp)
                lexp = (LambdaExp) val;
        } else if (exp instanceof LambdaExp)
            lexp = (LambdaExp) exp;

        if (lexp == null)
            return exp;

        lexp.getArg(0).setType(Continuation.typeContinuation);
        lexp.firstDecl().setType(Continuation.typeContinuation);
        return exp;
    }

    protected Expression visitReferenceExp(ReferenceExp exp, Void ignored) {
        // check if the binding must be renamed, as an effect of
        // the fragmentation.
        Declaration newDecl = renameTable.get(exp.getBinding());
        if (newDecl != null) {
            exp.setBinding(newDecl);
            exp.setDontDereference(false);
        }
        return exp;
    }

    protected Expression visitSetExp(SetExp exp, Void ignored) {
        // check if the binding must be renamed, as an effect of
        // the fragmentation.
        Declaration newDecl = renameTable.get(exp.getBinding());
        if (newDecl != null)
            exp.setBinding(newDecl);
        exp.new_value = visit(exp.new_value, ignored);
        return exp;
    }
}
