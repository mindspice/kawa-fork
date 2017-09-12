// Copyright (c) 1998, 2004, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/**
 * Maintains the state for generating a switch statement or expression.
 *
 * <h3>Simple example</h3>
 * <p>
 * To translate: <blockquote>
 * 
 * <pre>
 * switch (exps) {
 *   case 1: exp1; break;
 *   case 2: exp2; break;
 *   default: expd;
 * }
 * </pre>
 * 
 * </blockquote> you can do: <blockquote>
 * 
 * <pre>
 * compile[exps]
 * SwitchState sw = code.startSwitch();
 * sw.addCaseOnly(1, code);
 * sw.addCaseOnly(2, code);
 * sw.addCase(1, code);
 * compile[exp1];
 * sw.exitSwitch(code);
 * sw.addCase(2, code);
 * compile[exp2];
 * sw.exitSwitch(code);
 * sw.addDefault(code);
 * compile[expd];
 * sw.finish(code);
 * </pre>
 * 
 * </blockquote>
 */

public class SwitchState
{
    /** The smallest case value, so far. */
    int minValue;

    /** The largest case value, so far. */
    int maxValue;

    /** The number of cases (not including the default case). */
    int numCases;

    /** The case values, in numerical order (in values[0..numCases-1]). */
    int[] values;

    /** The case locations, in the same order as values. */
    Label[] labels;

    /** The location to jump to if none of the cases match. */
    Label defaultLabel;

    /** Location of the actual switch instruction. */
    Label switch_label;

    /**
     * Start of the "cases". This is used to store the type-state for each case.
     */
    Label cases_label;

    /** Code following the switch. */
    Label after_label;

    TryState outerTry;

    int currentCase;

    public int getMaxValue()
    {
        return maxValue;
    }

    public int getNumCases()
    {
        return numCases;
    }

    public SwitchState(CodeAttr code)
    {
        switch_label = new Label(code);
        cases_label = new Label(code);
        after_label = new Label(code);
        defaultLabel = new Label(code);
        outerTry = code.try_stack;

        numCases = 0;
        currentCase = 0;
    }

    /**
     * Needs to be called after the switch value has been pushed. I.e. in
     * execution order, just before the actual switch instruction. Called
     * implicitly by {@link CodeAttr#startSwitch}.
     */
    public void switchValuePushed(CodeAttr code)
    {
        Type top = code.popType(); // pop switch value
        cases_label.setTypes(code);
        code.pushType(top);
        switch_label.setTypes(code);
    }

    void checkEmitSwitch(CodeAttr code)
    {
        if (currentCase == 0)
        {
            switch_label.define(code);

            if (numCases <= 1)
            {
                if (numCases == 1)
                {
                    if (minValue == 0)
                        code.emitIfIntEqZero();
                    else
                    {
                        code.emitPushInt(minValue);
                        code.emitIfEq();
                    }
                    code.emitGoto(labels[0]);
                    code.emitElse();
                    code.emitGoto(defaultLabel);
                    code.emitFi();
                } else
                {
                    code.emitPop(1);
                    code.emitGoto(defaultLabel);
                }
            } else
            {
                code.emit();
                long rangeDim = (long) maxValue - (long) minValue;
                if (2 * numCases >= rangeDim)
                {
                    org.objectweb.asm.Label[] asmLabels = new org.objectweb.asm.Label[maxValue
                        - minValue + 1];
                    int index = 0;
                    // convoluted code in case maxValue==Integer.MAX_VALUE
                    for (int i = minValue;; i++)
                    {
                        Label lab = values[index] == i ? labels[index++] : defaultLabel;
                        code.fixupAdd(lab);
                        asmLabels[i - minValue] = lab.asmLabel;
                        if (i == maxValue)
                            break;
                    }
                    code.getMethod().mv.visitTableSwitchInsn(minValue, maxValue,
                        defaultLabel.asmLabel, asmLabels);
                } else
                {
                    int[] actualValues = new int[numCases];
                    org.objectweb.asm.Label[] asmLabels = new org.objectweb.asm.Label[numCases];
                    for (int i = 0; i < numCases; i++)
                    {
                        actualValues[i] = values[i];
                        asmLabels[i] = labels[i].asmLabel;
                        code.fixupAdd(labels[i]);
                    }
                    code.getMethod().mv.visitLookupSwitchInsn(defaultLabel.asmLabel, actualValues,
                        asmLabels);
                }
                code.popType();
            }
            cases_label.define(code);
            code.fixupAdd(cases_label);
        }
    }

    private Label getCaseLabel(int value, CodeAttr code)
    {
        checkEmitSwitch(code);
        int ind = java.util.Arrays.binarySearch(values, 0, numCases, value);
        if (ind < 0)
            throw new Error("wrong value");
        Label label = labels[ind];
        labels[ind] = null;
        return label;
    }

    /** Emit a new case, for the given value, whose label is here. */
    /**
     * Add a new case.
     * 
     * @param value
     *            the case value to match against at run-time
     * @param code
     *            the CodeAttr of the Method we are generating code for
     * @return true on success; false if value duplicates an existing value
     */
    public boolean addCase(int value, CodeAttr code)
    {
        Label label = getCaseLabel(value, code);
        if (label == null)
            return false;
        label.setTypes(cases_label);
        label.define(code);
        currentCase++;
        return true;
    }

    /** Optimization of {@code addCase(value, code); emitGoto(label)}. */
    public boolean addCaseGoto(int value, CodeAttr code, Label label)
    {
        Label label2 = getCaseLabel(value, code);
        if (label2 == null)
            return false;
        if (label2 != label)
            throw new Error("wrong Label");
        label.setTypes(cases_label);
        code.setUnreachable();
        currentCase++;
        return true;
    }

    public void addDefault(CodeAttr code)
    {
        if (defaultLabel.defined())
            throw new Error();
        if (outerTry != code.try_stack)
            defaultLabel.setTypes(code);
        defaultLabel.setTypes(cases_label);
        defaultLabel.define(code);
    }

    /**
     * Internal routine to add a new case.
     * 
     * @param value
     *            the case value to match against at run-time
     * @param label
     *            the location to go to if the value matches
     * @param code
     *            the CodeAttr of the Method we are generating code for
     * @return true on success; false if value duplicates an existing value
     */
    boolean insertCase(int value, Label label, CodeAttr code)
    {
        if (values == null)
        {
            values = new int[10];
            labels = new Label[10];
            numCases = 1;
            minValue = maxValue = value;
            values[0] = value;
            labels[0] = label;
            return true;
        }

        int[] old_values = values;
        Label[] old_labels = labels;
        int copyBefore;
        if (value < minValue)
        {
            copyBefore = 0;
            minValue = value;
        } else if (value > maxValue)
        {
            copyBefore = numCases;
            maxValue = value;
        } else
        {
            // Binary search.
            int low = 0;
            int hi = numCases - 1;
            copyBefore = 0;
            while (low <= hi)
            {
                copyBefore = (low + hi) >>> 1;
                if (old_values[copyBefore] >= value)
                    hi = copyBefore - 1;
                else
                    low = ++copyBefore;
            }

            if (value == old_values[copyBefore])
                return false;
        }
        if (numCases >= values.length)
        {
            values = new int[2 * numCases];
            labels = new Label[2 * numCases];
        }
        int copyAfter = numCases - copyBefore;
        System.arraycopy(old_values, copyBefore, values, copyBefore + 1, copyAfter);
        System.arraycopy(old_values, 0, values, 0, copyBefore);
        values[copyBefore] = value;
        System.arraycopy(old_labels, copyBefore, labels, copyBefore + 1, copyAfter);
        System.arraycopy(old_labels, 0, labels, 0, copyBefore);
        labels[copyBefore] = label;
        numCases++;
        return true;
    }

    public boolean addCaseOnly(int value, CodeAttr code)
    {
        if (currentCase != 0)
            throw new Error("addCaseOnly cannot be called after addCase");
        Label label = new Label(code);
        return insertCase(value, label, code);
    }

    public boolean addCaseGotoOnly(int value, CodeAttr code, Label label)
    {
        if (currentCase != 0)
            throw new Error("addCaseGotoOnly cannot be called after addCase");
        return insertCase(value, label, code);
    }

    /**
     * Break/exit from this switch. Doesn't allow exiting through a try - if you
     * need that, use an {@link ExitableBlock}.
     */
    public void exitSwitch(CodeAttr code)
    {
        if (code.reachableHere())
        {
            if (outerTry != code.try_stack)
                throw new Error("exitSwitch cannot exit through a try");
            code.emitGoto(after_label);
        }
    }

    /**
     * Handle the end of the switch statement. Assume the case value is on the
     * stack; go to the matching case label.
     */
    public void finish(CodeAttr code)
    {
        if (numCases != currentCase)
            throw new Error("addCaseOnly requires addCase");
        checkEmitSwitch(code);
        if (code.reachableHere())
            exitSwitch(code);
        if (!defaultLabel.defined())
        {
            defaultLabel.define(code);
            ClassType ex = ClassType.make("java.lang.RuntimeException");
            code.emitNew(ex);
            code.emitDup(ex);
            code.emitPushString("bad case value!");
            Type[] args = { Type.string_type };
            Method con = ex.addMethod("<init>", Access.PUBLIC, args, Type.voidType);
            code.emitInvokeSpecial(con);
            code.emitThrow();
        }

        code.setUnreachable();
        if (after_label.isUsed())
            after_label.define(code);
        else
            after_label.defineRaw(code);
    }
}
