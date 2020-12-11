// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.*;
import java.io.*;
import java.lang.annotation.*;
/* #ifdef use:javax.lang.model */
import javax.lang.model.element.*;
/* #endif */
import java.lang.reflect.Proxy;

/** Represents a "RuntimeVisibleParameterAnnotations" or "RuntimeInvisibleParameterAnnotations" attribute. */

public class RuntimeParameterAnnotationsAttr extends Attribute
  /* #ifdef use:javax.lang.model */
  /* FUTURE: implements AnnotationValueVisitor<Object,CodeAttr> */
  /* #endif */
{

    private int dataLength;
    private List<List<AnnotationEntry>> parameterEntries;

    /** Add a new AnnotationAttr to a Member. */
    public RuntimeParameterAnnotationsAttr(String name, AttrContainer container) {
        super(name);
        parameterEntries = new ArrayList<>();
        addToFrontOf(container);
    }

    public static RuntimeParameterAnnotationsAttr
        getAnnotationsAttr(AttrContainer container, String name) {
        Attribute attr = Attribute.get(container, name);
        if (attr != null)
            return (RuntimeParameterAnnotationsAttr) attr;
        return new RuntimeParameterAnnotationsAttr(name, container);
    }

    /** Get or create a RuntimeVisibleParameterAnnotations attribute. */
    public static RuntimeParameterAnnotationsAttr
        getRuntimeVisibleParameterAnnotations (AttrContainer container) {
        return getAnnotationsAttr(container, "RuntimeVisibleParameterAnnotations");
    }

    /** Get or create a RuntimeInvisibleParameterAnnotations attribute. */
    public static RuntimeParameterAnnotationsAttr
        getRuntimeInvisibleParameterAnnotations (AttrContainer container) {
        return getAnnotationsAttr(container, "RuntimeInvisibleParameterAnnotations");
    }

    public <T extends java.lang.annotation.Annotation> T getAnnotation(int parameterIndex, Class<T> clas) {
        if (parameterIndex >= parameterEntries.size())
            return null;
        List<AnnotationEntry> annotations = parameterEntries.get(parameterIndex);
        for (int i = 0;  i < annotations.size();  i++) {
            AnnotationEntry ann = annotations.get(i);
            if (ann.getAnnotationType().getReflectClass() == clas) {
                return (T) Proxy.newProxyInstance(ann.getClass().getClassLoader(), new Class[] { clas }, ann);
            }
        }
        return null;
    }

    public static <T extends java.lang.annotation.Annotation> T getAnnotation(AttrContainer container, int parameterIndex, Class<T> clas) {
        for (Attribute attr = container.getAttributes();
             attr != null;  attr = attr.getNext())  {
            if (attr instanceof RuntimeParameterAnnotationsAttr) {
                T ann = ((RuntimeParameterAnnotationsAttr) attr).getAnnotation(parameterIndex, clas);
                if (ann != null)
                    return ann;
            }
        }
        return null;
    }

    public static void assureParameterSlot(AttrContainer container, int parameterIndex) {
        RuntimeParameterAnnotationsAttr attr;

        attr = getAnnotationsAttr(container, "RuntimeVisibleParameterAnnotations");
        while (attr.parameterEntries.size() <= parameterIndex)
            attr.parameterEntries.add(new ArrayList<>());

        attr = getAnnotationsAttr(container, "RuntimeInvisibleParameterAnnotations");
        while (attr.parameterEntries.size() <= parameterIndex)
            attr.parameterEntries.add(new ArrayList<>());
    }

    /** Add to appropriate annotations attribute.
     * If the annotation's retention policy is {@code RUNTIME},
     * add to the {@code RuntimeVisibleParameterAnnotations} attribute.
     * If the annotation's retention policy is {@code CLASS},
     * add to the {@code RuntimeInvisibleParameterAnnotations} attribute.
     * Otherwise, ignore the annotation.
     */
    public static void maybeAddAnnotation(AttrContainer container, int parameterIndex, AnnotationEntry annotation) {
        RetentionPolicy retention = annotation.getRetention();
        String attrname;
        if (retention == RetentionPolicy.RUNTIME)
            attrname = "RuntimeVisibleParameterAnnotations";
        else if (retention == RetentionPolicy.CLASS)
            attrname = "RuntimeInvisibleParameterAnnotations";
        else
            return;
        getAnnotationsAttr(container, attrname).addAnnotation(parameterIndex, annotation);
    }

    /** Add an annotation to this attribute. */
    public void addAnnotation(int parameterIndex, AnnotationEntry ann) {
        while (parameterIndex >= parameterEntries.size())
            parameterEntries.add(new ArrayList<>());
        parameterEntries.get(parameterIndex).add(ann);
    }

    /** Return the length of the attribute in bytes.
     * Does not include the 6-byte header (for the name_index and the length).*/
    public int getLength() { return dataLength; }

    public void print(ClassTypeWriter dst) {
        dst.print("Attribute \"");
        dst.print(getName());
        dst.print("\", length:");
        dst.print(getLength());
        dst.print(", number of parameters: ");
        dst.println(parameterEntries.size());
        for (int i = 0;  i < parameterEntries.size();  i++) {
            dst.print("Parameter index ");
            dst.println(i);
            for (int j = 0; j < parameterEntries.get(i).size(); j++) {
                dst.printSpaces(2);
                parameterEntries.get(i).get(j).print(2, dst);
                dst.println();
            }
        }
    }

    public void assignConstants(ClassType cl) {
        super.assignConstants(cl);
        dataLength = 1;
        for (int i = 0;  i < parameterEntries.size();  i++) {
            dataLength += 2;
            for (int j = 0; j < parameterEntries.get(i).size(); j++) {
                dataLength += RuntimeAnnotationsAttr.assignConstants(parameterEntries.get(i).get(j), cl.getConstants());
            }
        }
    }

    public void write(DataOutputStream dstr) throws java.io.IOException {
        dstr.writeByte(parameterEntries.size());
        for (int i = 0;  i < parameterEntries.size();  i++) {
            dstr.writeShort(parameterEntries.get(i).size());
            for (int j = 0; j < parameterEntries.get(i).size(); j++) {
                RuntimeAnnotationsAttr.write(parameterEntries.get(i).get(j), getConstants(), dstr);
            }
        }
    }
    
}
