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

/** Represents a "RuntimeVisibleAnnotations" or "RuntimeInvisibleAnnotations" attribute. */

public class RuntimeAnnotationsAttr extends Attribute
  /* #ifdef use:javax.lang.model */
  /* FUTURE: implements AnnotationValueVisitor<Object,CodeAttr> */
  /* #endif */
{
    int dataLength;

    int numEntries;
    AnnotationEntry[] entries;

    /** Add a new AnnotationAttr to a Member. */
    public RuntimeAnnotationsAttr(String name, AnnotationEntry[] entries,
                                  int numEntries, AttrContainer container) {
        super(name);
        this.entries = entries;
        this.numEntries = numEntries;
        addToFrontOf(container);
    }

    public static RuntimeAnnotationsAttr
        getAnnotationsAttr(AttrContainer container, String name) {
        Attribute attr = Attribute.get(container, name);
        if (attr != null)
            return (RuntimeAnnotationsAttr) attr;
        return new RuntimeAnnotationsAttr(name, null, 0, container);
    }

    /** Get or create a RuntimeVisibleAnnotations attribute. */
    public static RuntimeAnnotationsAttr
        getRuntimeVisibleAnnotations (AttrContainer container) {
        return getAnnotationsAttr(container, "RuntimeVisibleAnnotations");
    }

    /** Get or create a RuntimeInvisibleAnnotations attribute. */
    public static RuntimeAnnotationsAttr
        getRuntimeInvisibleAnnotations (AttrContainer container) {
        return getAnnotationsAttr(container, "RuntimeInvisibleAnnotations");
    }

    public <T extends java.lang.annotation.Annotation> T getAnnotation(Class<T> clas) {
        for (int i = 0;  i < numEntries;  i++) {
            AnnotationEntry ann = entries[i];
            if (ann.getAnnotationType().getReflectClass() == clas) {
                return (T) Proxy.newProxyInstance(ann.getClass().getClassLoader(), new Class[] { clas }, ann);
            }
        }
        return null;
    }

    public static <T extends java.lang.annotation.Annotation> T getAnnotation(AttrContainer container, Class<T> clas) {
        for (Attribute attr = container.getAttributes();
             attr != null;  attr = attr.getNext())  {
            if (attr instanceof RuntimeAnnotationsAttr) {
                T ann = ((RuntimeAnnotationsAttr) attr).getAnnotation(clas);
                if (ann != null)
                    return ann;
            }
        }
        return null;
    }

    /** Add to appropriate annotations attribute.
     * If the annotation's retention policy is {@code RUNTIME},
     * add to the {@code RuntimeVisibleAnnotations} attribute.
     * If the annotation's retention policy is {@code CLASS},
     * add to the {@code RuntimeInvisibleAnnotations} attribute.
     * Otherwise, ignore the annotation.
     */
    public static void maybeAddAnnotation(AttrContainer container,
                                           AnnotationEntry annotation) {
        RetentionPolicy retention = annotation.getRetention();
        String attrname;
        if (retention == RetentionPolicy.RUNTIME)
            attrname = "RuntimeVisibleAnnotations";
        else if (retention == RetentionPolicy.CLASS)
            attrname = "RuntimeInvisibleAnnotations";
        else
            return;
        getAnnotationsAttr(container, attrname).addAnnotation(annotation);
    }

    /** Add an annotation to this attribute. */
    public void addAnnotation(AnnotationEntry ann) {
        if (entries == null)
            entries = new AnnotationEntry[4];
        else if (entries.length <= numEntries) {
            AnnotationEntry[] tmp = new AnnotationEntry[2 * entries.length];
            System.arraycopy(entries, 0, tmp, 0, numEntries);
            entries = tmp;
        }
        entries[numEntries++] = ann;
    }

    /** Return the length of the attribute in bytes.
     * Does not include the 6-byte header (for the name_index and the length).*/
    public int getLength() { return dataLength; }
}
