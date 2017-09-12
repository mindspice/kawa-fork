// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.*;
import java.lang.reflect.Array;
import java.lang.reflect.Proxy;
import java.lang.annotation.*;
/* #ifdef use:javax.lang.model */
import javax.lang.model.element.*;
/* #endif */

/** An annotation value mirror. */

public class AnnotationEntry
implements java.lang.reflect.InvocationHandler
/* #ifdef JAVA5 */
, java.lang.annotation.Annotation
/* #endif */
/* #ifdef use:javax.lang.model */
/* FUTURE also implements: javax.lang.model.element.AnnotationMirror */
/* #endif */
{
  ClassType annotationType;
  int annotationTypeIndex;

  public RetentionPolicy getRetention ()
  {
    Annotation retention = getAnnotationType().getReflectClass()
      .getAnnotation(Retention.class);
    if (retention == null)
      return RetentionPolicy.CLASS;
    return ((Retention) retention).value();
  }

  /** Is there is a {@code @Target} meta-annotation that includes the parameter?
   * If the annotationType has no {@code @Target} meta-annotation, return true,
   * since in that case the annotation type is allowed in all contexts.
   * If {@code etype==null}, return false iff there is a {@code @Target} meta-annotation.
   */
  public boolean hasTarget (ElementType etype)
  {
    Annotation target = getAnnotationType().getReflectClass()
      .getAnnotation(Target.class);
    if (target == null)
      return true;
    if (etype != null)
      {
        ElementType[] etypes = (ElementType[]) ((Target) target).value();
        for (int i = etypes.length;  --i >= 0; )
          if (etypes[i] == etype)
            return true;
      }
    return false;
  }

  public AnnotationEntry ()
  {
  }

  public AnnotationEntry (ClassType annotationType)
  {
    this.annotationType = annotationType;
  }

  LinkedHashMap<String,Value> elementsValue = new LinkedHashMap<String,Value>(10);

  public ClassType getAnnotationType ()
  {
    return annotationType;
  }

  public void addMember(String name, Value value)
  {
    elementsValue.put(name, value);
  }

  public void addMember(String name, Object value, Type type)
  {
      elementsValue.put(name, asAnnotationValue(value, type));
  }

  public static Value asAnnotationValue(Object val, Type type)
  {
    String sig = type.getSignature();
    char kind = sig.charAt(0);
    switch (kind)
      {
      case 'B': val= ((Number) val).byteValue();  break;
      case 'S': val= ((Number) val).shortValue();  break;
      case 'I': val= ((Number) val).intValue();  break;
      case 'J': val = ((Number) val).longValue();  break;
      case 'L':
        if (sig.equals("Ljava/lang/String;"))
          {
            kind = 's';
            val = ((CharSequence) val).toString();
          }
        else if (sig.equals("Ljava/lang/Class;"))
          {
            kind = 'c';
            if (val instanceof Type)
              val = (Type) val;
            else
              val = Type.make((Class) val);
          }
        else if (((ClassType) type).isSubclass("java.lang.Enum"))
          {
            kind = 'e';
          }
        else if (((ClassType) type).implementsInterface(Type.javalangannotationAnnotationType))
          {
            kind = '@';
            val = (AnnotationEntry) Proxy.getInvocationHandler(val);
          }
        break;
      case '[':
        Type eltype = ((ArrayType) type).getComponentType();
        List<AnnotationEntry.Value> alist = new ArrayList<AnnotationEntry.Value>();
        if (val instanceof List<?>)
          {
            List<?> lst = (List<?>) val;
            int len = lst.size();
            for (int i = 0;  i < len; i++)
              alist.add(asAnnotationValue(lst.get(i), eltype));
          }
        else
          {
            int len = Array.getLength(val);
            for (int i = 0;  i < len; i++)
              alist.add(asAnnotationValue(Array.get(val, i), eltype));
          }
        val = alist;
        break;
      }
    return new AnnotationEntry.Value(kind, type, val);
  }


  /* #ifdef JAVA5 */
  @SuppressWarnings("unchecked")
  /* #endif */
  public Class<? extends java.lang.annotation.Annotation> annotationType ()
  {
    return (Class<? extends java.lang.annotation.Annotation>) annotationType.getReflectClass();
  }

  /* FUTURE
  public Map<Member,AnnotationValue> getElementValues()
  {
      convert from elementsValue;
  }
  */

  public boolean equals(Object obj)
  {
    if (! (obj instanceof AnnotationEntry))
      return false;
    AnnotationEntry other = (AnnotationEntry) obj;
    if (! getAnnotationType().getName().equals(other.getAnnotationType().getName()))
      return false;
    for (Map.Entry<String,Value> it : elementsValue.entrySet())
      {
        String key = it.getKey();
        Value value1 = it.getValue();
        Value value2 = other.elementsValue.get(key);
        if (value1 != value2)
          {
            if (value1 == null || value2 == null
                || ! value1.equals(value2))
              return false;
          }
      }
    for (Map.Entry<String,Value> it : other.elementsValue.entrySet())
      {
        String key = it.getKey();
        Object value2 = it.getValue();
        Object value1 = elementsValue.get(key);
        if (value1 != value2)
          {
            if (value1 == null || value2 == null
                || ! value1.equals(value2))
              return false;
          }
      }
    return true;
  }

  public int hashCode()
  {
    int hash = 0;
    // Note the Annotation spec requires we also include the
    // hashCode of members with default values; I don't think we do that.
    for (Map.Entry<String,Value> it : elementsValue.entrySet())
      {
        int khash = it.getKey().hashCode();
        int vhash = it.getValue().hashCode();
        hash += 127 * khash ^ vhash;
      }
    return hash;
  }

  public String toString()
  {
    StringBuilder sbuf = new StringBuilder();
    sbuf.append('@');
    sbuf.append(getAnnotationType().getName());
    sbuf.append('(');
    int count = 0;
    for (Map.Entry<String,Value> it : elementsValue.entrySet())
      {
        if (count > 0)
          sbuf.append(", ");
        sbuf.append(it.getKey());
        sbuf.append('=');
        sbuf.append(it.getValue());
        count++;
      }
    sbuf.append(')');
    return sbuf.toString();
  }

  public Object invoke (Object proxy, java.lang.reflect.Method method, Object[] args)
  {
    String mname = method.getName();
    int nargs = args == null ? 0 : args.length;
    if (mname.equals("toString") && nargs == 0)
      return this.toString();
    if (mname.equals("hashCode") && nargs == 0)
      return this.hashCode();
    return elementsValue.get(mname).getValue();
  }

  public static class Value
  /* #ifdef use:javax.lang.model */
  implements AnnotationValue
  /* #endif */
  {
    Type type;

    /** Either one of the standard primitize signature code
     * B (byte), S (short), I (int) , J (long), F (float), D (double),
     * Z (boolean), C (char).
     * or:
     * 'e' (enum-constant)
     * '[' (array)
     * '@' (type)
     * 's' (string)
     * 'c' (string)
     */
    char kind;

    Object value;
    Object valuex;

    public Value (char kind, Type type, Object value)
    {
      this.kind = kind;
      this.type = type;
      this.value = value;
    }

    /** Get an Object representing the annotation value.
     * If the kind is 'e', the value is *either* a Field or an 2-element
     * array [ClassName, EnumName] or the actual Enum value.
     * If kind is 'c', the value is *either* a ClassType or a String.
     */
    public Object getValue()
    {
      if (kind == '[')
        {
          if (valuex == null)
            {
              List<? extends AnnotationEntry.Value> lvalue =
                (List<? extends AnnotationEntry.Value>) value;
              int n = lvalue.size();
              Class eltype = type.getReflectClass().getComponentType();
              Object arr = Array.newInstance(eltype, n);
              for (int i = 0;  i < n;  i++)
                Array.set(arr, i, lvalue.get(i).getValue());
              valuex = arr;
            }
          return valuex;
        }
      else if (kind == 'e') {
          if (valuex == null) {
              if (value instanceof Enum) {
                  valuex = value;
              } else {
                  ClassType type;
                  String name;

                  if (value instanceof Field) {
                      Field f = (Field) value;
                      type = f.getDeclaringClass();
                      name = f.getName();
                  } else {
                      String[] sarr = (String[]) value;
                      type = (ClassType) Type.signatureToType(sarr[0]);
                      name = sarr[1];
                  }
                  Class<?> clas = type.getReflectClass();
                  Class<? extends Enum> eclas = clas.asSubclass(Enum.class);
                  Enum val = Enum.valueOf(eclas, name);
                  valuex = val;
              }
          }
          return valuex;
      }
      // FIXME other conversions needed?
      return value;
    }

    public String toString() { return getValue().toString(); }
 
    /* #ifdef use:javax.lang.model */
    public <R,P> R accept(AnnotationValueVisitor<R,P> v, P p)
    {
      switch (kind)
        {
        case 'Z':  return v.visitBoolean(((Boolean) value).booleanValue(), p);
        case 'C':  return v.visitChar(((Character) value).charValue(), p);
        case 'B':  return v.visitByte(((Byte) value).byteValue(), p);
        case 'S':  return v.visitShort(((Short) value).shortValue(), p);
        case 'I':  return v.visitInt(((Integer) value).intValue(), p);
        case 'J':  return v.visitLong(((Long) value).longValue(), p);
        case 'F':  return v.visitFloat(((Float) value).floatValue(), p);
        case 'D':  return v.visitDouble(((Double) value).doubleValue(), p);
        case 's':  return v.visitString((String) value, p);
        case '[': 
          return v.visitArray((List<? extends AnnotationValue>) value, p);
        case 'e': /* FIXME:   return v.visitEnumConstant((Field) value, p);*/
        case '@':
        default:
          throw new UnsupportedOperationException();
        }
    }
    /* #endif */
  }
}
