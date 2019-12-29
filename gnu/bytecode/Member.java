// Copyright (c) 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

public interface Member
{
  ClassType getDeclaringClass ();

  String getName ();

  void setName (String name);

  int getModifiers ();

  boolean getStaticFlag ();

    public <T extends java.lang.annotation.Annotation>
    T getAnnotation(Class<T> clas);
}
