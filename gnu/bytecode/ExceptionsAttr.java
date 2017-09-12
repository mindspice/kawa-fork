package gnu.bytecode;
import java.io.*;

/**
  * Represents the contents of a standard "Exceptions" attribute.
  * @author      Geoff Berry
  */

public class ExceptionsAttr extends Attribute
{
  // The exception types.
  ClassType[] exceptions;
  // The exception table.
  short[] exception_table;

  /** Add a new ExceptionsAttr to a Method. */
  public ExceptionsAttr(Method meth)
  {
    super("Exceptions");
    addToFrontOf(meth);
  }

  /** Set the Exceptions attribute to refer to the given exception types.
    * @param excep_types the types of the exceptions. */
  public void setExceptions (ClassType[] excep_types)
  {
    exceptions = excep_types;
  }

  /** The size of this Attribute (in bytes) is 2 (for
      number_of_exception) plus 2 * number_of_exceptions. */
  public final int getLength()
  {
    return 2 + 2 * (exceptions == null ? 0 : exceptions.length);
  }

  /** The types of the exceptions in this attr. */
  public final ClassType[] getExceptions()
  {
    return exceptions;
  }

  public void write (DataOutputStream dstr) throws java.io.IOException
  {
    int count = exceptions.length;
    dstr.writeShort(count);
    for (int i = 0;  i < count;  i++)
      {
	dstr.writeShort(exception_table[i]);
      }
  }
}
