package kawa.lang;
import codegen.*;
import java.util.Hashtable;

/** A primitive Procedure implemented by a plain Java method. */

public class PrimProcedure extends ProcedureN
{
  Type retType;
  Type[] argTypes;
  Method method;
  int op_code;

  public final int opcode() { return op_code; }

  public Object applyN (Object[] args)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new GenericError("apply not implemented for PrimProcedure");
  }

  public PrimProcedure(Method method)
  {
    this.method = method;
    this.argTypes = method.getParameterTypes();
    this.retType = method.getReturnType();
  }

  public PrimProcedure(int opcode, Type retType, Type[] argTypes)
  {
    this.op_code = opcode;
    this.retType = retType;
    this.argTypes= argTypes;
  }

  public PrimProcedure(int op_code, ClassType classtype, String name,
		       Type retType, Type[] argTypes)
  {
    this.op_code = op_code;
    method = classtype.new_method (name, argTypes, retType,
				   op_code == 184 ? Access.STATIC : 0);
    this.retType = retType;
    this.argTypes= argTypes;
  }

  /** Use to compile new followed by constructor. */
  public PrimProcedure(ClassType classtype, Type[] argTypes)
  {
    this(183, classtype, "<init>", Type.void_type, argTypes);
    this.retType = classtype;
  }

  public final boolean getStaticFlag()
  {
    return method == null || method.getStaticFlag() || op_code == 183;
  }

  public final Type[] getParameterTypes() { return argTypes; }

  static Hashtable types;

  public static Type string2Type (String name)
  {
    if (types == null)
      {
	types = new Hashtable ();
	types.put ("void", Type.void_type);
	types.put ("int", Type.int_type);
	types.put ("char", Type.char_type);
	types.put ("boolean", Type.boolean_type);
	types.put ("byte", Type.byte_type);
	types.put ("short", Type.short_type);
	types.put ("long", Type.long_type);
	types.put ("float", Type.float_type);
	types.put ("double", Type.double_type);

	types.put ("Object", Type.pointer_type);
	types.put ("java.lang.Object", Type.pointer_type);
	types.put ("String", Type.string_type);
	types.put ("java.lang.String", Type.string_type);
      }
    Type t = (Type) types.get(name);
    if (t != null)
      return t;
    t = new ClassType (name);
    types.put (name, t);
    return t;
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer(100);
    buf.append(retType.getName());
    if (method == null)
      {
	buf.append("<op ");
	buf.append(op_code);
	buf.append('>');
      }
    else
      {
	buf.append(' ');
	buf.append(method.getDeclaringClass().getName());
	buf.append('.');
	buf.append(method.getName());
      }
    buf.append('(');
    for (int i = 0; i < argTypes.length; i++)
      {
	if (i > 0)
	  buf.append(',');
	buf.append(argTypes[i].getName());
      }
    buf.append(')');
    return buf.toString();
  }
}
