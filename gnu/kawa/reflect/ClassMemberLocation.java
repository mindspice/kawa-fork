package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.mapping.Location;  // As opposed to gnu.bytecode.Location.
import gnu.expr.*;

/** A Location whose value is that of a named field/method of an object.
 * The object is used as the owning Location's value.
 * (For now, only fields are supported.)
 */

public class ClassMemberLocation extends Location
{
  Object instance;
  ClassType type;
  /** Member (method or field) name. */
  String mname;
  java.lang.reflect.Field rfield;

  public ClassMemberLocation(Object instance, ClassType type, String mname)
  {
    this.instance = instance;
    this.type = type;
    this.mname = mname;
  }

  public ClassMemberLocation(Object instance, Class clas, String mname)
  {
    this.instance = instance;
    this.type = (ClassType) Type.make(clas);
    this.mname = mname;
  }

  public String getMemberName()
  {
    return mname;
  }

  public ClassType getDeclaringClass()
  {
    return type;
  }

  public ClassMemberLocation(Object instance, java.lang.reflect.Field field)
  {
    this.instance = instance;
    this.rfield = field;
    this.mname = field.getName();
  }

  void setup ()
  {
    if (rfield == null)
      {
	Class clas;
	try
	  {
	    clas = type.getReflectClass();
	  }
	catch (RuntimeException ex)
	  {
	    throw new UnboundLocationException(null, "Unbound location - "
					       + ex.toString());
	  }
        try
          {
            rfield = clas.getField(mname);
          }
        catch (java.lang.NoSuchFieldException ex)
          {
	    throw new UnboundLocationException(null, "Unbound lolcation "
					       + " - no field " + mname
					       + " in " + type.getName());
          }
      }
  }

  public java.lang.reflect.Field getRField ()
  {
    java.lang.reflect.Field rfld = this.rfield;
    if (rfld == null)
      {
	Class clas
= null;
	try
	  {
	    clas = type.getReflectClass();
            rfld = clas.getField(mname);
	    this.rfield = rfld;
	  }
	catch (Exception ex)
	  {
	    if (id==971)
	      new Error("issing member :"+ex+" type:"+type+" clas:"+clas+" mname:"+mname).printStackTrace();
	    return null;
	  }
      }
    return rfld;
  }

  public Object get (Object defaultValue)
  {
    java.lang.reflect.Field rfld = getRField();
    if (rfield == null)
      return defaultValue;

    try
      {
        return rfield.get(instance);
      }
    catch (IllegalAccessException ex)
      {
        throw new WrappedException(ex);
      }
  }

  public boolean isConstant ()
  {
    java.lang.reflect.Field rfld = getRField();
    return rfld != null && (rfield.getModifiers() & Access.FINAL) != 0;
  }

  public boolean isBound ()
  {
    java.lang.reflect.Field rfld = getRField();
    return rfld != null;
  }

  public void set (Object value)
  {
    setup();
    try
      {
        rfield.set(instance, value);
	return;
      }
    catch (IllegalAccessException ex)
      {
	throw new WrappedException(ex);
      }
    // This is a bit of a kludge  FIXME.
    //setLocation(loc, new TrivialLocation(getEnvironment(loc)));
    //setValue(loc, value);
  }

  public static void define (Object instance,java.lang.reflect.Field rfield,
			     String uri, Interpreter interp, Environment env)
    throws IllegalAccessException
  {
    Object fvalue = rfield.get(instance);
    Type ftype = Type.make(rfield.getType());
    boolean isAlias = ftype == Compilation.typeLocation;
    boolean isProcedure = ftype.isSubtype(Compilation.typeProcedure);
    Object fdname = ((fvalue instanceof Named && ! isAlias)
		     ? ((Named) fvalue).getSymbol()
		     : Compilation.demangleName(rfield.getName(), true));
    Symbol sym;
    if (fdname instanceof Symbol)
      sym = (Symbol) fdname;
    else
      {
	sym = Symbol.make(uri == null ? "" : uri, fdname.toString().
			  intern());
      }
    boolean isFinal = (rfield.getModifiers() & Access.FINAL) != 0;
    if (isAlias && isFinal)
      {
      env.addLocation(sym, null, (Location) fvalue);
      }
    else
      {
	Object property = null;
	if (isFinal)
	  property = interp.getEnvPropertyFor(rfield, fvalue);
	env.addLocation(sym, property,
			new ClassMemberLocation(instance, rfield));
      }
  }

  public static void defineAll(Object instance, Environment env)
    throws IllegalAccessException
  {
    defineAll(instance, null, Interpreter.getInterpreter(), env);
  }

  /** Import all the public fields of an object. */
  public static void defineAll(Object instance, String uri,
			       Interpreter interp, Environment env)
    throws IllegalAccessException
  {
    Class clas = instance.getClass();
    java.lang.reflect.Field[] fields = clas.getFields();
    for (int i = fields.length;  --i >= 0; )
      {
	java.lang.reflect.Field field = fields[i];
	if (field.getName().startsWith(Declaration.PRIVATE_PREFIX))
	      continue;
	define(instance, field, uri, interp, env);
      }
  }

  public static ClassMemberLocation make (/*Object name,*/ String cname, String fldName)
  {
    return make(/*name,*/ null, cname, fldName);
  }

  public static ClassMemberLocation make (/*Object name,*/ Object instance, String cname, String fldName)
  {
    return new ClassMemberLocation(/*name,*/ instance, ClassType.make(cname), fldName);
  }
}
