package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

public class ArrayGet extends Procedure2 implements Inlineable
{
  Type element_type;

  public ArrayGet (Type element_type)
  {
    this.element_type = element_type;
  }

  public Object apply2 (Object array, Object index)
  {
    return java.lang.reflect.Array.get(array, ((Number) index).intValue());
  }
  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    args[0].compile(comp, new ArrayType(element_type));
    args[1].compile(comp, Type.int_type);
    CodeAttr code = comp.getCode();
    code.emitArrayLoad(element_type);
    target.compileFromStack(comp, element_type);
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return element_type;
  }
}
