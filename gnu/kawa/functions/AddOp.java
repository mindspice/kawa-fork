// Copyright (c) 2000, 2001, 2003, 2005, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.math.*;
import java.math.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard functions "+" and "-".
 * @author Per Bothner
 */

public class AddOp extends ArithOp
{
  int plusOrMinus = 1;

  public AddOp(String name, int plusOrMinus)
  {
    super(name, plusOrMinus > 0 ? ADD : SUB);
    this.plusOrMinus = plusOrMinus;
    String compiler = plusOrMinus > 0
      ? "gnu.kawa.functions.CompileArith:PLUS"
      : "gnu.kawa.functions.CompileArith:MINUS";
    Procedure.compilerKey.set(this, compiler);
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileArith:validateApplyArithOp");
 }

  public static final AddOp PLUS = new AddOp("+", 1);
  public static final AddOp MINUS = new AddOp("-", -1);

  public static Object apply2(int plusOrMinus, Object arg1, Object arg2)
  {
    int code1 = Arithmetic.classifyValue(arg1);
    int code2 = Arithmetic.classifyValue(arg2);
    /*
    if (code1 < 0 || code2 < 0)
    throw new ClasscastException(); // FIXME
    */
    int code = Arithmetic.leastSpecificCode(code1, code2);
    switch (code)
      {
      case Arithmetic.INT_CODE:
      case Arithmetic.UINT_CODE:
	int i1 = Arithmetic.asInt(arg1);
	int i2 = Arithmetic.asInt(arg2);
        int is = plusOrMinus > 0 ? i1 + i2 : i1 - i2;
	return code == Arithmetic.INT_CODE ? Integer.valueOf(is)
            : UInt.valueOf(is);
      case Arithmetic.LONG_CODE:
      case Arithmetic.ULONG_CODE:
	long l1 = Arithmetic.asLong(arg1);
	long l2 = Arithmetic.asLong(arg2);
        long ls = plusOrMinus > 0 ? l1 + l2 : l1 - l2;
	return code == Arithmetic.INT_CODE ? Long.valueOf(ls)
            : ULong.valueOf(ls);
      case Arithmetic.BIGINTEGER_CODE:
	BigInteger bi1 = Arithmetic.asBigInteger(arg1);
	BigInteger bi2 = Arithmetic.asBigInteger(arg2);
	return plusOrMinus > 0 ? bi1.add(bi2) : bi1.subtract(bi2);
      case Arithmetic.INTNUM_CODE:
	return IntNum.add(Arithmetic.asIntNum(arg1), Arithmetic.asIntNum(arg2),
			  plusOrMinus);
      case Arithmetic.BIGDECIMAL_CODE:
	BigDecimal bd1 = Arithmetic.asBigDecimal(arg1);
	BigDecimal bd2 = Arithmetic.asBigDecimal(arg2);
	return plusOrMinus > 0 ? bd1.add(bd2) : bd1.subtract(bd2);
      case Arithmetic.RATNUM_CODE:
	return RatNum.add(Arithmetic.asRatNum(arg1), Arithmetic.asRatNum(arg2),
			  plusOrMinus);
      case Arithmetic.FLOAT_CODE:
	float f1 = Arithmetic.asFloat(arg1);
	float f2 = Arithmetic.asFloat(arg2);
	return Float.valueOf(plusOrMinus > 0 ? f1 + f2 : f1 - f2);
      case Arithmetic.DOUBLE_CODE:
	double d1 = Arithmetic.asDouble(arg1);
	double d2 = Arithmetic.asDouble(arg2);
	return Double.valueOf(plusOrMinus > 0 ? d1 + d2 : d1 - d2);
      case Arithmetic.FLONUM_CODE:
	d1 = Arithmetic.asDouble(arg1);
	d2 = Arithmetic.asDouble(arg2);
	return new DFloNum(plusOrMinus > 0 ? d1 + d2 : d1 - d2);
      default:
	Numeric num1 = Arithmetic.asNumeric(arg1);
	Numeric num2 = Arithmetic.asNumeric(arg2);
	return num1.add(num2, plusOrMinus);
      }
  }

  public static Object PLUS(Object arg1, Object arg2)
  {
    return apply2(1, arg1, arg2);
  }

  public static Object MINUS(Object arg1, Object arg2)
  {
    return apply2(-1, arg1, arg2);
  }

  public static Object MINUS(Object arg1)
  {
    int code = Arithmetic.classifyValue(arg1);
    switch (code)
      {
      case Arithmetic.INT_CODE:
	return Integer.valueOf(- Arithmetic.asInt(arg1));
      case Arithmetic.UINT_CODE:
	return UInt.valueOf(- Arithmetic.asInt(arg1));
      case Arithmetic.LONG_CODE:
	return Long.valueOf(- Arithmetic.asLong(arg1));
      case Arithmetic.ULONG_CODE:
	return ULong.valueOf(- Arithmetic.asLong(arg1));
      case Arithmetic.BIGINTEGER_CODE:
	return Arithmetic.asBigInteger(arg1).negate();
      case Arithmetic.INTNUM_CODE:
	return IntNum.neg(Arithmetic.asIntNum(arg1));
      case Arithmetic.BIGDECIMAL_CODE:
	return Arithmetic.asBigDecimal(arg1).negate();
      case Arithmetic.RATNUM_CODE:
	return RatNum.neg(Arithmetic.asRatNum(arg1));
      case Arithmetic.FLOAT_CODE:
	return Float.valueOf(- Arithmetic.asFloat(arg1));
      case Arithmetic.DOUBLE_CODE:
	return Double.valueOf(- Arithmetic.asDouble(arg1));
      case Arithmetic.FLONUM_CODE:
	return DFloNum.valueOf(- Arithmetic.asDouble(arg1));
      default:
        return Arithmetic.asNumeric(arg1).neg();
      }

  }

  public static Object PLUS$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return applyN(1, apply2(1,apply2(1, arg1, arg2), arg3), rest);
  }

  public static Object MINUS$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return applyN(-1, apply2(-1,apply2(-1, arg1, arg2), arg3), rest);
  }

  public static Object applyN(int plusOrMinus, Object[] args)
  {
    int len = args.length;
    if (len == 0) {
      if (plusOrMinus < 0)
        throw new WrongArguments(MINUS, 0);
      return IntNum.zero ();
    }
    Object result = args[0];
    if (len == 1 && plusOrMinus < 0)
      return MINUS(result);
    for (int i = 1; i < len; i++)
      result = apply2(plusOrMinus, result, args[i]);
    return result;
  }

  public static Object applyN(int plusOrMinus, Object init, Object[] args)
  {
    int len = args.length;
    Object result = init;
    for (int i = 0; i < len; i++)
      result = apply2(plusOrMinus, result, args[i]);
    return result;
  }

  public Object applyN (Object[] args)
  {
    return applyN(plusOrMinus, args);
  }

  public int numArgs()
  {
    return plusOrMinus < 0 ? 0xfffff001 : 0xfffff000;
  }
}
