package kawa.math;

/** A Quantity represented as the product of a plain double and a Unit.
 * @author	Per Bothner
 */

public class DQuantity extends Quantity
{
  double factor;
  Unit unt;
  public final Unit unit() { return unt; }
  public final Complex number() { return new DFloNum(factor); }

  public final RealNum re() { return new DFloNum (factor); }
  public final double doubleValue() { return factor * unt.factor; }

  public DQuantity (double factor, Unit unit)
  {
    this.factor = factor;
    this.unt = unit;
  }

  public boolean isExact () { return false; }

  public boolean isZero () { return factor == 0.0; }

  public static DQuantity add (DQuantity x, DQuantity y, double k)
  {
    if (x.dimensions() != y.dimensions())
      throw new ArithmeticException ("units mis-match");
    return new DQuantity (x.factor + k * y.factor, x.unit());
  }

  public static DQuantity mul (DQuantity x, DQuantity y)
  {
    double factor = x.factor * y.factor;
    Unit unit = Unit.mul (x.unit(), y.unit());
    return new DQuantity (factor, unit);
  }

  public static DQuantity div (DQuantity x, DQuantity y)
  {
    double factor = x.factor / y.factor; 
    Unit unit = Unit.div (x.unit(), y.unit());
    return new DQuantity (factor, unit);
  }

  public Numeric add (Object y, int k)
  {
    if (y instanceof DQuantity)
      return add (this, (DQuantity) y, (double) k);
    if (dimensions() == Dimensions.Empty && y instanceof RealNum)
      return new DQuantity (factor + k * ((RealNum)y).doubleValue (), unit());
    if (!(y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).add_reversed (this, k);
  }

  public Numeric add_reversed (Numeric x, int k)
  {
    if (dimensions() == Dimensions.Empty && x instanceof RealNum)
      return new DFloNum (((RealNum)x).doubleValue () + k * factor);
    throw new IllegalArgumentException ();
  }

  public Numeric mul (Object y)
  {
    if (y instanceof DQuantity)
      return mul (this, (DQuantity) y);
    if (y instanceof RealNum)
      return new DQuantity (factor * ((RealNum)y).doubleValue (), unit());
    if (!(y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).mul_reversed (this);
  }

  public Numeric mul_reversed (Numeric x)
  {
    if (x instanceof RealNum)
      return new DQuantity (((RealNum)x).doubleValue () * factor, unit());
    throw new IllegalArgumentException ();
  }

  public Numeric div (Object y)
  {
    if (y instanceof DQuantity)
      {
	DQuantity qy = (DQuantity) y;
	if (dimensions() == qy.dimensions())
	  return new DFloNum ((factor * unit().doubleValue())
			      / (qy.factor * qy.unit().factor));
	return div (this, qy);
      }
    if (y instanceof RealNum)
      return new DQuantity (factor / ((RealNum)y).doubleValue (), unit());
    if (!(y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).div_reversed (this);
  }

  public Numeric div_reversed (Numeric x)
  {
    if (x instanceof RealNum)
      return new DQuantity (((RealNum)x).doubleValue () / factor,
			   Unit.div (Unit.Empty, unit()));
    throw new IllegalArgumentException ();
  }
}
