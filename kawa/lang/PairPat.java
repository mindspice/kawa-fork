package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;
import java.io.*;

public class PairPat extends Pattern implements Printable, Externalizable
{
  Pattern car;
  Pattern cdr;
  private int car_count, cdr_count;

  public PairPat ()
  {
  }

  public PairPat (Pattern car, Pattern cdr)
  {
    this.car = car;
    this.cdr = cdr;
    car_count = car.varCount ();
    cdr_count = cdr.varCount ();
  }

  public static PairPat make (Pattern car, Pattern cdr)
  {
    return new PairPat (car, cdr);
  }

  public boolean match (Object obj, Object[] vars, int start_vars)
  {
    if (obj instanceof SyntaxForm)
      obj = ((SyntaxForm) obj).form;
    if (! (obj instanceof Pair))
      return false;
    Pair pair = (Pair) obj;
    return (car.match (pair.car, vars, start_vars)
	    && cdr.match (pair.cdr, vars, start_vars + car_count));
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<pair-pattern car: ");
    SFormat.print (car, ps);
    ps.print (" cdr: ");
    SFormat.print (cdr, ps);
    ps.print ('>');
  }

  public int varCount () { return car_count + cdr_count; }

  /**
   * @serialData Write the car and then the cdr patterns (using writeObject).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(car);
    out.writeObject(cdr);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    car = (Pattern) in.readObject();
    cdr = (Pattern) in.readObject();
  }
}
