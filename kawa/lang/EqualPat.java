package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;

/**
 * A pattern that requires an exact match (using equal?).
 */

public class EqualPat extends Pattern implements Printable, Externalizable
{

  Object value;

  public EqualPat () { }

  public EqualPat (Object obj) { value = obj; }

  static public EqualPat make (Object obj) { return new EqualPat (obj); }

  public boolean match (Object obj, Object[] vars, int start_vars) {
    return value.equals (obj);
  }

  public int varCount () { return 0; }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<equals: ");
    ps.print(value);
    ps.print('>');
  }

  /**
   * @serialData Write the value (using writeObject).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(value);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    value = in.readObject();
  }
}
