// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;

// Should be moved to some other package?  FIXME

/** A class type implemented as a pair of an interface and a class.
 * This is how true muliplte inheritance can be implemented.
 */

public class PairClassType extends ClassType
{
  Object staticLink;

  public ClassType reflectInstanceClass;

  public PairClassType()
  {
  }

  PairClassType(Class reflectInterface, Class reflectInstanceClass)
  {
    super(reflectInterface.getName());
    setExisting(true);
    access_flags |= Access.INTERFACE;
    reflectClass = reflectInterface;
    registerTypeForClass(reflectInterface, this);
    this.reflectInstanceClass = (ClassType) Type.make(reflectInstanceClass);
    //Type.mapNameToType.put(getName(), this);
  }

  public static PairClassType make(Class reflectInterface,
				   Class reflectInstanceClass)
  {
    return new PairClassType(reflectInterface, reflectInstanceClass);
  }

  public static PairClassType make(Class reflectInterface,
				   Class reflectInstanceClass,
				   Object staticLink)
  {
    PairClassType type
      = new PairClassType(reflectInterface, reflectInstanceClass);
    type.staticLink = staticLink;
    return type;
  }

  public Object getStaticLink()
  {
    return staticLink;
  }
}
