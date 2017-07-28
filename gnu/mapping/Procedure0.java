package gnu.mapping;

/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

/**
 * Abstract class for 0-argument procedures.
 * @author	Per Bothner
 */

public abstract class Procedure0 extends Procedure
{
    public Procedure0() {
        super(false, Procedure0.applyToObject);
    }

    public Procedure0(String name) {
        super(false, Procedure0.applyToObject, name);
    }

  public int numArgs() { return 0; }

  public abstract Object apply0 () throws Throwable;

  public Object apply1 (Object arg1)
  {
      throw new WrongArguments(this, 1);
  }

   public Object apply2 (Object arg1,Object arg2)
  { 
    throw new WrongArguments(this, 2);
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
  {
    throw new WrongArguments(this, 3);
  }

  public Object apply4 (Object arg1, Object arg2,
			Object arg3, Object arg4)
  {
    throw new WrongArguments(this, 4);
  }

  public Object applyN (Object[] args) throws Throwable
  {
    if (args.length != 0)
      throw new WrongArguments(this, args.length);
    return apply0 ();
  }

    public static Object applyToObject(Procedure proc, CallContext ctx)
    throws Throwable {
        if (ctx.checkDone() == 0)
            return proc.apply0();
        return ctx;
    }

    public static final MethodHandle applyToObject
        = lookupApplyHandle(Procedure0.class, "applyToObject");
}
