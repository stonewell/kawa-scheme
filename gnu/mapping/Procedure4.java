package gnu.mapping;

/* #ifdef use:java.lang.invoke */
import java.lang.invoke.MethodHandle;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

/**
 * Abstract class for 4-argument Scheme procedures.
 * @author	Per Bothner
 */

public abstract class Procedure4 extends Procedure
{
    public Procedure4() {
        super(false, Procedure4.applyToObject);
    }

    public Procedure4(String name) {
        super(false, Procedure4.applyToObject, name);
    }

    public int numArgs() { return 0x4004; }

  public Object apply0 ()
  {
    throw new WrongArguments(this, 0);
  }

  public Object apply1 (Object arg1)
  {
    throw new WrongArguments(this, 1);
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    throw new WrongArguments(this, 2);
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
  {
    throw new WrongArguments(this, 3);
  }

  public abstract Object apply4(Object arg1,Object arg2,
				Object arg3,Object arg4) throws Throwable;

  public Object applyN (Object[] args) throws Throwable
  {
    if (args.length != 4)
      throw new WrongArguments(this, args.length);
    return apply4 (args[0], args[1], args[2], args[3]);
  }

    public static Object applyToObject(Procedure proc, CallContext ctx)
    throws Throwable {
        Object arg0 = ctx.getNextArg();
        Object arg1 = ctx.getNextArg();
        Object arg2 = ctx.getNextArg();
        Object arg3 = ctx.getNextArg();
        if (ctx.checkDone() == 0)
            return proc.apply4(arg0, arg1, arg2, arg3);
        return ctx;
    }

    public static final MethodHandle applyToObject
        = lookupApplyHandle(Procedure4.class, "applyToObject");
}
