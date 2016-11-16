package gnu.mapping;

/* #ifdef use:java.lang.invoke */
import java.lang.invoke.MethodHandle;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

/**
 * Abstract class for 3-argument Scheme procedures..
 * @author	Per Bothner
 */

public abstract class Procedure3 extends Procedure
{
    public Procedure3() {
        super(false, Procedure3.applyToObject);
    }

    public Procedure3(String name) {
        super(false, Procedure3.applyToObject, name);
    }

  public int numArgs() { return 0x3003; }

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

  public abstract Object apply3 (Object arg1, Object arg2, Object arg3) throws Throwable;

  public Object apply4 (Object arg1, Object arg2, Object arg3, Object arg4)
  {
    throw new WrongArguments(this, 4);
  }

  public Object applyN (Object[] args) throws Throwable
  {
    if (args.length != 3)
      throw new WrongArguments(this, args.length);
    return apply3 (args[0], args[1], args[2]);
  }

    public static Object applyToObject(Procedure proc, CallContext ctx)
    throws Throwable {
        Object arg0 = ctx.getNextArg();
        Object arg1 = ctx.getNextArg();
        Object arg2 = ctx.getNextArg();
        if (ctx.checkDone() == 0)
            return proc.apply3(arg0, arg1, arg2);
        return ctx;
    }

    public static final MethodHandle applyToObject
        = lookupApplyHandle(Procedure3.class, "applyToObject");
}
