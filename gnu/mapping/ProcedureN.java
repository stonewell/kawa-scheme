package gnu.mapping;

/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #endif */

/**
 * Abstract class for "N-argument" Scheme procedures, where N>4 or variable.
 * @author	Per Bothner
 */

public abstract class ProcedureN extends Procedure
{
    /* #ifdef use:java.lang.invoke */
    public ProcedureN() {
        super(false, ProcedureN.applyToObject);
    }

    public ProcedureN(String name) {
        super(false, ProcedureN.applyToObject, name);
    }

    public ProcedureN(boolean resultGoesToConsumer, MethodHandle applyMethod) {
        super(resultGoesToConsumer, applyMethod);
    }
    public ProcedureN(boolean resultGoesToConsumer, MethodHandle applyMethod, String n) {
        super(resultGoesToConsumer, applyMethod, n);
    }
    public ProcedureN(MethodHandle applyMethod) {
        super(false, applyMethod);
    }
    public ProcedureN(MethodHandle applyMethod, String n) {
        super(false, applyMethod, n);
    }
    /* #endif */

    public static final Object[] noArgs = new Object[0];
    /*
  public Object apply0 () throws Throwable
  {
      //if (applyMethod != null)
      //  return super.apply0();
    return applyN(noArgs);
  }

  public Object apply1 (Object arg1) throws Throwable
  {
      //if (applyMethod != null)
      //  return super.apply1(arg1);
    Object[] args = new Object[1];
    args[0] = arg1;
    return applyN (args);
  }

   public Object apply2 (Object arg1,Object arg2) throws Throwable
   {
       // if (applyMethod != null)
       //  return super.apply2(arg1, arg2);
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    return applyN (args);
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3) throws Throwable
  {
      //if (applyMethod != null)
      //  return super.apply3(arg1, arg2, arg3);
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return applyN (args);
  }

  public Object apply4 (Object arg1, Object arg2,
			Object arg3, Object arg4)  throws Throwable
  {
      //if (applyMethod != null)
      //  return super.apply4(arg1, arg2, arg3, arg4);
    Object[] args = new Object[4];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    return applyN (args);
  }
    */

  public abstract Object applyN (Object[] args) throws Throwable;

    public static Object applyToObject(Procedure proc, CallContext ctx)
    throws Throwable {
        Object[] args = ctx.getRestArgsArray();
        if (ctx.checkDone() == 0)
            return proc.applyN(args);
        return ctx;
    }

    public static final MethodHandle applyToObject;
    static {
        try {
            applyToObject = MethodHandles.lookup()
                .findStatic(ProcedureN.class, "applyToObject", applyMethodType);
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }
}
