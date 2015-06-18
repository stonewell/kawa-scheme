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

    //public abstract Object applyN (Object[] args) throws Throwable;

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
