package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;
import java.lang.reflect.Array;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #endif */

/** Implement the standard Scheme function "apply".
 * This has been generalized so that the last (list argument)
 * can be any sequence, or any primitive array. */

public class Apply extends ProcedureN {
    ApplyToArgs applyToArgs;

    public Apply(String name, ApplyToArgs applyToArgs) {
        super(name);
        this.applyToArgs = applyToArgs;
        this.applyToObjectMethod = applyToObjectApp;
        this.applyToConsumerMethod = applyToConsumerApp;
    }
    
    public int numArgs() { return 0xfffff002; }

    public static Object applyToConsumer(Procedure proc, CallContext ctx) throws Throwable {
        proc = ((Apply) proc).applyToArgs;
        Object last = ctx.popLast();
        ctx.addArgList(last);
        Object r =  proc.getApplyToConsumerMethod().invokeExact(proc, ctx);
        return r;
    }

    public static Object applyToObject(Procedure proc, CallContext ctx) throws Throwable {
        proc = ((Apply) proc).applyToArgs;
        Object last = ctx.popLast();
        ctx.addArgList(last);
        return proc.getApplyToObjectMethod().invokeExact(proc, ctx);
    }

    static final MethodHandle applyToObjectApp =
        Procedure.lookupApplyHandle(Apply.class, "applyToObject");
    static final MethodHandle applyToConsumerApp =
        Procedure.lookupApplyHandle(Apply.class, "applyToConsumer");
}
