package gnu.kawa.functions;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.MethodHandle;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

/**
 * Implement the Scheme standard function "call-with-current-continuation".
 * This is a restricted version, that only works for escape-like applications.
 * @author Per Bothner
 */

public class CallCC extends MethodProc implements Inlineable
{
    public static final MethodHandle applyToConsumerCC
        = lookupApplyHandle(CallCC.class , "applyToConsumerCC");

    public static final CallCC callcc = new CallCC();

    CallCC() {
        super(true, applyToConsumerCC);
        setProperty(Procedure.validateApplyKey,
                    "gnu.kawa.functions.CompileMisc:validateApplyCallCC");
    }

    public int numArgs() { return 0x1001; }

    public static Object applyToConsumerCC(Procedure proc, CallContext ctx)
        throws Throwable {
        Object arg = ctx.getNextArg();
        if (! (arg instanceof Procedure)) {
            ctx.matchError(MethodProc.NO_MATCH_BAD_TYPE);
            return ctx;
        }
        if (ctx.checkDone() != 0)
            return ctx;
 
        Procedure parg = (Procedure) arg;
        Continuation cont = new Continuation(ctx);
        ctx.setupApply(parg, cont);
        try {
            ctx.runUntilDone();
            cont.invoked = true;
        } catch (Exception ex) {
            Continuation.handleException$X(ex, cont, ctx);
        }
        return null;
    }

  /*
  public void apply (CallContext stack)
  {
    kawa.lang.Continuation cont = new Continuation ();
    cont.frame = stack.proc;
    cont.pc = stack.pc;
    stack.value = cont;
  }
  */

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    CompileMisc.compileCallCC(exp, comp, target, this);
  }

  public Type getReturnType (Expression[] args)
  {
    return Type.pointer_type;
  }
}

/*
class Continuation extends MethodProc
{
  Procedure frame;
  int pc;

  public void apply (CallContext stack)
  {
    Object result = Values.make(stack.args);
    stack.pc = pc;
    stack.proc = frame;
    stack.result = result;
  }
}
*/
