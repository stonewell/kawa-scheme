// Copyright (c) 2004, 2015  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #endif */

/**
 * The abstract parent for all Scheme functions.
 * @author  Per Bothner
 */

public abstract class Procedure extends PropertySet
{
    /** A static method with signature ??apply(Procedure,CallContext)
     */
    protected /*final?*/ MethodHandle applyToObjectMethod;
    protected /*final?*/ MethodHandle applyToConsumerMethod;

    ///** Does impemention write Object to Consumer?
    // * If so, implement applyToObjectMethod using applyToConsumerMethod.
    // * If false, impement applyToConsumerMethod using applyToObjectMethod.
    // */
    //protected boolean resultGoesToConsumer() {
    //        return false;
    //}

  private static final String sourceLocationKey = "source-location";
  private static final Symbol setterKey = Namespace.EmptyNamespace.getSymbol("setter");

  /** Key for a property used by gnu.expr.Inlinecalls.
   * The property value is either a String of the form "CLASSNAME:METHODNAME",
   * or a java.lang.reflect.Method (or FUTURE: MethodHandle) for a static
   * method whose parameters are
   * {@code (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)} and returns a re-written/validated {@code Expression}.
   */
  public static final Symbol validateApplyKey =
    Namespace.EmptyNamespace.getSymbol("validate-apply");
    /** Same as validateApplyKey but handles splice args. */
  public static final Symbol validateXApplyKey =
    Namespace.EmptyNamespace.getSymbol("validate-xapply");
  public static final Symbol compilerXKey =
    Namespace.EmptyNamespace.getSymbol("compile-apply");

  // This should be a LazyPropertyKey<gnu.expr.Inlineable>, but we want
  // to avoid any strict dependency on gnu.expr for run-time classes.
  public static final LazyPropertyKey<?> compilerKey
  = new LazyPropertyKey("compiler");

  public void setSourceLocation (String file, int line)
  {
    setProperty(sourceLocationKey, file + ":" + line);
  }

  public String getSourceLocation ()
  {
    Object value = getProperty(sourceLocationKey, null);
    return value == null ? null : value.toString();
  }

    /*
    public final Object applyToConsumer(Procedure proc, CallContext ctx) throws Throwable {
        return applyToConsumerMethod.invokeExact(proc, ctx);
    }
    */
    public final MethodHandle getApplyToConsumerMethod() {
        return applyToConsumerMethod;
    }
    public final MethodHandle getApplyToObjectMethod() {
        return applyToObjectMethod;
    }

    public static Object applyToConsumerDefault(Procedure proc, CallContext ctx) throws Throwable {
        //System.err.println("Proc.applyToConsumerDefault proc:"+proc+" ctx.count:"+ctx.count+" ctx.next:"+ctx.next+" state:"+Integer.toHexString(ctx.matchState));
        Object r = proc.applyToObjectMethod.invokeExact(proc, ctx);
        //System.err.println("Proc.applyToConsumerDefault proc:"+proc+" r:"+r);
        if (r != ctx) {
            ctx.consumer.writeObject(r);
            r = null;
        }
        return r;
    }

    public static Object applyToObjectDefault(Procedure proc, CallContext ctx)
            throws Throwable {
        int start = ctx.startFromContext();
        try {
            if (proc.applyToConsumerMethod.invokeExact(proc, ctx) != ctx)
              return ctx.getFromContext(start);
            ctx.cleanupFromContext(start);
            return ctx;
            //Object v = ctx.getFromContext(start);
            //return r == ctx ? null : v;
        } catch (Throwable ex) {
            ctx.cleanupFromContext(start);
            throw ex;
        }
        //return ctx.runUntilValue();
        
    }
    public Procedure() {
    }

    public Procedure(String n) {
        setName(n);
    }

    /* #ifdef use:java.lang.invoke */
    public Procedure(boolean resultGoesToConsumer, MethodHandle applyMethod) {
        if (resultGoesToConsumer) {
            applyToConsumerMethod = applyMethod;
            applyToObjectMethod = applyToObjectDefault;
        } else {
            applyToObjectMethod = applyMethod;
            applyToConsumerMethod = applyToConsumerDefault;
        }
    }
    public Procedure(boolean resultGoesToConsumer, MethodHandle applyMethod, String n) {
        this(resultGoesToConsumer, applyMethod);
        setName(n);
    }
    /* #endif */

    public void checkBadCode(CallContext ctx) {
        int code = 0; // FIXME
	//throw MethodProc.matchFailAsException(code, this, args);
    }

    public Object applyN (Object[] args) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApplyAll(this, args);
        return ctx.runUntilValue();
    }

    public Object apply0() throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApply(this);
        return ctx.runUntilValue();
    }

    public Object apply1(Object arg1) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApply(this, arg1);
        return ctx.runUntilValue();
    }

    public Object apply2(Object arg1,Object arg2) throws Throwable {
        //System.err.println("Proc.apply2 a1:"+arg1+" a2:"+arg2+" p:"+this);
        CallContext ctx = CallContext.getInstance();
        ctx.setupApply(this, arg1, arg2);
        Object r = ctx.runUntilValue();
        //System.err.println("-> Proc.apply2 p:"+this+" r:"+r);
        return r;
    }

    public Object apply3(Object arg1, Object arg2,
                         Object arg3) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApply(this, arg1, arg2, arg3);
        return ctx.runUntilValue();
    }

    public Object apply4(Object arg1, Object arg2,
                         Object arg3, Object arg4) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApply(this, arg1, arg2, arg3, arg4);
        return ctx.runUntilValue();
    }

  /** Minimum number of arguments required. */
  public final int minArgs() { return minArgs(numArgs()); }

  /** Maximum number of arguments allowed, or -1 for unlimited.
   * (May also return -1 if there are keyword arguments, for implementation
   * reasons.) */
  public final int maxArgs() { return maxArgs(numArgs()); }

  /** Return {@code minArgs()|(maxArgs<<12)}.
   * We use a single virtual function to reduce the number of methods
   * in the system, as well as the number of virtual method table entries.
   * We shift by 12 so the number can normally be represented using a
   * sipush instruction, without requiring a constant pool entry.
   */
  public int numArgs() { return 0xfffff000; }

  /** Extract minimum number of arguments from {@code numArgs()} encoding. */
  public static int minArgs (int num) { return num & 0xFFF; }
  /** Extract maximum number of arguments from {@code numArgs()} encoding. */
  public static int maxArgs (int num) { return num >> 12; }

  /** Check that the number of arguments in a call is valid.
    * @param proc the Procedure being called
    * @param argCount the number of arguments in the call
    * @exception WrongArguments there are too many or too
    *     few actual arguments
    */
  public static void checkArgCount(Procedure proc, int argCount)
  {
    int num = proc.numArgs();
    if (argCount < minArgs(num)
	|| (num >= 0 && argCount > maxArgs(num)))
      throw new WrongArguments(proc, argCount);
  }

    int nesting = 0;

  public static void apply (Procedure proc, CallContext ctx) throws Throwable
  {
        ctx.next = 0;
        ctx.matchState = CallContext.MATCH_THROW_ON_EXCEPTION;
        Object ignored = proc.applyToConsumerMethod.invokeExact(proc, ctx);
  }

  public Procedure getSetter()
  {
    if (! (this instanceof HasSetter))
      {
	Object setter = getProperty(setterKey, null);
	if (setter instanceof Procedure)
	  return (Procedure) setter;
	throw new RuntimeException("procedure '"+getName()+ "' has no setter");
      }
    int num_args = numArgs();
    if (num_args == 0x0000)
      return new Setter0(this);
    if (num_args == 0x1001)
      return new Setter1(this);
    return new Setter(this);
  }

  public void setSetter (Procedure setter)
  {
    if (this instanceof HasSetter)
      throw new RuntimeException("procedure '"+getName()+
				 "' has builtin setter - cannot be modified");
    setProperty(Procedure.setterKey, setter);
  }

  /** If HasSetter, the Procedure is called in the LHS of an assignment. */
  public void set0(Object result) throws Throwable
  {
    getSetter().apply1(result);
  }

  public void set1(Object arg1, Object value) throws Throwable
  {
    getSetter().apply2(arg1, value);
  }

  public void setN (Object[] args) throws Throwable
  {
    getSetter().applyN(args);
  }

  /** True if this Procedure (definitely) has no side-effects.
   * Note side-effect-free does not imply idempotent if this
   * allocates an object with "identity".
   */
  public boolean isSideEffectFree ()
  {
    return false;
  }
  
  /** Semi-deprecated - instead should be set at Inline time. FIXME */
  public gnu.bytecode.Type getReturnType (gnu.expr.Expression[] args)
  {
    return gnu.bytecode.Type.objectType;
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append ("#<procedure ");
    String n = getName();
    if (n == null)
      n = getSourceLocation();
    if (n == null)
      n = getClass().getName();
    sbuf.append(n);
    sbuf.append('>');
    return sbuf.toString();
  }

    public static MethodHandle lookupApplyHandle(Class clas, String mname) {
        try {
            return MethodHandles.lookup()
                .findStatic(clas, mname, applyMethodType);
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    public static final MethodType applyMethodType =
        MethodType.methodType(Object.class, Procedure.class, CallContext.class); 
    public static final MethodHandle applyToObjectDefault
        = lookupApplyHandle(Procedure.class, "applyToObjectDefault");
    public static final MethodHandle applyToConsumerDefault
        = lookupApplyHandle(Procedure.class, "applyToConsumerDefault");
}
