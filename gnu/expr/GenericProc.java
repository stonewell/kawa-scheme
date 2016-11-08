// Copyright (c) 1999, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.Type;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #endif */

/** A collection of MethodProcs;  one is chosen at apply time. */

public class GenericProc extends MethodProc
{
    protected MethodProc[] methods;
    int count;
    int minArgs;
    int maxArgs;

    public GenericProc(String name) {
        super(true, applyToConsumerGP);
        setName(name);
    }

    public GenericProc() {
        super(true, applyToConsumerGP);
    }

  public int getMethodCount ()
  {
    return count;
  }

  public MethodProc getMethod (int i)
  {
    return i >= count ? null : methods[i];
  }

  public int numArgs()
  {
    return minArgs | (maxArgs << 12);
  }

  protected synchronized void addAll (MethodProc[] procs)
  {
    int n = procs.length;
    if (methods == null)
      methods = new MethodProc[n];
    for (int i = 0;  i < n;  i++)
      add(procs[i]);
  }

  public synchronized void addAtEnd (MethodProc method)
  {
    int oldCount = count;
    if (methods == null)
      methods = new MethodProc[8];
    else if (oldCount >= methods.length)
      {
        MethodProc[] copy = new MethodProc[2 * methods.length];
        System.arraycopy(methods, 0, copy, 0, oldCount);
        methods = copy;
      }

    methods[oldCount] = method;

    int n = method.minArgs();
    if (n < minArgs || count==0)
      minArgs = n;
    n = method.maxArgs();
    if ((n < 0 || n > maxArgs) && maxArgs >= 0)
      maxArgs = n;
    count = ++oldCount;
  }

  public synchronized void add(MethodProc method)
  {
    int oldCount = count;
    addAtEnd(method);

    for (int i = 0;  i < oldCount;  i++)
      {
	MethodProc best = MethodProc.mostSpecific(method, methods[i]);
	if (best == method)
          {
            System.arraycopy(methods, i, methods, i + 1, oldCount - i);
            methods[i] = method;
            break;
          }
      }
  }

    @Override
    public int isApplicable(Type[] args, Type restType) {
    int best = -1;
    for (int i = count;  --i >= 0; )
      {
        MethodProc method = methods[i];
        int result = method.isApplicable(args, restType);
        if (result == 1)
          return 1;
        if (result == 0)
          best = 0;
      }
    return best;
  }

    // FIXME also implement applyToObjectGP
    public static Object applyToConsumerGP(Procedure proc, CallContext ctx)
        throws Throwable {
        GenericProc gproc = (GenericProc) proc;
        MethodProc[] methods = gproc.methods;
        int count = gproc.count;
        if (count == 0)
            return methods[0].getApplyToConsumerMethod().invokeExact((Procedure) methods[0], ctx);
        int startState = ctx.getMode();
        //System.err.println("applyToConsumerGP "+proc+" state:"+startState+"  c.proc:"+ctx.proc+" consumer:"+ctx.consumer);
        int methodState = startState;
        if (startState == CallContext.MATCH_THROW_ON_EXCEPTION)
            methodState = CallContext.MATCH_CHECK;
        for (int i = 0;  i < count;  i++) {
            Procedure method = methods[i];
            ctx.rewind(methodState);
            Object r = method.getApplyToConsumerMethod().invokeExact(method, ctx);
            if (r != ctx) {
                //System.err.println("->applyToConsumerGP r:"+r+" state:"+Integer.toHexString(ctx.getMode())+" c.p:"+ctx.proc+" method:"+method+" app:"+java.lang.invoke.MethodHandles.lookup().revealDirect(method.getApplyToConsumerMethod())+" consumer:"+ctx.consumer+"::"+ctx.consumer.getClass().getName());
                //ctx.consumer.dump();
                return r;
            }
        }
        ctx.rewind(startState);
        ctx.matchError(NO_MATCH);
        return ctx;
    }

 public void setProperty (Keyword key, Object value)
  {
    String name = key.getName();
    if (name == "name")
      setName(value.toString());
    else if (name == "method")
      add((MethodProc) value);
    else
      super.setProperty(key.asSymbol(), value);
  }

  public final void setProperties (Object[] args)
  {
    int alen = args.length;
    for (int i = 0;  i < alen;  i++)
      {
	Object arg = args[i];
	if (arg instanceof Keyword)
          setProperty((Keyword) arg, args[++i]);
	else
	  add((MethodProc) arg);
      }
  }

  /** Create a GenericProc from one or more methods, plus properties. */
  public static GenericProc make (Object[] args)
  {
    GenericProc result = new GenericProc();
    result.setProperties(args);
    return result;
  }

  public static GenericProc makeWithoutSorting (Object... args)
  {
    GenericProc result = new GenericProc();

    int alen = args.length;
    for (int i = 0;  i < alen;  i++)
      {
	Object arg = args[i];
	if (arg instanceof Keyword)
          result.setProperty((Keyword) arg, args[++i]);
	else
	  result.addAtEnd((MethodProc) arg);
      }

    return result;
  }
    public static final MethodHandle applyToConsumerGP;
    static {
        MethodHandles.Lookup lookup = MethodHandles.lookup();
        try {
            applyToConsumerGP = lookup.findStatic(GenericProc.class, "applyToConsumerGP", applyMethodType);
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }
}
