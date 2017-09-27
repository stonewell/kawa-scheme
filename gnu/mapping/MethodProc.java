// Copyright (c) 1999, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.bytecode.Type;
import gnu.bytecode.ArrayType;
import gnu.expr.PrimProcedure;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

/** Similar to a CLOS method.
 * Can check if arguments "match" before committing to calling method. */

public abstract class MethodProc extends ProcedureN
{
    public MethodProc() { // FIXME - remove
        super(true, applyToConsumerDefaultMP);
    }
    public MethodProc(boolean resultGoesToConsumer, MethodHandle applyMethod) {
        super(resultGoesToConsumer, applyMethod);
    }
  /** The parameter types.
   * Usually either an Type[] or a String encoding. */
  protected Object argTypes;

    /** Test if method is applicable to an invocation with given arguments.
     * @param argTypes array of known "single" arguments.
     * @param restType If null, the arguments are fully  specified by argTypes.
     *   If non-null, there may be an unknown number of extra arguments
     *   of the given restType.  This is used for splices, where we usually
     *   don't know at compile-time how many argument values we have.
     * @return -1 if no; 1 if yes; 0 if need to check at run-time.
     */
    public int isApplicable(Type[] argTypes, Type restType) {
        int argCount = argTypes.length;
        int num = numArgs();
        int min = Procedure.minArgs(num);
        int max = Procedure.maxArgs(num);
        if ((argCount < min && restType == null)
            || (num >= 0 && argCount > max))
            return -1;
        int result = 1;
        for (int i = 0;  ; i++ )  {
            if (i >= argCount && (restType == null || i >= min))
                break;
            Type argType = i < argCount ? argTypes[i] : restType;
            int code =  getParameterType(i).isCompatibleWithValue(argType);
            if (code < 0) {
                return -1;
            }
            else if (code == 0)
                result = 0;
        }
        return result;
    }

  /** Return number of parameters, including optional and rest arguments. */
  public int numParameters()
  {
    int num = numArgs();
    int max = num >> 12;
    if (max >= 0)
      return max;
    // This isn't really right, but it works for PrimProcedure.  FIXME.
    int min = num & 0xFFF;
    return min + 1;
  }

  static final Type[] unknownArgTypes = { Type.pointer_type };

  /** Figure out or decode the parameter types, setting argTypes. */
  protected void resolveParameterTypes()
  {
    argTypes = unknownArgTypes;
  }

  public Type getParameterType(int index)
  {
    if (! (argTypes instanceof Type[]))
      resolveParameterTypes();

    Type[] atypes = (Type[]) argTypes;
    if (index < atypes.length
        && (index < atypes.length - 1 || maxArgs() >= 0))
      return atypes[index];
    if (maxArgs() < 0)
      {
        Type rtype = atypes[atypes.length-1];
        if (rtype instanceof ArrayType)
          return ((ArrayType) rtype).getComponentType();
      }
    return Type.objectType;
  }

  /* Special code for matchState field to require exception on mismatch. */
  public static final int THROW_ON_EXCEPTION = 0;

  /** Return code from match:  Unspecified failure. */
  public static final int NO_MATCH = -1;

  /** Return code from match:  Too few actual arguments.
   * The lower half is the minimum number of arguments (if not 0xffff). */
  public static final int NO_MATCH_TOO_FEW_ARGS = 0xfff10000;

  /** Return code from match:  Too many actual arguments.
   * The lower half is the maximum number of arguments (if not 0xffff). */
  public static final int NO_MATCH_TOO_MANY_ARGS = 0xfff20000;

  /** Return code from match:  Ambigious which method to select. */
  public static final int NO_MATCH_AMBIGUOUS = 0xfff30000;

  /** Return code from match: Invalid argument type.
   * In that case the lower half is the 1-origin index of the first
   * argument that does not match. */
  public static final int NO_MATCH_BAD_TYPE = 0xfff40000;

  /** Return code from match: Unused keyword argument.
   * I.e. a keyword in the call doesn't match a keyword parameter.
   * In that case the lower half is the 1-origin index of the first
   * keyword argument that does not match. */
  public static final int NO_MATCH_UNUSED_KEYWORD = 0xfff50000;

  public static final int NO_MATCH_GUARD_FALSE =  0xfff60000;

  /** Helper method to throw an exception if a <code>matchX</code>
   * method fails. */
  public static RuntimeException
  matchFailAsException(int code, Procedure proc, ArgList args)
  {
    int arg = (short) code;
    code &= 0xffff0000;
    if (code != NO_MATCH_BAD_TYPE)
      return new WrongArguments(proc, args.numArguments());
    return new WrongType(proc, arg, arg > 0 ? args.getArgAsObject(arg-1) : null);
  }

  /** Return the more specific of the arguments.
   * @return null if neither is more specific. */
  public static MethodProc mostSpecific(MethodProc proc1, MethodProc proc2)
  {
    // True if we've determined proc1 cannot be the more specific.  I.e. there
    // can be aguments lists that are applicable to proc1 and not proc2.
    boolean not1 = false;
    // True if we've determined proc2 cannot be the more specific.
    boolean not2 = false;
    int min1 = proc1.minArgs();
    int min2 = proc2.minArgs();
    int max1 = proc1.maxArgs();
    int max2 = proc2.maxArgs();
    if ((max1 >= 0 && max1 < min2)
	|| (max2 >= 0 && max2 < min1))
      return null;
    int num1 = proc1.numParameters();
    int num2 = proc2.numParameters();
    int limit = num1 < num2 ? num1 : num2;
    if (max1 != max2)
      {
        if (max1 < 0)
          not1 = true;
        if (max2 < 0)
          not2 = true;
      }
    if (min1 < min2)
      not1 = true;
    else if (min1 > min2)
      not2 = true;
    for (int i = 0; i < limit; i++)
      {
        Type t1 = proc1.getParameterType(i);
        Type t2 = proc2.getParameterType(i);
        int c1 = t1.isCompatibleWithValue(t2);
        int c2 = t2.isCompatibleWithValue(t1);
        if (c2 >= 0 && c2 > c1)
          {
            not2 = true;
            if (not1)
              return null;
          }
        if (c1 >= 0 && c1 > c2)
          {
            not1 = true;
            if (not2)
              return null;
          }
      }
    return not2 ? proc1 : not1 ? proc2 : null;
  }

    /** An approximation of "override-equivalent" as defined in the JLS. */
    public static boolean overrideEquivalent(MethodProc proc1, MethodProc proc2) {
        int num1 = proc1.numParameters();
        int num2 = proc2.numParameters();
        if (num1 != num2)
            return false;
        for (int i = 1;  i < num1;  i++) {
            Type t1 = proc1.getParameterType(i);
            Type t2 = proc2.getParameterType(i);
            if (t1.compare(t2) != 0)
                return false;
        }
        return true;
    }

    public static Object applyToConsumerDefaultMP(Procedure proc, CallContext ctx) throws Throwable {
        throw new Error("applyToConsumerDefaultMP for "+proc+"::"+proc.getClass().getName());
        /*
        Object[] args = ctx.getArgs();
        System.err.println("applyToConsumerDefaultMP for "+proc+"::"+proc.getClass().getName());
        int code = proc.matchN(args, ctx);
        if (code != 0) {
             if (ctx.matchState == CallContext.MATCH_THROW_ON_EXCEPTION)
                 throw MethodProc.matchFailAsException(code, proc, args);
             else
                 return ctx;
        }
        //proc.apply(ctx);
        Object ignored = proc.applyToConsumerMethod.invokeExact(proc, ctx);
        return null;
        */
    }
    public static final MethodHandle applyToConsumerDefaultMP
        = lookupApplyHandle(MethodProc.class, "applyToConsumerDefaultMP");
}
