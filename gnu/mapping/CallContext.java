// Copyright (C) 2002, 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.math.*;
import gnu.lists.*;
import gnu.expr.Keyword; // FIXME - bad cross-package dependency
import gnu.expr.Special;
import gnu.kawa.util.HeapSort;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

/** A procedure activation stack (when compiled with explicit stacks). */

public class CallContext // implements Runnable
    // extends ValueStack ??? FIXME
    extends ArgListImpl implements ArgList, ArgListBuilder
{
    static ThreadLocal currentContext = new ThreadLocal();

  public static void setInstance(CallContext ctx)
  {
    Thread thread = Thread.currentThread();
    currentContext.set(ctx);
  }

  /** Get but don't create a CallContext for the current thread. */
  public static CallContext getOnlyInstance()
  {
    return (CallContext) currentContext.get();
  }

  /** Get or create a CallContext for the current thread. */
  public static CallContext getInstance()
  {
    CallContext ctx = getOnlyInstance();
    if (ctx == null)
      {
	ctx = new CallContext();
	setInstance(ctx);
      }
    return ctx;
  }

    public/*private*/ MethodHandle applyMethod; // used for runUntilDone trampoline
    // FIXME is this redundant, given the Procedure parameter to the apply
    // methods?  We probably don't need both.
    public /*private*/ Procedure proc; // mostly used for error reporting

    public final void setNextProcedure(Procedure proc, MethodHandle apply) {
        this.proc = proc;
        this.applyMethod = apply;
    }

    public final void setNextProcedure(Procedure proc) {
        this.proc = proc;
        this.applyMethod = proc == null ? null : proc.applyToConsumerMethod;
    }

  /* CPS:
  CallFrame frame;
  */

  /** Default place for function results.
   * In the future, function arguments will also use vstack. */
  private ValueStack vstack = new ValueStack();
  /** Function results are written to this Consumer.
   * This may point to vstack - or some other Consumer. */
  public Consumer consumer = vstack;

  /** Index of next argument.
   * This is used by methods like getNextArg, used by callees. */
  public int next;

  public Object getArgAsObject(int i)
  {
      /*
    if (i < 8)
      {
        switch ((this.where >> (4 * i)) & 15)
          {
          case ARG_IN_VALUE1:  return value1;
          case ARG_IN_VALUE2:  return value2;
          case ARG_IN_VALUE3:  return value3;
          case ARG_IN_VALUE4:  return value4;
          case ARG_IN_IVALUE1:  return IntNum.make(ivalue1);
          case ARG_IN_IVALUE2:  return IntNum.make(ivalue2);
          }
      }
      */
    return values[i];
  }

    /** Note state of matching - have we seen an error?
     * Either one of the NO_MATCH values defined in MethodProc, or
     * one of MATCH_THROW_ON_EXCEPTION, MATCH_CHECK, MATCH_CHECK_ONLY.
     */
    int matchState;
    /** Request to throw an exception on a match failure.
     * This is instead of setting matchState to an error value. */
    public static final int MATCH_THROW_ON_EXCEPTION = 0;
    /** Request to on failure return error code.
     * Error is indicated by check routine returning this CallContext.
     * Otherwise, execute actual function and return result (or null). */
    public static final int MATCH_CHECK = 1;
    /** Request to on failure return error code; on failure return 0.
     * Regardless, don't invoke body method. */
    public static final int MATCH_CHECK_ONLY = 2;

    public void rewind(int mode) {
        matchState = mode;
        rewind();
    }

    public void rewind() {
        next = 0;
        nextKeyword = 0;
    }

    public void reset() {
        super.clear();
        rewind(MATCH_THROW_ON_EXCEPTION);
    }

    public void shiftArgs(Procedure proc, int toDrop) {
        super.shiftArgs(toDrop);
        rewind();
        setNextProcedure(proc);
    }

    public int getMode() { return matchState; }
   
    //boolean throwOnMatchError;
    //boolean matchOnly;
    // Maybe:
    // int argState;
    // argState >= 0: number of args until first key arg (i.e. count-next)
    // argsStatc < 0: error code - subsumes matchState

    public void matchError(int code) {
        //new Error("matchError mp:"+proc+" code:"+Integer.toHexString(code)+" st:"+Integer.toHexString(matchState)).printStackTrace();
        if (throwOnException()) {
            int arg = (short) code;
            code &= 0xffff0000;
            if (code == MethodProc.NO_MATCH_TOO_FEW_ARGS
                || code == MethodProc.NO_MATCH_TOO_MANY_ARGS) {
                WrongArguments wr = new WrongArguments(proc, getArgCount()-numKeywords);
                //System.err.println("before WrongArgs code:"+Integer.toHexString(code)+" proc:"+proc+(proc==null?"":(" min:"+proc.minArgs()+" max:"+proc.maxArgs()))+" nargs:"+getArgCount()+" wr.m:"+wr.getMessage()+" wr:"+wr+" next:"+next+" count:"+count+" firstK:"+firstKeyword+" numK:"+numKeywords+" nextK:"+nextKeyword);
                throw wr;
            }
            if (code == MethodProc.NO_MATCH_UNUSED_KEYWORD) {
                StringBuilder mbuf = new StringBuilder();
                String pname = proc == null ? null : proc.getName();
                if (pname == null)
                    mbuf.append("call has ");
                else {
                    mbuf.append("call to ");
                    mbuf.append(pname);
                    mbuf.append(" has ");
                }
                if (keywords == null || numKeywords <= arg) {
                    
                    mbuf.append("unexpected keyword");
                }
                else {
                    if (sortedKeywords != null && nextKeyword > 0
                        && nextKeyword < sortedKeywords.length
                        && arg == sortedKeywords[nextKeyword]
                        && keywords[arg] == keywords[sortedKeywords[nextKeyword-1]])
                        //if (arg+1 < numKeywords && keywords[arg] == keywords[arg+1])
                        mbuf.append("duplicated keyword '");
                    else
                        mbuf.append("unexpected keyword '");
                    mbuf.append(keywords[arg]);
                    mbuf.append('\'');
                }
                throw new IllegalArgumentException(mbuf.toString());
            }
            if (code == MethodProc.NO_MATCH_GUARD_FALSE) {
                throw new IllegalArgumentException("guard evaluated to false");
            }
            throw new WrongType(proc, arg, arg >= 0 ? getArgAsObject(arg) : null);
	}
        if (matchState == MATCH_CHECK || matchState == MATCH_CHECK_ONLY)
            matchState = code;
    }

    public boolean throwOnException() {
        return matchState == MATCH_THROW_ON_EXCEPTION;
    }

    public boolean haveArg() {
        // If using argState: return argState > 0;
        return next < count && (next != firstKeyword || numKeywords == 0); // && matchState == 0;
    }

    /*
    public int checkOptionalDone() { // Or: doneWithArgs FIXME unused?
        if (next != count)
            matchError(MethodProc.NO_MATCH_TOO_MANY_ARGS|next);
        else if (matchState == MATCH_THROW_ON_EXCEPTION
                 || matchState == MATCH_CHECK)
            return 0;
        return matchState;
    }
    */

    public void checkKeywordsDone() {
        if (numKeywords != 0 && next < firstKeyword)
            matchError(MethodProc.NO_MATCH_TOO_MANY_ARGS|next);
        if (nextKeyword < numKeywords) {
            short keywordIndex = sortedKeywords == null ? 0
                : sortedKeywords[nextKeyword];
            matchError(MethodProc.NO_MATCH_UNUSED_KEYWORD|keywordIndex);
            nextKeyword = numKeywords;
        }
        if (numKeywords > 0)
            next = firstKeyword + numKeywords;
    }

    public int checkDone() { // Or: doneWithArgs
        if (next == firstKeyword && numKeywords > 0)
            matchError(MethodProc.NO_MATCH_UNUSED_KEYWORD|firstKeyword);
        else if (next != count)
            matchError(MethodProc.NO_MATCH_TOO_MANY_ARGS|next);
        else if (matchState == MATCH_THROW_ON_EXCEPTION
                 || matchState == MATCH_CHECK)
            return 0;
        return matchState;
    }

  public int getArgCount () { return count; }

    /** Get the next incoming argument.
     */
    public Object getNextArg() {
        if (next >= count || (next == firstKeyword && numKeywords != 0)) {
            matchError(MethodProc.NO_MATCH_TOO_FEW_ARGS|next);
            return null;
        } else
            return getArgAsObject(next++);
    }


    /** Get the next incoming argument.
     * Return defaultValue if there are no more arguments.
     */
    public Object getNextArg(Object defaultValue) {
        if (! haveArg())
            return defaultValue;
        return getArgAsObject(next++);
    }

    public final Object[] getRestPlainArray() {
        int numKeys = numKeywords();
        int firstKey = firstKeyword();
        if (next >= firstKey+numKeys || numKeys == 0)
            return getRestArgsArray();
        if (next <= firstKey) {
            Object[] args = new Object[firstKey-next];
            int i = 0;
            while (next < firstKey)
                args[i++] = getArgAsObject(next++);
            return args;
        }
        return new Object[0];
    }

    public final Object[] getRestArgsArray() {
        Object[] arr = getRestArgsArray(next);
        next = count;
        return arr;
    }
  /** Get remaining arguments as an array. */
  public final Object[] getRestArgsArray (int next)
  {
    int numKeys = numKeywords();
    int skipKeys = next - firstKeyword();
    if (skipKeys > 0 && skipKeys <= numKeys)
        numKeys -= skipKeys;
    int sz = count - next;
    Object[] args = new Object[count - next + numKeys];
    int i = 0;
    while (next < count)
      {
        String key = getKeyword(next);
        if (key != null)
            args[i++] = Keyword.make(key);
	args[i++] = getArgAsObject(next++);
      }
    return args;
  }

    public final LList peekRestArgsList() {
        return getRestArgsList(next);
    }

    public final ArgListVector getRestArgsVector() {
        ArgListVector args = getRestArgsVector(next);
        next = count;
        nextKeyword = numKeywords;
        return args;
    }

    public final LList getRestArgsList() {
        LList lst = getRestArgsList(next);
        next = count;
        nextKeyword = numKeywords;
        return lst;
    }

    /** Get remaining arguments as a list.
     * Used for Scheme and Lisp rest args. */
    public final LList getRestArgsList(int next) {
        if (numKeywords == 0)
            return getRestPlainList(next);
        else
            return ArgListPair.valueOf(getRestArgsVector(next));
    }

    public final LList getRestPlainList(int next) {
        if (false) // FIXME
            matchError(MethodProc.NO_MATCH_UNUSED_KEYWORD|firstKeyword);
    LList nil = LList.Empty;
    LList list = nil;
    Pair last = null;
    while (next < count)
      {
	Pair pair = new Pair(getArgAsObject(next++), nil);
	if (last == null)
	  list = pair;
	else
	  last.setCdr(pair);
	last = pair;
      }
    return list;
  }

    /** Get remaining arguments are an ArgListVector.
     * @param next The number of arguments that should be skipped.
     * Assume either no keywords have been processed, or they all have.
     */
    public ArgListVector getRestArgsVector(int next) {
        int size = count - next;
        if (next >= firstKeyword + numKeywords) {
            Object[] args = new Object[size];
            System.arraycopy(values, next, args, 0, size);
            return new ArgListVector(args, 0, 0);
        }
        size += numKeywords;
        Object[] args = new Object[size];
        int j = 0;
        for (int i = next; i < count; i++) {
            String key = getKeyword(i);
            if (key != null)
                args[j++] = Keyword.make(key);
            args[j++] = getArgAsObject(i);
        }
        return new ArgListVector(args, firstKeyword-next, numKeywords);
    }

  /** Note that we are done with the input arguments.
   * Throw WrongArguments if there are unprocessed arguments.
   */
  public void lastArg()
  {
    if (next < count)
      throw new WrongArguments(null, count);
    //values = null;
  }

  public Object[] getArgs()
  {
	Object[] args = new Object[count];
        System.arraycopy(values, 0, args, 0, count);
        return args;
      /*
    if (where == 0)
      return values;
    else
      {
	int n = count;
	next = 0;
	for (int i = 0;  i < n;  i++)
	  args[i] = getNextArg();
	return args;
      }
      */
  }

    /*
    void resetArgCount(int size) {
        if (values.length < size) {
            int nsize = size > 32 ? size : 2 * size;
            values = new Object[nsize];
        } else {
            for (int i = size; i < count; i++)
            values[i] = null;
        }
    }
    */

    /* * A sorted list of keyword arguments.
     * (It might be a win to use a String instead of a short[],
     * because a String is cheaper to initialize.
     * {@code values[sortedKeywords[i]+firstKeyword]} is the {@code i}'th
     * keyword argument in lexicographic order.
     */ 
    public static short[] getSortedKeywords(String[] keywords, int nkeys) {
        short[] sorted = new short[nkeys];
        for (short i = (short) nkeys; --i >= 0; )
            sorted[i] = i;
        keywordSorter.heapSort(sorted, nkeys, keywords);
        return sorted;
    }

    static class KeywordSorter extends HeapSort<short[], String[]> {
        protected void swap(short[] a, int i, int j) {
            short t = a[i];
            a[i] = a[j];
            a[j] = t;
        }
        protected int compare(short[] a, int i, int j, String[] keywords) {
            int cmp = keywords[a[i]].compareTo(keywords[a[j]]);
            return cmp != 0 ? cmp : i > j ? 1 : i < j ? -1 : 0;
        }
    }
    static final KeywordSorter keywordSorter = new KeywordSorter();

    // Index in the sortedKeywords array.  Initialized to 0.
    int nextKeyword;

    /** Return index of matching keyword argument.
     * Must be called with keywords in increasing lexicographic order.
     * @return {@code dfault} if no matching keyword argument,
     *   or the corresponding keyword value.
     */

    public int nextKeyword(String keyword) {
        return nextKeywordIndex(keyword, false);
    }
    public int nextKeywordAllowOthers(String keyword) {
        return nextKeywordIndex(keyword, true);
    }
    public int nextKeywordIndex(String keyword, boolean allowOthers) {
        int klen = numKeywords;
        if (klen == 0)
            return -1;
        if (sortedKeywords == null)
            sortedKeywords = getSortedKeywords(keywords, klen);
        int i = nextKeyword;
        for (;; i++) {
            if (i >= klen) {
                nextKeyword = i;
                return -1;
            }
            short keywordIndex = sortedKeywords[i];
            String argkey = keywords[keywordIndex];
            // Assumes keyword strings are interned.
            if (argkey==keyword) {
                nextKeyword = i+1;
                return firstKeyword+keywordIndex;
            }
            if (allowOthers && argkey.compareTo(keyword) < 0)
                continue;
            // argkey > keyword
            nextKeyword = i;
            return -1;
        }
    }
    public Object nextKeyword(String keyword, Object dfault) {
        int index = nextKeywordIndex(keyword, false);
        return index >= 0 ? values[index] : dfault;
    }

    public Object nextKeywordAllowOthers(String keyword, Object dfault) {
        int index = nextKeywordIndex(keyword, true);
        return index >= 0 ? values[index] : dfault;
    }

    /*
    public static int compareForLookup(Keyword keyword1, Keyword keyword2) {
        // It might be faster to first compare hashCodes, and only
        // then do a compareTo.
        // That assumes String#hashCode is optimized by the VM.
        return keyword1.getName().compareTo(keyword2.getName());
    }
    */

    public void setupApply(Procedure proc) {
        setNextProcedure(proc);
        super.setArgs();
        rewind(MATCH_THROW_ON_EXCEPTION);
    }

    public void setupApply(Procedure proc, Object arg0) {
        setNextProcedure(proc);
        super.setArgs(arg0);
        rewind(MATCH_THROW_ON_EXCEPTION);
    }

    public void setupApply(Procedure proc, Object arg0, Object arg1) {
        setNextProcedure(proc);
        super.setArgs(arg0, arg1);
        rewind(MATCH_THROW_ON_EXCEPTION);
    }

    public void setupApply(Procedure proc, Object arg0, Object arg1,
                            Object arg2) {
        setNextProcedure(proc);
        super.setArgs(arg0, arg1, arg2);
        rewind(MATCH_THROW_ON_EXCEPTION);
    }

    public void setupApply(Procedure proc, Object arg0, Object arg1,
                            Object arg2, Object arg3) {
        setNextProcedure(proc);
        super.setArgs(arg0, arg1, arg2, arg3);
        rewind(MATCH_THROW_ON_EXCEPTION);
    }

    public void setupApplyAll(Procedure proc, Object[] args) {
        setNextProcedure(proc);
        super.setArgsAll(args);
        rewind(MATCH_THROW_ON_EXCEPTION);
    }

    public void setupApplyAll(Procedure proc, Object[] args,
                              int fromIndex, int toIndex) {
        setNextProcedure(proc);
        super.setArgsAll(args, fromIndex, toIndex);
        rewind(MATCH_THROW_ON_EXCEPTION);
    }

    public void addArg(Object arg) {
        super.add(arg);
    }

    public void addArg(Object arg0, Object arg1, Object arg2, Object arg3) {
        super.add(arg0, arg1, arg2, arg3);
    }

    /*
   public void setArgs() {
        super.setArgs();
        next = 0;
    }

    public void setArgs(Object arg0) {
        super.setArgs(arg0);
        next = 0;
    }

    public void setArgs(Object arg0, Object arg1) {
        super.setArgs(arg0, arg1);
        next = 0;
    }

    public void setArgs(Object arg0, Object arg1, Object arg2) {
        super.setArgs(arg0, arg1, arg2);
        next = 0;
    }

    public void setArgs(Object arg0, Object arg1, Object arg2, Object arg3) {
        super.setArgs(arg0, arg1, arg2, arg3);
        next = 0;
    }
    public void setArgsAll(Object[] args) {
        super.setArgsAll(args);
        next = 0;
    }
    */

   public void runUntilDone()  throws Throwable
    {
    for (;;)
      {
        MethodHandle handle = applyMethod;
	if (handle == null)
	      break;
	this.applyMethod = null;
        rewind(MATCH_THROW_ON_EXCEPTION);
        Object ignored = handle.invokeExact(proc, this);
      }
  }

  /** Setup routine before calling a method that takes a CallContext.
   * The compiler emits a call to this before a call to a method that takes
   * a CallContext, when it wants the function result as an Object.
   * It pushes the CallContest state so it can uses the vstack for a
   * temporary, After the method, getFromContext extract the method's result
   * from the vstack and restores the state.
   */
    public final int startFromContext() {
        //System.err.println("startFromContext proc:"+proc+" gapS:"+vstack.gapStart +" onSt:"+vstack.gapStartOnPush+" cons:"+consumer.getClass().getName()+" last:"+vstack.lastObject);
        if (vstack.gapStart == vstack.gapStartOnPush
            && consumer == vstack
            && vstack.lastObject==vstack) { // Simple efficient case.
            return -1;
        } else {
            vstack.push();
            vstack.consumerOnPush = consumer;
            vstack.oindexOnPush = vstack.oindex;
            vstack.gapStartOnPush = vstack.gapStart;
            consumer = vstack;
            return vstack.gapStart;
        }
    }

    /** Routine to extract result and restore state after startFromContext.
     */
    public final Object getFromContext(int saved) throws Throwable {
        runUntilDone();
        //System.err.println("getFromContext proc:"+proc+" gapS:"+vstack.gapStart +" onSt:"+vstack.gapStartOnPush+" last:"+vstack.lastObject);
        Object result = ((ValueStack) consumer).getValue();
        cleanupFromContext(saved);
        return result;
    }

    /** Cleanup-only part of getFromContext.
     * This can be in an exception handler as an alternative
     * to getFromContext, which is called in the non-exception case.
     * (Alternatively, the compiler could call cleanupFromContext
     * from a finally clause but that is less efficient, partly
     * because the JVM stack must be empty before a finally subroutine.)
     */
    public final void cleanupFromContext(int saved) {
        vstack.gapStart = vstack.gapStartOnPush;
        int oindexOnPush = vstack.oindexOnPush;
        for (int i = vstack.oindex;  --i >= oindexOnPush; )
            vstack.objects[i] = null;
        vstack.oindex = oindexOnPush;
        vstack.lastObject = vstack;
        if (saved >= 0) {
            consumer = vstack.consumerOnPush;
            vstack.pop(saved);
        }
    }

    /** Run until no more continuations, returning final result.
     * Assume that the current applyHandle is from proc. */
    public final Object runUntilValue() throws Throwable {
        // Functionally equivalent to the following, but more efficient.
        // int saved = startFromContext();
        // try {
        //     return getFromContext(saved);
        // } catch (Throwable ex) {
        //     cleanupFromContext(saved);
        // throw ex;
        // }
        if (proc != null && applyMethod == Procedure.applyToConsumerDefault) {
            applyMethod = null;
            return proc.applyToObjectMethod.invokeExact(proc, this);
        }
        Consumer consumerSave = consumer;
        ValueStack vst = vstack;
        consumer = vst;
        Object lastSave = vst.lastObject;
        vst.lastObject = vst;
        int dindexSave = vst.gapStart;
        int gapStartOnPushSave = vst.gapStartOnPush;
        vstack.gapStartOnPush = vst.gapStart;
        int oindexSave = vst.oindex;
        Object r;
        try {
            runUntilDone();
            return vst.getValue();
        } finally {
            consumer = consumerSave;
            vst.gapStart = dindexSave;
            vst.oindex = oindexSave;
            vst.gapStartOnPush = gapStartOnPushSave;
            vst.lastObject = lastSave;
            
        }
    }

  /** Run until no more continuations, sending result to a COnsumer. */
  public final void runUntilValue(Consumer out) throws Throwable
  {
    Consumer consumerSave = consumer;
    consumer = out;
    try
      {
	runUntilDone();
      }
    finally
      {
	consumer = consumerSave;
      }
  }

  /** Write values (of function result) to current consumer. */
  public void writeValue(Object value)
  {
    Values.writeValues(value, consumer);
  }

  /** Current stack of evaluation frames for interpreter. */
  public Object[][] evalFrames;

    int consumerOnPushArgState;
    public final void pushArgState() {
        vstack.pushArgState(this);
    }
    public final void popArgState() {
        vstack.popArgState(this);
    }

    /* #ifndef use:java.lang.invoke */
    // /** A fake "shim" for MethodHandle, as for Procedure application. */
    // public static abstract class MethodHandle {
    //     public abstract Object invokeExact(Procedure proc, CallContext ctx) throws Throwable;
    // }
    // /** A implementation of "fake MethodHandle" using reflection. */
    // public static class ReflectMethodHandle extends MethodHandle {
    //     java.lang.reflect.Method method;
    //     public ReflectMethodHandle(java.lang.reflect.Method method) {
    //         this.method = method;
    //     }
    //     public Object invokeExact(Procedure proc, CallContext ctx) throws Throwable {
    //         try {
    //             return method.invoke(null, proc, ctx);
    //         } catch (java.lang.reflect.InvocationTargetException ex) {
    //             throw ex.getCause();
    //         }
    //     }
    // }
    /* #endif */
}

/* CPS:
class CallFrame
{
  Procedure proc;
  CallFrame previous;
  int saveVstackLen;

  // Should probably be in sub-classes of ClassFrame:
  Object[] values;
}
*/
