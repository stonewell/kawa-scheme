// Copyright (C) 2002, 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.math.*;
import gnu.lists.*;
import gnu.expr.Keyword; // FIXME - bad cross-package dependency
import gnu.expr.Special;
import gnu.kawa.util.HeapSort;

/** A procedure activation stack (when compiled with explicit stacks). */

public class CallContext // implements Runnable
    // extends ValueStack ??? FIXME
    extends ArgListImpl implements ArgList, ArgListBuilder
{
    static ThreadLocal currentContext = new ThreadLocal();

  public static void setInstance(CallContext ctx)
  {
    Thread thread = Thread.currentThread();
    /* #ifdef JAVA2 */
    currentContext.set(ctx);
    /* #else */
    // if (thread instanceof Future)
    //   ((Future) thread).closure.context = ctx;
    // else
    //   threadMap.put(thread, ctx);
    /* #endif */
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

    // Perhaps replace with:
    // MethodHandle applyMethod;
    public Procedure proc;  // used for runUntilDone trampoline
    public Procedure mproc; // used for error reporting

  /** The program location in the current procedure.
   * This a selector that only has meaning to the proc's Procedure.*/
  public int pc;

  /* CPS:
  CallFrame frame;
  */

  /** Default place for function results.
   * In the future, function arguments will also use vstack. */
  private ValueStack vstack = new ValueStack();
  /** Function results are written to this Consumer.
   * This may point to vstack - or some other Consumer. */
  public Consumer consumer = vstack;

  /* * Number of actual arguments. * /
  public int count;
    * /
  
  /** Index of next argument.
   * This is used by methods like getNextArg, used by callees. */
  public int next;

    //public ArgList arguments = new ArgList();

  /** Encoding of where the arguments are.
   * Each argument uses 4 bits.
   * Arguments beyond 8 are implicitly ARG_IN_VALUES_ARRAY.
   * DEPRECATED
   */
  public int where;
  public final static int ARG_IN_VALUES_ARRAY = 0;
  public final static int ARG_IN_VALUE1 = 1;
  public final static int ARG_IN_VALUE2 = 2;
  public final static int ARG_IN_VALUE3 = 3;
  public final static int ARG_IN_VALUE4 = 4;
  public final static int ARG_IN_IVALUE1 = 5;
  public final static int ARG_IN_IVALUE2 = 6;

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
        next = 0;
    }

    public void shiftArgs(Procedure proc, int toDrop) {
        super.shiftArgs(toDrop);
        next = 0;
        this.proc = proc;
        this.mproc = proc;
    }

    public int getMode() { return matchState; }
   
    //boolean throwOnMatchError;
    //boolean matchOnly;
    // Maybe:
    // int argState;
    // argState >= 0: number of args until first key arg (i.e. count-next)
    // argsStatc < 0: error code - subsumes matchState

    public void matchError(int code) {
        //new Error("matchError mp:"+mproc+" code:"+Integer.toHexString(code)+" st:"+Integer.toHexString(matchState)).printStackTrace();
        if (matchState == MATCH_THROW_ON_EXCEPTION) {
            int arg = (short) code;
            code &= 0xffff0000;
            if (code == MethodProc.NO_MATCH_TOO_FEW_ARGS
                || code == MethodProc.NO_MATCH_TOO_MANY_ARGS) {
                System.err.println("before WrongArgs proc:"+proc+" mproc:"+mproc+" nargs:"+getArgCount());
                throw new WrongArguments(mproc, getArgCount());
            }
            if (code == MethodProc.NO_MATCH_UNUSED_KEYWORD)
                throw new IllegalArgumentException(keywords != null && keywords.length > arg ? "unexpected keyword '"+keywords[arg]+"'" : "unexpected keyword");
            throw new WrongType(mproc, arg, arg > 0 ? getArgAsObject(arg-1) : null);
	}
        if (matchState == MATCH_CHECK || matchState == MATCH_CHECK_ONLY)
            matchState = code;
    }

    public boolean haveArg() {
        // If using argState: return argState > 0;
        return next < count; // && matchState == 0;
    }

    public int checkOptionalDone() { // Or: doneWithArgs
        if (next != count)
            matchError(MethodProc.NO_MATCH_TOO_MANY_ARGS|next);
        else if (matchState == MATCH_THROW_ON_EXCEPTION
                 || matchState == MATCH_CHECK)
            return 0;
        return matchState;
    }

    public void checkKeywordsDone() {
        if (next != firstKeyword)
            matchError(MethodProc.NO_MATCH_TOO_MANY_ARGS|next);
        if (nextKeyword < numKeywords) {
            short keywordIndex = sortedKeywords[nextKeyword];
            System.err.println("bad checkKeywordsDone next:"+next+" nextKey:"+nextKeyword+" numK:"+numKeywords);
            matchError(MethodProc.NO_MATCH_UNUSED_KEYWORD|keywordIndex);
            nextKeyword = numKeywords;
        }
        next = firstKeyword + numKeywords;
    }

    public int checkDone() { // Or: doneWithArgs
        int r= xcheckDone();
        //System.err.println("checkDone->"+r);
        return r;
    }
    public int xcheckDone() { // Or: doneWithArgs
        if (numKeywords > 0)
            checkKeywordsDone();
        if (next != count) {
            matchError(MethodProc.NO_MATCH_TOO_MANY_ARGS|next);
        }
        else if (matchState == MATCH_THROW_ON_EXCEPTION
                 || matchState == MATCH_CHECK)
            return 0;
        return matchState;
    }

  public int getArgCount () { return count; }

  /** Get the next incoming argument.
   * Throw WrongArguments if there are no more arguments.
   * FIXME: This and following methods don't really fit until the
   * current match/apply-based API, at least as currently implemented.
   * We probably need to pass in (or make this a method of) the Procedure.
   */
  public Object getNextArg()
  {
          if (haveArg())
              return getArgAsObject(next++);
          else {
              matchError(MethodProc.NO_MATCH_TOO_FEW_ARGS|next);
              return null;
          }
   }

    public Object getLastArg()  {
        if (haveArg()) {
            Object r = getArgAsObject(next++);
            if (haveArg()) {
                matchError(MethodProc.NO_MATCH_TOO_FEW_ARGS|next);
                return null;
            }
            return r;
        }
        else {
            matchError(MethodProc.NO_MATCH_TOO_MANY_ARGS|next);
            return null;
        }
    }

  public int getNextIntArg()
  {
    if (next >= count)
      throw new WrongArguments(null, count);
    Object arg = getArgAsObject(next++);
    return ((Number) arg).intValue();
  }

  /** Get the next incoming argument.
   * Return defaultValue if there are no more arguments.
   */
  public Object getNextArg(Object defaultValue)
  {
    if (next >= count)
      return defaultValue;
    return getArgAsObject(next++);
  }

  public int getNextIntArg(int defaultValue)
  {
    if (next >= count)
      return defaultValue;
    return ((Number) getArgAsObject(next++)).intValue();
  }

    public final Object[] getRestArgsArray() {
        Object[] arr = getRestArgsArray(next);
        next = count;
        return arr;
    }
  /** Get remaining arguments as an array. */
  public final Object[] getRestArgsArray (int next)
  {
    Object[] args = new Object[count - next];
    int i = 0;
    while (next < count)
      {
	args[i++] = getArgAsObject(next++);
      }
    return args;
  }

    public final LList getRestArgsList() {
        LList lst = getRestArgsList(next);
        next = count;
        return lst;
    }
  /** Get remaining arguments as a list.
   * Used for Scheme and Lisp rest args. */
  public final LList getRestArgsList (int next)
  {
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

  /** Note that we are done with the input arguments.
   * Throw WrongArguments if there are unprocessed arguments.
   */
  public void lastArg()
  {
    if (next < count)
      throw new WrongArguments(null, count);
    values = null;
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

    //int firstKeyword;
    //int numKeywords;

    /* * A sorted list of keyword arguments.
     * (It might be a win to use a String instead of a short[],
     * because a String is cheaper to initialize.
     * {@code values[keywordIndexes[i]+firstKeyword]} is the {@code i}'th
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
            return keywords[a[i]].compareTo(keywords[a[j]]);
        }
    }
    static final KeywordSorter keywordSorter = new KeywordSorter();

    //short[] keywordIndexes;
    // Index in keywordIndexes array.  Set to 0;
    // Should be combined with argState???
    int nextKeyword;
    /** Return index of matching keyword argument.
     * Must be called with keywords in increasing lexicographic order.
     * @return {@code dfault} if no matching keyword argument,
     *   or the corresponding keyword value.
     */
    public int nextKeyword(String keyword) {
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
            //String argkey = (String) values[firstKeyword+keywordIndex];
            String argkey = keywords[keywordIndex];
            // Assumes keyword strings are interned.
            if (argkey==keyword) {
                nextKeyword = i+1;
                return firstKeyword+keywordIndex;
            }
            int cmp = argkey.compareTo(keyword);
            if (cmp < 0) { // argkey < keyword
                // unused keyword argument
                matchError(MethodProc.NO_MATCH_UNUSED_KEYWORD|keywordIndex);
            }
            else { // argkey > keyword
                nextKeyword = i;
                return -1;
            }
        }
    }
    public Object nextKeyword(String keyword, Object dfault) {
        int index = nextKeyword(keyword);
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
        this.proc = proc;
        this.mproc = proc;
        super.setArgs();
        next = 0;
        matchState = MATCH_THROW_ON_EXCEPTION;
    }

    public void setupApply(Procedure proc, Object arg0) {
        this.proc = proc;
        this.mproc = proc;
        super.setArgs(arg0);
        next = 0;
        matchState = MATCH_THROW_ON_EXCEPTION;
    }

    public void setupApply(Procedure proc, Object arg0, Object arg1) {
        this.proc = proc;
        this.mproc = proc;
        super.setArgs(arg0, arg1);
        next = 0;
        matchState = MATCH_THROW_ON_EXCEPTION;
    }

    public void setupApply(Procedure proc, Object arg0, Object arg1,
                            Object arg2) {
        this.proc = proc;
        this.mproc = proc;
        super.setArgs(arg0, arg1, arg2);
        next = 0;
        matchState = MATCH_THROW_ON_EXCEPTION;
    }

    public void setupApply(Procedure proc, Object arg0, Object arg1,
                            Object arg2, Object arg3) {
        this.proc = proc;
        this.mproc = proc;
        super.setArgs(arg0, arg1, arg2, arg3);
        next = 0;
        matchState = MATCH_THROW_ON_EXCEPTION;
    }

    public void setupApplyAll(Procedure proc, Object[] args) {
        this.proc = proc;
        this.mproc = proc;
        super.setArgsAll(args);
        next = 0;
        matchState = MATCH_THROW_ON_EXCEPTION;
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
	Procedure proc = this.proc;
	if (proc == null)
	  {
	    /* CPS:
	    CallFrame fr = frame;
	    if (fr == null)
	      break;
	    proc = fr.proc;
	    frame = fr.previous;
	    if (proc == null)
	    */
	      break;
	  }
	this.proc = null;
        //System.err.println("in runUntilDone before apply of "+proc+" count:"+count+" next:"+next);
	proc.apply(this);
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
        //System.err.println("startFromContext mproc:"+mproc+" gapS:"+vstack.gapStart +" onSt:"+vstack.gapStartOnPush+" cons:"+consumer.getClass().getName()+" last:"+vstack.lastObject);
        if (vstack.gapStart == vstack.gapStartOnPush
            && consumer == vstack) { // Simple efficient case.
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
        //System.err.println("getFromContext mproc:"+mproc+" gapS:"+vstack.gapStart +" onSt:"+vstack.gapStartOnPush+" last:"+vstack.lastObject);
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

    /** Run until no more continuations, returning final result. */
    public final Object runUntilValue() throws Throwable {
        // Functionally equivalent to the following, but more efficient.
        // int saved = startFromContext();
        // try {
        //     return getFromContext(saved);
        // } catch (Throwable ex) {
        //     cleanupFromContext(saved);
        // throw ex;
        // }
        if (proc != null && proc.applyToConsumerMethod == Procedure.applyToConsumerDefault) {
            Procedure p = proc;
            proc = null;
            //Object rapp=java.lang.invoke.MethodHandles.lookup().revealDirect(p.applyToObjectMethod);
            //System.err.println("runUntilV "+p+"::"+p.getClass().getName()+" mproc:"+mproc+" app:"+rapp+" count:"+count+(count>0?("arg0:"+values[0]):""));
            Object r= p.applyToObjectMethod.invokeExact(p, this);
            //System.err.println("ret runUntilV "+p+" ->r:"+r);
            return r;
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
