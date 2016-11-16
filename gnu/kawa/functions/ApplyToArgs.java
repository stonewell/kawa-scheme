package gnu.kawa.functions;

import gnu.expr.Language;
import gnu.lists.Strings;
import gnu.mapping.*;
import gnu.kawa.reflect.Invoke;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */
import gnu.lists.*;
import java.util.List;
import gnu.text.Char;

/** Implement the standard Scheme function "apply".
 * This has been generalized so that the last (list argument)
 * can be any sequence, or any primitive array coercible to Object[]. */

public class ApplyToArgs extends ProcedureN
{
  public ApplyToArgs (String name, Language language)
  {
    super(name);
    applyToObjectMethod = applyToObjectA2A;
    applyToConsumerMethod = applyToConsumerA2A;
    this.language = language;
    setProperty(Procedure.validateXApplyKey,
                "gnu.kawa.functions.CompilationHelpers:validateApplyToArgs");
  }

  Language language;

    public static Object index(CharSequence str, Object index) {
        IntSequence indexes = Sequences.asIntSequenceOrNull(index);
        if (indexes != null) {
            return Strings.indirectIndexed(str, indexes);
        } else {
            int iindex = ((Number) index).intValue();
            return Char.valueOf(Strings.characterAt(str, iindex));
        }
    }

    public Object applyN (Object[] args) throws Throwable {
        Object proc = Promise.force(args[0]);
        if (proc instanceof Procedure) {
            Object[] rargs = new Object[args.length-1];
            System.arraycopy(args, 1, rargs, 0, rargs.length);
            return ((Procedure) proc).applyN(rargs);
        }
        if (proc instanceof gnu.bytecode.Type
            || proc instanceof Class) {
            return gnu.kawa.reflect.Invoke.make.applyN(args);
        }
        if (proc instanceof CharSequence) {
            if (args.length != 2)
                throw new WrongArguments(this, args.length); // FIXME
            return index((CharSequence) proc, Promise.force(args[1]));
        }
        if (proc instanceof gnu.lists.Array) {
            return ComposedArray.generalIndex((Array) proc, false,
                                              1, args.length-1, args);
        }
        if (proc instanceof List) {
            if (args.length != 2)
                throw new WrongArguments(this, args.length); // FIXME
            List lst = (List) proc;
            Object arg1 = Promise.force(args[1]);
            IntSequence indexes = Sequences.asIntSequenceOrNull(arg1);
            if (indexes != null) {
                return Sequences.indirectIndexed(lst, indexes);
            } else {
                int index = ((Number) arg1).intValue();
                return lst.get(index);

            }
        }
        /*
          What should happen if key has no associated value?
          Throw an exception?  Return null?
        if (proc instanceof java.util.Map) {
            if (args.length != 2)
                throw new WrongArguments(this, args.length); // FIXME
            Object key = Promise.force(args[1]);
            
        }
        */
        Class pclass = proc.getClass();
        if (pclass.isArray()) {
            if (args.length != 2)
                throw new WrongArguments(this, args.length); // FIXME
            return java.lang.reflect.Array.get(proc, ((Number) args[1]).intValue());
        }
        throw new WrongType(this, 0, proc, "procedure");
    }

    public static Object applyToConsumerA2A(Procedure proc, CallContext ctx) throws Throwable {
        Object arg0 = Promise.force(ctx.getNextArg());
        if (arg0 instanceof Procedure) {
            proc = (Procedure) arg0;
            ctx.shiftArgs(null, 1);
            return proc.getApplyToConsumerMethod().invokeExact(proc, ctx);
        }
        Object r = applyRest(arg0, ctx);
        if (r != ctx) {
            ctx.consumer.writeObject(r);
            r = null;
        }
        return r;
    }
    public static Object applyToObjectA2A(Procedure proc, CallContext ctx) throws Throwable {
        Object arg0 = Promise.force(ctx.getNextArg());
        if (arg0 instanceof Procedure) {
            proc = (Procedure) arg0;
            ctx.shiftArgs(null, 1);
            return proc.getApplyToObjectMethod().invokeExact(proc, ctx);
        }
        return applyRest(arg0, ctx);
    }
    private static Object applyRest(Object arg0, CallContext ctx) throws Throwable {
        if (arg0 instanceof gnu.bytecode.Type
            || arg0 instanceof Class) {
            Procedure proc = gnu.kawa.reflect.Invoke.make;
            ctx.rewind();
            ctx.setNextProcedure(proc, null);
            return proc.getApplyToObjectMethod().invokeExact(proc, ctx);
        }
        if (arg0 instanceof CharSequence) {
            Object arg1 = Promise.force(ctx.getNextArg());
            if (ctx.checkDone() != 0)
                return ctx;
            IntSequence indexes = Sequences.asIntSequenceOrNull(arg1);
            CharSequence str = (CharSequence) arg0;
            if (indexes != null)
                return Strings.indirectIndexed(str, indexes);
            if (! (arg1 instanceof Number))
                ctx.matchError(MethodProc.NO_MATCH_BAD_TYPE|2);
            int iindex = ((Number) arg1).intValue();
            return Char.valueOf(Strings.characterAt(str, iindex));
        }
        if (arg0 instanceof java.util.List) {
            Object arg1 = Promise.force(ctx.getNextArg());
            if (! (arg1 instanceof Number))
                ctx.matchError(MethodProc.NO_MATCH_BAD_TYPE|2);
            if (ctx.checkDone() != 0)
                return ctx;
            int index = ((Number) Promise.force(arg1)).intValue();
            return ((java.util.List) arg0).get(index);
        }
        if (arg0 != null) {
            Class pclass = arg0.getClass();
            if (pclass.isArray()) {
                Object arg1 = ctx.getNextArg();
                if (! (arg1 instanceof Number))
                    ctx.matchError(MethodProc.NO_MATCH_BAD_TYPE|2);
                if (ctx.checkDone() != 0)
                    return ctx;
                int index = ((Number) Promise.force(arg1)).intValue();
                return java.lang.reflect.Array.get(arg0, index);
            }
            if (arg0 instanceof gnu.lists.Array) {
                Procedure proc = ArrayRef.arrayRef;
                ctx.next = 0;    // OR: ctx.shiftArgs(proc, 0);
                return proc.getApplyToObjectMethod().invokeExact(proc, ctx);
            }
            /*
              What should happen if key has no associated value?
              Throw an exception?  Return null?
              if (arg0 instanceof java.util.Map) {
              if (args.length != 2)
              throw new WrongArguments(this, args.length); // FIXME
              Object key = Promise.force(args[1]);
            
              }
            */
        }
        ctx.matchError(MethodProc.NO_MATCH_BAD_TYPE|1);
        return ctx;
    }

    public static final MethodHandle applyToObjectA2A =
        Procedure.lookupApplyHandle(ApplyToArgs.class, "applyToObjectA2A");
    public static final MethodHandle applyToConsumerA2A =
        Procedure.lookupApplyHandle(ApplyToArgs.class, "applyToConsumerA2A");
}
