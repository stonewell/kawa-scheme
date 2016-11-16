package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.lists.FString;
import gnu.lists.FVector;
import java.lang.reflect.Array;
import gnu.kawa.lispexpr.ClassNamespace; // FIXME
import gnu.kawa.lispexpr.LangObjType;
import java.lang.reflect.Array;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.MethodHandle;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

public class Invoke extends Procedure
{
  /** The kind on invoke operation.
   *  'N' - make (new).
   *  'P' - invoke-special.
   *  'S' - invoke-static (static or non-static):
   *        The first operand is a Class or Type, the second is the name,
   *        and if the method is non-static the 3rd is the receiver.
   *  's' - Like 'S' but only allow static methods. [not used]
   *  'V' - non-static invoke, only allow non-static methods. [not used]
   *  '*' - non-static invoke, can match static methods also.
   *        This is Java's 'Primary.MethodName(args)' - if the selected method
   *        is static, we only use Primary's type for method select,
   *        but ignore its value.
   */
  char kind;

  Language language;

    static final MethodHandle applyToObject =
        lookupApplyHandle(Invoke.class, "applyToObject");

  public static final Invoke invoke = new Invoke("invoke", '*');
  public static final Invoke invokeStatic = new Invoke("invoke-static", 'S');
  public static final Invoke invokeSpecial = new Invoke("invoke-special", 'P');
  public static final Invoke make = new Invoke("make", 'N');

  public Invoke(String name, char kind)
  {
    this(name, kind, Language.getDefaultLanguage());
  }

  public Invoke(String name, char kind, Language language)
  {
    super(false, Invoke.applyToObject, name);
    this.kind = kind;
    this.language = language;
    setProperty(Procedure.validateXApplyKey,
                "gnu.kawa.reflect.CompileInvoke:validateApplyInvoke");
  }

  public static Object invoke$V(Object[] args) throws Throwable
  {
    return invoke.applyN(args);
  }

  public static Object invokeStatic$V(Object[] args) throws Throwable
  {
    return invokeStatic.applyN(args);
  }

  public static Object make$V(Object[] args) throws Throwable
  {
    return make.applyN(args);
  }

  private static ObjectType typeFrom (Object arg, Invoke thisProc)
  {
    if (arg instanceof Class)
      arg = Type.make((Class) arg);
    if (arg instanceof ObjectType)
      return (ObjectType) arg;
    if (arg instanceof String || arg instanceof FString)
      return ClassType.make(arg.toString());
    if (arg instanceof Symbol)
      return ClassType.make(((Symbol) arg).getName());
    if (arg instanceof ClassNamespace)
      return ((ClassNamespace) arg).getClassType();
    throw new WrongType(thisProc, 0, arg, "class-specifier");
  }

    /* FIXME
  public void apply (CallContext ctx) throws Throwable
  {
    Object[] args = ctx.getArgs();
    if (kind=='S' || kind=='V' || kind=='s' || kind=='*')
      {
        // The following is an optimization, so that output from the
        // method is sent directly to ctx.consumer, rather than reified.
        int nargs = args.length;
        Procedure.checkArgCount(this, nargs);
        Object arg0 = args[0];
        ObjectType dtype = (ObjectType)
          ((kind == 'S' || kind == 's') ? typeFrom(arg0, this)
           : Type.make(arg0.getClass()));
        Procedure proc = lookupMethods(dtype, args[1]);
        Object[] margs = new Object[nargs-(kind == 'S' ? 2 : 1)];
        int i = 0;
        if (kind == 'V' || kind == '*')
          margs[i++] = args[0];
        System.arraycopy(args, 2, margs, i, nargs - 2);
        proc.checkN(margs, ctx);
      }
    else
      ctx.writeValue(this.applyN(args));
  }
    */

    public static Object applyToObject(Procedure proc, CallContext ctx)
    throws Throwable {
        Invoke invoke = (Invoke) proc;
        char kind = invoke.kind;
        Language language = invoke.language;

    if (kind == 'P')
      throw new RuntimeException(invoke.getName() 
                                 + ": invoke-special not allowed at run time");
    
    int nargs = ctx.numArguments();
    Procedure.checkArgCount(invoke, nargs);
    Object arg0 = ctx.getArgAsObject(0);
    ObjectType dtype = (kind != 'V' && kind != '*' ? typeFrom(arg0, invoke)
                       : (ObjectType) Type.make(arg0.getClass()));
    Object mname;
    if (kind == 'N')
      {
	mname = null;
        if (dtype instanceof TypeValue)
          {
            Procedure constructor = ((TypeValue) dtype).getConstructor();
            if (constructor != null)
              {
                return constructor.applyL(ArgListVector.drop(ctx, 1));
              }
          }
	if (dtype instanceof PairClassType)
	  {
	    PairClassType ptype = (PairClassType) dtype;
	    dtype = ptype.instanceType;
	  }
        if (dtype instanceof ArrayType
            || dtype == LangObjType.constVectorType)
          {
            int len = ctx.numArguments()-1;
            String name;
            int length;
            int i;
            boolean lengthSpecified;
            /* FIXME
            if (len >= 2 && args[1] instanceof Keyword
                && ("length".equals(name = ((Keyword) args[1]).getName())
                    || "size".equals(name)))
              {
                length = ((Number) args[2]).intValue();
                i = 3;
                lengthSpecified = true;
              }
            else
            */
              {
                length = len;
                i = 1;
                lengthSpecified = false;
              }
            Type elementType = (dtype == LangObjType.constVectorType
                                ? Type.objectType
                                : ((ArrayType) dtype).getComponentType());
            Object arr = Array.newInstance(elementType.getReflectClass(),
                                           length);
            int index = 0;
            for (; i <= len;  i++)
              {
                Object arg = ctx.getArgAsObject(i);
                String kname;
                if (lengthSpecified && (kname = ctx.getKeyword(i)) != null)
                  {
                    try
                      {
                        index = Integer.parseInt(kname);
                      }
                    catch (Exception ex)
                      {
                        throw new RuntimeException("non-integer keyword '"+kname+"' in array constructor");
                      }
                  }
                Array.set(arr, index, elementType.coerceFromObject(arg));
                index++;
              }
            if (dtype == LangObjType.constVectorType)
                return FVector.makeConstant((Object[]) arr);
            return arr;
          }
      }
    else
      {
        mname = ctx.getArgAsObject(1);
      }
    MethodProc mproc = invoke.lookupMethods((ObjectType) dtype, mname);
    if (kind != 'N')
      {
        ArgListVector margs = ArgListVector.drop(ctx, 2);
        if (kind == 'V' || kind == '*')
            margs = ArgListVector.prepend(margs, ctx.getArgAsObject(0));
        return mproc.applyL(margs);
      }
    else
      {
        Object result;
        ArgListVector args = ArgListVector.drop(ctx, 0);
        int keywordStart = ctx.firstKeyword();
        if (ctx.numKeywords() == 0)
          {
            ctx.rewind(CallContext.MATCH_CHECK);
            Object r = mproc.getApplyToObjectMethod().invokeExact((Procedure) mproc, ctx);
            if (r != ctx)
                return r;
            /*
            int err = proc.matchN(args, ctx);
            if (err == 0)
              return ctx.runUntilValue();
            */

            MethodProc vproc = ClassMethods.apply(dtype, "valueOf",
                                                  '\0', language);
            if (vproc != null)
              {
                ctx.setupApply(vproc);
                ctx.addAll((ArgList) ArgListVector.drop(args, 1));
                ctx.rewind(CallContext.MATCH_CHECK);
                r = ctx.runUntilValue();
                if (r != ctx)
                  return r;
              }
            result = mproc.apply1(args.getArgAsObject(0));
          }
        else
          {
            Object[] cargs = new Object[keywordStart];
            for (int i = 0;  i < keywordStart; i++)
                cargs[i] = args.getArgAsObject(i);
            result = mproc.applyN(cargs);
          }

        int numKeys = args.numKeywords();
        // Look for (keyword,value)-pairs.
        for (int i = 0;  i < numKeys;  i++)
          {
            Object arg = args.getArgAsObject(keywordStart+i);
            Keyword key = Keyword.make(args.getKeyword(keywordStart+i));
            SlotSet.apply(false, result, key.getName(), arg);
          }
        int i = numKeys == 0 ? 1 : keywordStart+numKeys;
        if (i != nargs)
          {
            MethodProc aproc = ClassMethods.apply(dtype, "add",
                                              '\0', language);
            int err = MethodProc.NO_MATCH;
            if (aproc == null)
                throw MethodProc.matchFailAsException(err, proc, args);
            while (i < nargs)
                aproc.apply2(result, args.getArgAsObject(i++));
          }

        return result;
      }
  }

  public int numArgs()
  {
    return (-1 << 12) | (kind == 'N' ? 1 : 2);
  }

  protected MethodProc lookupMethods(ObjectType dtype, Object name)
  {
    String mname;
    if (kind == 'N')
      mname = "<init>";
    else
      {
        if (name instanceof CharSequence)
          mname = name.toString();
	else if (name instanceof Symbol)
	  mname = ((Symbol) name).getName();
        else
          throw new WrongType(this, 1, name, "string-or-symbol");
        mname = Compilation.mangleName(mname);
      }
    MethodProc proc = ClassMethods.apply(dtype, mname,
                                         kind == 'P' ? 'P'
                                         : kind == '*' || kind == 'V' ? 'V'
                                         : '\0',
                                         language);
    if (proc == null)
      throw new RuntimeException(getName() + ": no method named `"
                                 + mname + "' in class " + dtype.getName());
    return proc;
  }


  /** Return an ApplyExp that will call a method with given arguments.
   * @param type the class containing the method we want to call.
   * @param name the name of the method we want to call
   * @param args the arguments to the call
   * @return an ApplyExp representing the call
   */
  public static synchronized
  ApplyExp makeInvokeStatic(ClassType type, String name, Expression... args)
  {
    PrimProcedure method = getStaticMethod(type, name, args);
    if (method == null)
      throw new RuntimeException("missing or ambiguous method `" + name
                                 + "' in " + type.getName());
    return new ApplyExp(method, args);
  }

  /** @deprecated */
  public static synchronized PrimProcedure
  getStaticMethod(ClassType type, String name, Expression[] args)
  {
    return CompileInvoke.getStaticMethod(type, name, args);
  }
}
