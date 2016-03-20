// Copyright (c) 1999, 2000-2005, 2006, 2010, 2014 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.util.*;
import java.io.*;
import kawa.Shell;
import java.util.zip.*;
import java.util.Stack;
import gnu.kawa.functions.Convert;
import gnu.kawa.io.*;
import gnu.kawa.lispexpr.LangObjType;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.kawa.reflect.LazyType;
import gnu.lists.Pair;
import gnu.text.Char;
import gnu.text.Lexer;
import gnu.text.Options;
import gnu.text.SourceLocator;
import gnu.text.SourceMessages;
import kawa.lang.Translator.FormStack;

/** State for a single expression or module.
 * For each top-level thing (expression or file) we compile or evaluate
 * we create a new Compilation.
 */

public class Compilation implements SourceLocator
{
  /** True if the form is too complex to evaluate,and we must compile it.
   * This is because it contains a construct we know how to compile, but not
   * evaluate, and it it outside a function (which we always compile).
   * This can be a let scope, or primitive procedure. */
  public boolean mustCompile = ModuleExp.alwaysCompile;

  /** Used by LambdaExp.getSelectorValue if need to allocate new selector. */
  int maxSelectorValue;

  public ClassType curClass;
  public ClassType mainClass;
  /** Generated class that extends ModuleBody.  Normally same as mainClass. */
  public ClassType moduleClass;

  public LambdaExp curLambda;
  public ModuleExp mainLambda;
  public Variable thisDecl;

  /** Contains "$class" if the module is static; otherwise null. */
  Variable moduleInstanceVar;

  /** A code, one of the following constants, indicating how far along
   * we are in the parsing/compilation process.
   * These codes are even integers for completed stages and odd integers
   * for begun but not completed stages. */
  private int state;
  /** Returns a code indicating how far along
   * we are in the parsing/compilation process. */
  public int getState () { return state; }
  public void setState (int state) { this.state = state; }
  /** State code for initial pre-parse looking for module name. */
  public static final int PROLOG_PARSING = 1;
  /** We have determined the module name and class, but not finished parsing. */
  public static final int PROLOG_PARSED = 2;
  /** State code indicating the entire module has been parsed. */
  public static final int BODY_PARSED = 4;
  /** State code for lexical bindings having been resolved. */ 
  public static final int RESOLVED = 6;
  /** State code when initial tree-walking (PushApply) are done. */
  public static final int PRE_WALKED = 8;
  /** State code when various inlining and optimization passes are done. */
  public static final int WALKED = 10;
  /** State code that various compile-only data has been determined. */
  public static final int COMPILE_SETUP = 12;
  /** State code indicating the bytecode has been generated. */
  public static final int COMPILED = 14;
  /** State code indicating that bytecode has been written to its target. */
  public static final int CLASS_WRITTEN = 16;
  public static final int ERROR_SEEN = 100;

  public Lexer lexer;

    private boolean pedantic;
    public boolean isPedantic() { return pedantic; }
    public void setPedantic(boolean value) { pedantic = value; }

  /** Used to access the "main" instance.
   * If we're compiling a static module (FIXME: and not init-run), then {@code moduleInstanceMainField}
   * is a field in {@code mainClass} named {@code "$instance"} that
   * points to the single instance of the module.
   */
  Field moduleInstanceMainField;

  /** Stack of quads of (ModuleInfo, ScopeExp, position, formSize). */
  public java.util.Stack<Object> pendingImports;

  public void pushPendingImport(ModuleInfo info, ScopeExp defs, FormStack forms)
  {
    if (pendingImports == null)
      pendingImports = new java.util.Stack<Object>();
    pendingImports.push(info);
    pendingImports.push(defs);
    Expression posExp = new ReferenceExp((Object) null);
    posExp.setLine(this);
    pendingImports.push(posExp);
    pendingImports.push(forms.lastPair());
  }

    public Map<String,ModuleInfo> subModuleMap;

    /* Write out implicitly-compiled classes.
     * Compare javac's -implicit:class flag.  Current not set.
     */
    public static boolean writeImplicitClasses = false;
    
  /** If true, print out expressions after parsing and before optimizations. */
  public static boolean debugPrintExpr = false;

  /** If true, print out final expressions after optimizations etc. */
  public static boolean debugPrintFinalExpr;

  public static boolean debugPrintANF = false;
  public static boolean enableANF = false;
  
  public static Options options = new Options();
  public static Options.OptionInfo fullTailCallsVariable =
    options.add("full-tailcalls",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
		"support full tailcalls");
  public static Options.OptionInfo mainMethodVariable =
    options.add("main",
                Options.BOOLEAN_OPTION, Boolean.FALSE,
                "generate an application, with a main method");
  public static Options.OptionInfo warnUnreachable =
    options.add("warn-unreachable",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
		"warn if this code can never be executed");
  public static Options.OptionInfo warnVoidUsed =
    options.add("warn-void-used",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
		"warn if void-valued expression is used");
  public static Options.OptionInfo warnUndefinedVariable =
    options.add("warn-undefined-variable",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
		"warn if no compiler-visible binding for a variable");
  public static Options.OptionInfo warnUnknownMember =
    options.add("warn-unknown-member",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
		"warn if referencing an unknown method or field");
  public static Options.OptionInfo warnInvokeUnknownMethod =
    options.add("warn-invoke-unknown-method",
                Options.BOOLEAN_OPTION, warnUnknownMember,
		"warn if invoke calls an unknown method (subsumed by warn-unknown-member)");
  public static Options.OptionInfo warnUnused =
    options.add("warn-unused",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
                "warn if a variable is usused or code never executed");
  public static Options.OptionInfo warnAsError =
    options.add("warn-as-error", Options.BOOLEAN_OPTION, Boolean.FALSE,
		"Make all warnings into errors");

  public Options currentOptions = new Options(options);

  public boolean generateMainMethod ()
  {
    return currentOptions.getBoolean(mainMethodVariable);
  }
  
  public boolean warnUnreachable ()
  {
    return currentOptions.getBoolean(warnUnreachable);
  }
  public boolean warnUndefinedVariable ()
  {
    return currentOptions.getBoolean(warnUndefinedVariable);
  }
  public boolean warnUnknownMember ()
  {
    return currentOptions.getBoolean(warnUnknownMember);
  }
  public boolean warnInvokeUnknownMethod ()
  {
    return currentOptions.getBoolean(warnInvokeUnknownMethod);
  }
  public boolean warnUnused()
  {
    return currentOptions.getBoolean(warnUnused);
  }
  public boolean warnVoidUsed()
  {
    return (!enableANF) && currentOptions.getBoolean(warnVoidUsed);
  }
  public boolean warnAsError ()
  {
    return currentOptions.getBoolean(warnAsError);
  }

  /** Get a named boolean option. */
  public final boolean getBooleanOption (String key, boolean defaultValue)
  {
    return currentOptions.getBoolean(key, defaultValue);
  }

  /** Get a named boolean option. */
  public final boolean getBooleanOption (String key)
  {
    return currentOptions.getBoolean(key);
  }

  public static int defaultClassFileVersion =
    /* #ifdef JAVA8 */
    // ClassType.JDK_1_8_VERSION
    /* #else */
    /* #ifdef JAVA7 */
    ClassType.JDK_1_7_VERSION
    /* #else */
    /* #ifdef JAVA6 */
    // ClassType.JDK_1_6_VERSION
    /* #else */
    /* #ifdef JAVA5 */
    // ClassType.JDK_1_5_VERSION
    /* #else */
    // ClassType.JDK_1_1_VERSION
    /* #endif */
    /* #endif */
    /* #endif */
    /* #endif */
    ;

  /** The default calling convention.
   * One of the following CALL_WITH_xxx values. */
  public static int defaultCallConvention;
  public static final int CALL_WITH_UNSPECIFIED = 0;
  /** Plain calling convention, using regular Java parameters and returns. */
  public static final int CALL_WITH_RETURN = 1;
  /** Function results are written to the current CallContext's Consumer. */
  public static final int CALL_WITH_CONSUMER = 2;
  /** Like CALL_WITH_CONSUMER, but handle full on-stack-growing tail-calls. */
  public static final int CALL_WITH_TAILCALLS = 3;
  /** Support for full continuations.  Not implemented. */
  public static final int CALL_WITH_CONTINUATIONS = 4;

    public int currentCallConvention() {
        Object ft = currentOptions.getLocal("full-tailcalls");
        if (ft instanceof Boolean)
            return ((Boolean) ft).booleanValue()
                ? Compilation.CALL_WITH_TAILCALLS
                : Compilation.CALL_WITH_RETURN;
        return defaultCallConvention;
    }

  public boolean usingCPStyle()
  { return currentCallConvention() == CALL_WITH_CONTINUATIONS; }
  public boolean usingTailCalls()
  { return currentCallConvention() >= CALL_WITH_TAILCALLS; }
  public boolean usingCallContext()
  { return currentCallConvention() >= CALL_WITH_CONSUMER; }

  public static final int MODULE_NONSTATIC = -1;
  public static final int MODULE_STATIC_DEFAULT = 0;
  public static final int MODULE_STATIC = 1;
  public static final int MODULE_STATIC_RUN = 2;
  /** Default for whether a module is static.
   * If {@code moduleStatic > 0},
   *   then {@code (module-static #t)} is implied by default.
   * If {@code moduleStatic == MODULE_STATIC_RUN},
   *   then {@code <clinit>} calls {@code run}.
   * If {@code moduleStatic < 0},
   *   then {@code (module-static #f)} is implied by default.
   */
  public static int moduleStatic = MODULE_STATIC_DEFAULT;

  ClassType[] classes;
  int numClasses;

  /** When immediate, the ClassLoader we will load the compiled
   * classes from. */
  ArrayClassLoader loader;

  /** True if the compiled result will be immediately loaded. */ 
  public boolean immediate;

  /** Compilation was explicitly requested, rather than being a dependency. */
  public boolean explicit;

  /** The current method. */
  public Method method;

  Method clinitMethod;

  public final CodeAttr getCode() { return method.getCode(); }

  int method_counter;

  /* When multiple procedures are compiled into a single method,
     we use a switch to jump to the correct part of the method. */
  SwitchState fswitch;

  Field fswitchIndex;

  // Various standard classes
  static public ClassType typeObject = Type.objectType;
  static public ClassType scmBooleanType = ClassType.make("java.lang.Boolean");
  static public ClassType typeString = ClassType.make("java.lang.String");
  static public ClassType typeCharSequence = ClassType.make("java.lang.CharSequence");
  static public ClassType javaStringType = typeString;
  static public ClassType scmKeywordType = ClassType.make("gnu.expr.Keyword");
  static public ClassType scmSequenceType = ClassType.make("gnu.lists.Sequence");
  public static final ClassType typeList = ClassType.make("java.util.List");
  static public ClassType scmListType = ClassType.make("gnu.lists.LList");
  static public ClassType typePair = ClassType.make("gnu.lists.Pair");
  public static final ClassType typeConstVector = ClassType.make("gnu.lists.ConstVector");
  public static final ArrayType objArrayType = ArrayType.make(typeObject);
  static public ClassType typeRunnable = ClassType.make("java.lang.Runnable");
  static public ClassType typeRunnableModule = ClassType.make("gnu.expr.RunnableModule");
  public static ClassType typeType = ClassType.make("gnu.bytecode.Type");
  public static ClassType typeObjectType
    = ClassType.make("gnu.bytecode.ObjectType");
  public static ClassType typeClass = Type.javalangClassType;
  static public ClassType typeClassType = ClassType.make("gnu.bytecode.ClassType");
  static public ClassType typeProcedure
    = ClassType.make("gnu.mapping.Procedure");
  static public ClassType typeLanguage
    = ClassType.make("gnu.expr.Language");
  static public ClassType typeEnvironment
    = ClassType.make("gnu.mapping.Environment");
  static public ClassType typeLocation
    = ClassType.make("gnu.mapping.Location");
  public static final ClassType typeLocationProc
    = ClassType.make("gnu.mapping.LocationProc");
  static public ClassType typeFieldLocation
    = ClassType.make("gnu.kawa.reflect.FieldLocation");
  static public ClassType typeStaticFieldLocation
    = ClassType.make("gnu.kawa.reflect.StaticFieldLocation");
  static public ClassType typeSymbol
    = ClassType.make("gnu.mapping.Symbol");
  static public final Field trueConstant
    = scmBooleanType.getDeclaredField("TRUE"); 
  static public final Field falseConstant
    = scmBooleanType.getDeclaredField("FALSE");
    public static final Field voidConsumerInstanceField
        = ClassType.make("gnu.lists.VoidConsumer")
        .getDeclaredField("instance");

  static Method makeListMethod;
  
  public static final Type[] int1Args = { Type.intType  };
  public static final Type[] string1Arg = { javaStringType };
  public static final Type[] sym1Arg = string1Arg;

  static {
    Type[] makeListArgs = { objArrayType, Type.intType  };
    makeListMethod = scmListType.addMethod ("makeList",
					     makeListArgs, scmListType,
					     Access.PUBLIC|Access.STATIC);
  }

  public static Method getCurrentEnvironmentMethod
    = typeEnvironment.addMethod("getCurrent", Type.typeArray0,
				typeEnvironment,Access.PUBLIC|Access.STATIC);

  public static Type[] apply0args = Type.typeArray0;
  public static Type[] apply1args = { typeObject };
  public static Type[] apply2args = { typeObject, typeObject };
  public static Type[] applyNargs = { objArrayType };

  static Method checkArgCountMethod;

  public static Method apply0method = typeProcedure.addMethod
  ("apply0", apply0args, typeObject, Access.PUBLIC|Access.FINAL);

  public static Method apply1method;
  public static Method apply2method;
  public static Method apply3method;
  public static Method apply4method;
  public static Method applyNmethod;

  static
  {
    apply1method = typeProcedure.addMethod ("apply1", apply1args,
						typeObject, Access.PUBLIC);
    apply2method = typeProcedure.addMethod ("apply2", apply2args,
						typeObject, Access.PUBLIC);
    Type[] apply3args = { typeObject, typeObject, typeObject };
    apply3method = typeProcedure.addMethod ("apply3", apply3args,
						typeObject, Access.PUBLIC);
    Type[] apply4args = { typeObject , typeObject, typeObject, typeObject};
    apply4method = typeProcedure.addMethod ("apply4", apply4args,
						typeObject, Access.PUBLIC);
    applyNmethod = typeProcedure.addMethod ("applyN", applyNargs,
						typeObject, Access.PUBLIC);
    Type[] args = new Type[2];
    args[0] = typeProcedure;
    args[1] = Type.intType;
    checkArgCountMethod
      = typeProcedure.addMethod("checkArgCount", args, Type.voidType,
				   Access.PUBLIC|Access.STATIC);
  }

  public static Method[] applymethods = {
    apply0method, apply1method, apply2method, apply3method,
    apply4method, applyNmethod };

  public static ClassType typeProcedure0
    = ClassType.make("gnu.mapping.Procedure0");
  public static ClassType typeProcedure1
    = ClassType.make("gnu.mapping.Procedure1");
  public static ClassType typeProcedure2
    = ClassType.make("gnu.mapping.Procedure2");
  public static ClassType typeProcedure3
    = ClassType.make("gnu.mapping.Procedure3");
  public static ClassType typeProcedure4
    = ClassType.make("gnu.mapping.Procedure4");
  public static ClassType typeProcedureN
    = ClassType.make("gnu.mapping.ProcedureN");
  public static ClassType typeModuleBody
    = ClassType.make("gnu.expr.ModuleBody");
  public static ClassType typeApplet = ClassType.make("java.applet.Applet");
  public static ClassType typeServlet = ClassType.make("gnu.kawa.servlet.KawaServlet");

  /* Classes, fields, and methods used wgen usingCPStyle". */
  public static ClassType typeCallContext
    = ClassType.make("gnu.mapping.CallContext");
  public static final ClassType typeConsumer
    = ClassType.make("gnu.lists.Consumer");
  public static Method getCallContextInstanceMethod
    = typeCallContext.getDeclaredMethod("getInstance", 0);
  public static ClassType typeValues
    = ClassType.make("gnu.mapping.Values");
  public static Field noArgsField
    = typeValues.getDeclaredField("noArgs");
  public static Field pcCallContextField
    = typeCallContext.getDeclaredField("pc");
  public static ClassType typeMethodProc
  = ClassType.make("gnu.mapping.MethodProc");
  public static ClassType typeCompiledProc
  = ClassType.make("gnu.expr.CompiledProc");
  //  public static Field numArgsCallFrameField = typeCallFrame.getDeclaredField("numArgs");
  public static Field argsCallContextField
    = typeCallContext.getDeclaredField("values");
  public static Field procCallContextField
    = typeCallContext.getDeclaredField("proc");
  private static Type[] applyCpsArgs = { typeCallContext};
  public static Method applyCpsMethod
    = typeProcedure.addMethod("apply", applyCpsArgs, Type.voidType,
				 Access.PUBLIC);

  public static ClassType[] typeProcedureArray = {
    typeProcedure0, typeProcedure1, typeProcedure2, typeProcedure3,
    typeProcedure4 };

    public static final Method getNextArgMethod = typeCallContext.getDeclaredMethod("getNextArg", 0);

  /** Rembembers stuff to do in <clinit> of main class. */
  Initializer clinitChain;

  LitTable litTable;

  int langOptions;
  
  /** True if we should generate an Applet. */
  public boolean generatingApplet ()
  {
    return (langOptions & Language.PARSE_FOR_APPLET) != 0;
  }

  /** True if we should generate a Servlet. */
  public boolean generatingServlet ()
  {
    return (langOptions & Language.PARSE_FOR_SERVLET) != 0;
  }

  public boolean sharedModuleDefs ()
  {
    return (langOptions & Language.PARSE_CURRENT_NAMES) != 0;
  }

  public void setSharedModuleDefs (boolean shared)
  {
    if (shared)
      langOptions |= Language.PARSE_CURRENT_NAMES;
    else
      langOptions &= ~Language.PARSE_CURRENT_NAMES;
  }

  public final ClassType getModuleType()
  {
    return typeModuleBody;
  }

    /** Emit code to "evaluate" a compile-time constant.
     * This is the normal external interface.
     * @param value the value to be compiled
     */
    public void compileConstant (Object value) {
        gnu.bytecode.CodeAttr code = getCode();
        if (value == null)
            code.emitPushNull();
        else if (value instanceof String && ! immediate)
            code.emitPushString((String) value);
        else {
            Literal literal = litTable.findLiteral(value);
            // if (immediate) maybe we want to use a differnet approach.
            if (literal.field == null)
                literal.assign(litTable);
            code.emitGetStatic(literal.field);
        }
    }

  public static boolean inlineOk = true;

  public boolean inlineOk (Expression proc)
  {
    if (proc instanceof LambdaExp)
      {
        LambdaExp lproc = (LambdaExp) proc;
        Declaration nameDecl = lproc.nameDecl;
	// The compiler gets confused if we turn off inlining for nested
	// procedures - and they can't be rebound anyway.
        if (nameDecl == null || nameDecl.getSymbol() == null
            || ! (nameDecl.context instanceof ModuleExp))
          return true;
        if (immediate
            && nameDecl.isPublic()
            && ! lproc.getFlag(LambdaExp.OVERLOADABLE_FIELD)
            && (curLambda == null || lproc.topLevel() != curLambda.topLevel()))
          return false;
      }
    return inlineOk;
  }

    public boolean inlineOk(Procedure proc) {
        if (immediate && proc instanceof CompiledProc) {
            Class moduleClass = ((CompiledProc) proc).getModuleClass();
            if (moduleClass.getClassLoader() instanceof ArrayClassLoader)
                return false;
        }
        return inlineOk;
    }

    /** Should this inlineable method by inlined?
     * Usually it's best to not inline a module-level function, since that
     * makes stack traces less helpful, and increases the risk of
     * methods getting too big.
     */
    static boolean avoidInline(LambdaExp proc) {
        return proc.getOuter() instanceof ModuleExp && proc.nameDecl != null;
    }

  public boolean isApplyFunction (Expression exp)
  {
    return false;
  }

  /** A simple apply function maps actual arguments to formals directly.
   * E.g. no distribution of multiple values.
   */
  public boolean isSimpleApplyFunction (Expression exp)
  {
    return false;
  }

  public void compileConstant (Object value, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    if (value instanceof Values && target instanceof ConsumerTarget)
      {
        Object[] values = ((Values) value).getValues();
        int len = values.length;
        for (int i = 0;  i < len;  i++)
          {
            compileConstant(values[i],
                            ((ConsumerTarget) target).getSingleTarget());
          } 
        return;
      }
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarg = (ConditionalTarget) target;
	getCode().emitGoto(getLanguage().isTrue(value) ? ctarg.ifTrue
			   : ctarg.ifFalse);
	return;
      }
    if (target instanceof StackTarget)
      {
	Type type = ((StackTarget) target).getType();
	if (type instanceof LazyType)
	  type = ((LazyType) type).getValueType();
	if (type instanceof PrimType)
	  {
	    try
	      {
		String signature = type.getSignature();
		CodeAttr code = getCode();
		char sig1 = (signature == null || signature.length() != 1) ? ' '
		  : signature.charAt(0);
		if (value instanceof Number)
		  {
		    Number num = (Number) value;
		    switch (sig1)
		      {
                      case 'C':
		      case 'I':
			code.emitPushInt(num.intValue());
			return;
		      case 'S':
			code.emitPushInt(num.shortValue());
			return;
		      case 'B':
			code.emitPushInt(num.byteValue());
			return;
		      case 'J':
			code.emitPushLong(num.longValue());
			return;
		      case 'F':
			code.emitPushFloat(num.floatValue());
			return;
		      case 'D':
			code.emitPushDouble(num.doubleValue());
			return;
		      }
		  }
                // FIXME we should move this into a new method
                // PrimType#pushValue(Object value), with LangPrimType override.
                if (type == LangPrimType.characterType
                    || type == LangPrimType.characterOrEofType)
                  {
                    if (value instanceof Char)
                      {
                        code.emitPushInt(((Char) value).intValue());
                        return;
                      }
                    if (value instanceof Character)
                      {
                        code.emitPushInt(((Character) value).charValue());
                        return;
                      }
                    if (value == gnu.lists.Sequence.eofValue
                        && type == LangPrimType.characterOrEofType)
                      {
                        code.emitPushInt(-1);
                        return;
                     }
                  }
		if (sig1 == 'C')
		  {
		    code.emitPushInt((int) ((PrimType) type).charValue(value));
		    return;
		  }
		if (sig1 == 'Z')
		  {
		    boolean val = getLanguage().isTrue(value);
		    code.emitPushInt(val ? 1 : 0);
		    return;
		  }
	      }
	    catch (ClassCastException ex)
	      {
		// should print an ERROR.
	      }
	  }
        if (type == Compilation.typeClass && value instanceof ClassType)
          {
            loadClassRef((ClassType) value);
            return;
          }
        try
          {
            value = type.coerceFromObject(value);
          }
        catch (Exception ex)
          {
	    StringBuffer sbuf = new StringBuffer();
	    if (value == Values.empty)
	      sbuf.append("cannot convert void to ");
	    else
	      {
		sbuf.append("cannot convert literal (of type ");
                if (value == null)
                  sbuf.append("<null>");
                else
                  sbuf.append(value.getClass().getName());
		sbuf.append(") to ");
	      }
	    sbuf.append(type);
            error('w', sbuf.toString());
         }
      }
    compileConstant(value);
    target.compileFromStack(this,
                            value == null ? target.getType()
                            : Type.make(value.getClass()));
  }

    /** Push language-specific boxed true or false value. */
    public void emitPushBoolean(boolean value) {
        CodeAttr code = getCode();
        Object valObject = language.booleanObject(value);
        if (valObject == Boolean.TRUE)
            code.emitGetStatic(Compilation.trueConstant);
        else if (valObject == Boolean.FALSE)
            code.emitGetStatic(Compilation.falseConstant);
        else compileConstant(valObject);
    }

  /** Generate code to test if an object is considered true.
   * Assume the object has been pushed on the JVM stack.
   * Generate code to push (unboxed) true or false as appropriate. */
  public void emitCoerceToBoolean()
  {
    CodeAttr code = getCode();
    Label trueLabel = new Label(code);
    Label falseLabel = new Label(code);
    ConditionalTarget ctarget
        = new ConditionalTarget(trueLabel, falseLabel, getLanguage());
    ctarget.compileFromStack(this, Type.objectType);
    code.emitIfThen();
    trueLabel.define(code);
    code.emitPushInt(1);
    code.emitElse();
    falseLabel.define(code);
    code.emitPushInt(0);
    code.emitFi();
  }

    /** Hook for language-specific handling in ConditionalTarget.
     * @param stackType Value to be treated as boolean, already pushed.
     * @return null if we've handled the conditional transfer;
     *    otherwise type value has been converted to (usually booleanType).
     */
    public Type asBooleanValue(ConditionalTarget target, Type stackType) {
        return stackType;
    }

    boolean dumpingInitializers;

    private void dumpInitializers(Initializer inits) {
        dumpingInitializers = true;
        for (Initializer init = Initializer.reverse(inits);
             init != null;  init = init.next)
            init.emit(this);
        dumpingInitializers = false;
    }

    static final Comparator<ClassType> 
            classTypeComparator = new Comparator<ClassType>() {
        public int compare(ClassType arg0, ClassType arg1) {
            return arg0.getName().compareTo(arg1.getName());
        }
    };

    boolean classesArrayIsSorted;

  /** Search this Compilation for a ClassType with a given name.
   * @param name the name of the class desired
   * @return the matching ClassType, or null if none is found */
    public ClassType findNamedClass(String name) {

        if (classes == null || numClasses == 0)
            return null;

        if (name.equals(classes[0].getName()))
                return classes[0];

        if (numClasses == 1)
            return null;

        if (! classesArrayIsSorted) {
            Arrays.sort(classes, 1, numClasses, classTypeComparator);
            classesArrayIsSorted = true;
        }

        ClassType nameType = new ClassType(name); // wrapper for key
        int index = Arrays.binarySearch
                      (classes, 1, numClasses, nameType, classTypeComparator);
        return (index > -1) ? classes[index] : null;
  }

  public static String classPrefixDefault = "";
  /** If non-null: a prefix for generateClassName to prepend to names. */
  public String classPrefix = classPrefixDefault;

  /** Recusive helper function to reverse order of words in hostname. */
  private static void putURLWords(String name, StringBuffer sbuf)
  {
    int dot = name.indexOf('.');
    if (dot > 0)
      {
	putURLWords(name.substring(dot+1), sbuf);
	sbuf.append('.');
	name = name.substring(0, dot);
      }
    sbuf.append(name);
  }

  /** Map a URI to a package/class name.
   * Similar to the JAXB mangling, and that in the Java language spec.
   */
  public static String mangleURI (String name)
  {
    boolean hasSlash = name.indexOf('/') >= 0;
    int len = name.length();
    if (len > 6 && name.startsWith("class:"))
      return name.substring(6);
    // Remove "http:" or "urn:".
    if (len > 5 && name.charAt(4) == ':'
	&& name.substring(0, 4).equalsIgnoreCase("http"))
      {
	name = name.substring(5);
	len -= 5;
	hasSlash = true;
      }
    else if (len > 4 && name.charAt(3) == ':'
	     && name.substring(0, 3).equalsIgnoreCase("uri"))
      {
	name = name.substring(4);
	len -= 4;
      }
    int start = 0;
    StringBuffer sbuf = new StringBuffer();
    for (;;)
      {
	int slash = name.indexOf('/', start);
	int end = slash < 0 ? len : slash;
	boolean first = sbuf.length() == 0;
	if (first && hasSlash)
	  {
	    // Remove initial "www.".
	    String host = name.substring(start, end);
	    if (end - start > 4 && host.startsWith("www."))
	      host = host.substring(4);
	    // Reverse order of words in "host" part.
	    putURLWords(host, sbuf);
	  }
	else if (start != end)
	  {
	    if (! first)
	      sbuf.append('.');
	    if (end == len)
	      {
		int dot = name.lastIndexOf('.', len);
		if (dot > start + 1 && ! first)
		  {
		    // Remove file extension:
		    int extLen = len - dot;
		    if (extLen <= 4
			|| (extLen == 5 && name.endsWith("html")))
		      {
			len -= extLen;
			end = len;
			name = name.substring(0, len);
		      }
		  }
	      }
	    sbuf.append(name.substring(start, end));
	  }
	if (slash < 0)
	  break;
	start = slash + 1;
      }
    return sbuf.toString();
  }

    /** Mangle a simple class or package name.
     * Does not handle qualified names.
     */
    public static String mangleClassName(String name) {
        return mangleSymbolic(name, false, false);
    }

    /** Mangle a possibly-qualified class name. */
    public static String mangleQualifiedName(String name) {
        return mangleSymbolic(name, true, false);
    }

  public static String mangleName (String name)
  {
    return Language.mangleName(name, -1);
  }

    /** Mangle according to John Rose's "Symbolic Freedom in the VM".
     * {@linkplain https://blogs.oracle.com/jrose/entry/symbolic_freedom_in_the_vm See this article.}
     * @param allowDots True if we're mangling a qualified name with dots,
     *   which should not be mangled.
     * @param force True if should escape '\\' even if that is the
     *   only disallowed character.  The may cause an already-mangled name
     *   to be doubly mangled.
     */
    public static String mangleSymbolic(String name,
                                        boolean allowDots, boolean force) {
        StringBuilder sbuf = null;
        int len = name.length();
        if (len == 0)
            return "\\=";
        int dangerous = 0;
        for (int i = 0; i < len; i++) {
            char ch = name.charAt(i);
            char ch2;
            switch (ch) {
            case '/':  ch2 = '|'; break;
            case '.':  ch2 = allowDots ? 0 : ','; break;
            case ';':  ch2 = '?'; break;
            case '$':  ch2 = '%'; break;
            case '<':  ch2 = '^'; break;
            case '>':  ch2 = '_'; break;
            case '[':  ch2 = '{'; break;
            case ']':  ch2 = '}'; break;
            case ':':  ch2 = '!'; break;
            case '\\': ch2 = '-'; break;
            default:   ch2 = 0;
            }
            if (ch2 != 0 && ch != '\\')
                dangerous++; 
            if (sbuf != null) {
                if (ch2 == 0)
                    sbuf.append(ch);
                else
                    sbuf.append('\\').append(ch2);
            } else if (ch2 != 0) {
                sbuf = new StringBuilder();
                if (i != 0)
                    sbuf.append("\\=");
                sbuf.append(name, 0, i);
                sbuf.append('\\').append(ch2);
            }
        }
        return sbuf == null || (dangerous == 0 && ! force) ? name
            : sbuf.toString();
    }

    public static String demangleSymbolic(String name) {
        int len = name.length();
        if (len < 2 || name.charAt(0) != '\\')
            return name;
        StringBuilder sbuf = new StringBuilder();
        int i = name.charAt(1) == '=' ? 2 : 0;
        while (i < len) {
            char ch = name.charAt(i);
            if (ch == '\\' && i+1 < len) {
                char ch2 = name.charAt(i+1);
                char ch1;
                switch (ch2) {
                case '|':  ch1 = '/'; break;
                case ',':  ch1 = '.'; break;
                case '?':  ch1 = ';'; break;
                case '%':  ch1 = '$'; break;
                case '^':  ch1 = '<'; break;
                case '_':  ch1 = '>'; break;
                case '{':  ch1 = '['; break;
                case '}':  ch1 = ']'; break;
                case '!':  ch1 = ':'; break;
                case '-':  ch1 = '\\'; break;
                default: ch1 = 0; break;
                }
                if (ch1 != 0)
                    sbuf.append(ch1);
                else
                    sbuf.append('\\').append(ch2);
                i += 2;
            } else {
                sbuf.append(ch);
                i += 1;
            }
        }
        return sbuf.toString();
    }

  /** Convert a string to a safe Java identifier.
   * @param reversible if we should use an invertible mapping.
   */
  public static String mangleName (String name, boolean reversible)
  {
    return Language.mangleName(name, reversible ? 1 : -1);
  }

  public static String mangleNameIfNeeded (String name)
  {
    return Language.mangleNameIfNeeded(name);
  }

  /** Demangle a three-character mangling starting with '$'.
   * UNFINISHED!
   */
  public static char demangle2(char char1, char char2)
  {
    switch (char1 << 16 | char2)
      {
      case 'A' << 16 | 'm':  return '&';
      case 'A' << 16 | 't':  return '@';
      case 'C' << 16 | 'l':  return ':';
      case 'C' << 16 | 'm':  return ',';
      case 'D' << 16 | 'q':  return '\"';
      case 'D' << 16 | 't':  return '.';
      case 'E' << 16 | 'q':  return '=';
      case 'E' << 16 | 'x':  return '!';
      case 'G' << 16 | 'r':  return '>';
      case 'L' << 16 | 'B':  return '[';
      case 'L' << 16 | 'C':  return '{';
      case 'L' << 16 | 'P':  return '(';
      case 'L' << 16 | 's':  return '<';
      case 'M' << 16 | 'c':  return '%';
      case 'M' << 16 | 'n':  return '-';
      case 'N' << 16 | 'm':  return '#';
      case 'P' << 16 | 'c':  return '%';
      case 'P' << 16 | 'l':  return '+';
      case 'Q' << 16 | 'u':  return '?';
      case 'R' << 16 | 'B':  return ']';
      case 'R' << 16 | 'C':  return '}';
      case 'R' << 16 | 'P':  return ')';
      case 'S' << 16 | 'C':  return ';';
      case 'S' << 16 | 'l':  return '/';
      case 'S' << 16 | 'q':  return '\\';
      case 'S' << 16 | 't':  return '*';
      case 'T' << 16 | 'l':  return '~';
      case 'U' << 16 | 'p':  return '^';
      case 'V' << 16 | 'B':  return '|';
      }
    return (char) (-1);
  }

  public static String demangleName(String name)
  {
    return demangleName(name, false);
  }

  public static String demangleName(String name, boolean reversible)
  {
    StringBuffer sbuf = new StringBuffer();
    int len = name.length();
    boolean mangled = false;
    boolean predicate = false;
    boolean downCaseNext = false;
    for (int i = 0;  i < len;  i++)
      {
	char ch = name.charAt(i);
        if (i == 0 && ch == '$' && len >= 3 && name.charAt(1) == 'N')
          {
            i = 1;
            mangled = true;
            continue;
          }
	if (downCaseNext && ! reversible)
	  {
	    ch = Character.toLowerCase(ch);
	    downCaseNext = false;
	  }
	char d;
	if (!reversible
	    && ch == 'i' && i == 0 && len > 2 && name.charAt(i+1) == 's'
	    && ! Character.isLowerCase(d = name.charAt(i+2)))
	  {
	    mangled = true;
	    predicate = true;
	    i++;
	    if (Character.isUpperCase(d) || Character.isTitleCase(d))
	      {
		sbuf.append(Character.toLowerCase(d));
		i++;
		continue;
	      }
	    continue;
	  }
	else if (ch == '$' && i + 2 < len)
	  {
	    char c1 = name.charAt(i+1);
	    char c2 = name.charAt(i+2);
	    d = Compilation.demangle2(c1, c2);
	    if (d != (char)(-1))
	      {
		sbuf.append(d);
		i += 2;
		mangled = true;
		downCaseNext = true;
		continue;
	      }
	    else if (c1 == 'T' && c2 == 'o' && i + 3 < len
		     && name.charAt(i+3) == '$')
	      {
		sbuf.append("->");
		i += 3;
		mangled = true;
		downCaseNext = true;
		continue;
	      }
	  }
	else if (! reversible && i > 1
		 && (Character.isUpperCase(ch) || Character.isTitleCase(ch))
		 && (Character.isLowerCase(name.charAt(i-1))))
	  {
	    sbuf.append('-');
	    mangled = true;
	    ch = Character.toLowerCase(ch);
	  }
	sbuf.append(ch);
      }
    if (predicate)
      sbuf.append('?');
    return mangled ? sbuf.toString() : name;
  }

  /** Generate an unused class name.
   * @param hint the requested name (or prefix)
   * @return a unique class name.
   */
  public String generateClassName (String hint)
  {
    hint = mangleClassName(hint);
    if (mainClass != null)
      hint = mainClass.getName() + '$' + hint;
    else if (classPrefix != null)
      hint = classPrefix + hint;
    if (findNamedClass (hint) == null)
      return hint;
    for (int i = 0;  ; i++)
      {
	String new_hint = hint + i;
	if (findNamedClass (new_hint) == null)
	  return new_hint;
      }
  }

  public Compilation (Language language, SourceMessages messages,
		      NameLookup lexical)
  {
    this.language = language;
    this.messages = messages;
    this.lexical = lexical;
  }

  public void outputClass (String directory) throws IOException
  {
    char dirSep = File.separatorChar;
    for (int iClass = 0;  iClass < numClasses;  iClass++)
      {
	ClassType clas = classes[iClass];
	String out_name
	  = (directory + clas.getName().replace('.', dirSep)
	     + ".class");
	String parent = new File(out_name).getParent();
	if (parent != null)
	  new File(parent).mkdirs();
	clas.writeToFile(out_name);
      }
    getMinfo().cleanupAfterCompilation();
  }

  public void cleanupAfterCompilation ()
  {
    for (int iClass = 0;  iClass < numClasses;  iClass++)
      classes[iClass].cleanupAfterCompilation();
    classes = null;
    ModuleInfo minfo = getMinfo();
    minfo.className = mainClass.getName(); // In case it hasn't been set yet.
    minfo.setCompilation(null);
    // We don't clear minfo.exp itself, since it might be re-required.
    if (minfo.exp != null)
      minfo.exp.body = null;
    mainLambda.body = null;
    mainLambda = null;
    if (! immediate)
      litTable = null;
  }

  public void compileToArchive (ModuleExp mexp, String fname)
    throws java.io.IOException
  {
    boolean makeJar = false;
    if (fname.endsWith(".zip"))
      makeJar = false;
    else if (fname.endsWith(".jar"))
      makeJar = true;
    else
      {
	fname = fname + ".zip";
	makeJar = false;
      }

    process(COMPILED);

    File zar_file = new File (fname);
    if (zar_file.exists ())
      zar_file.delete ();
    ZipOutputStream zout;
    /* #ifdef JAVA2 */
    if (makeJar)
      zout = new java.util.jar.JarOutputStream(new FileOutputStream(zar_file));
    else
    /* #endif */
      zout = new ZipOutputStream(new FileOutputStream(zar_file));

    byte[][] classBytes = new byte[numClasses][];
    CRC32 zcrc = new CRC32();
    for (int iClass = 0;  iClass < numClasses;  iClass++)
      {
	ClassType clas = classes[iClass];
	classBytes[iClass] = clas.writeToArray ();
	ZipEntry zent = new ZipEntry(clas.getName ().replace ('.', '/')
				     + ".class");

	zent.setSize(classBytes[iClass].length);
	zcrc.reset();
	zcrc.update(classBytes[iClass], 0, classBytes[iClass].length);
	zent.setCrc(zcrc.getValue());

	zout.putNextEntry (zent);
	zout.write (classBytes[iClass]);
      }
    zout.close ();
  }

  // FIXME - make this settable, as it does make .class files bigger.
  public static boolean emitSourceDebugExtAttr = true;

  private void registerClass (ClassType new_class)
  {
    if (classes == null)
      classes = new ClassType[20];
    else if (numClasses >= classes.length)
      {
	ClassType[] new_classes = new ClassType[2 * classes.length];
	System.arraycopy (classes, 0, new_classes, 0, numClasses);
	classes = new_classes;
      }
    new_class.addModifiers(new_class.isInterface() ? Access.PUBLIC
                           : Access.PUBLIC|Access.SUPER);
    if (new_class == mainClass && numClasses > 0)
      {
        // Ensure mainClass is written first when writing an archive.
        new_class = classes[0];
        classes[0] = mainClass;
      }
    classes[numClasses++] = new_class;
    classesArrayIsSorted = false;
  }

  public void addClass (ClassType new_class)
  {
    String fname = getModule().filename;
    if (fname != null)
      {
	if (emitSourceDebugExtAttr)
	  new_class.setStratum(getLanguage().getName());
	new_class.setSourceFile(fname);
      }
    registerClass(new_class);
    new_class.setClassfileVersion(defaultClassFileVersion);
  }

  public boolean makeRunnable ()
  {
    return ! generatingServlet() && ! generatingApplet()
      && ! getModule().staticInitRun()
      && ! getModule()
        .getFlag(ModuleExp.USE_DEFINED_CLASS|ModuleExp.SUPERTYPE_SPECIFIED);
  }

  public void addMainClass (ModuleExp module)
  {
    mainClass = module.classFor(this);
    ClassType type = mainClass;
    ClassType[] interfaces = module.getInterfaces();
    if (interfaces != null)
      type.setInterfaces(interfaces);
    ClassType sup = module.getSuperType();
    if (sup == null)
      {
        if (generatingApplet())
	  sup = typeApplet;
	else if (generatingServlet())
	  sup = typeServlet;
        else if (module.getFlag(ModuleExp.USE_DEFINED_CLASS))
          sup = Type.objectType;
	else
	  sup = getModuleType();
      }
    if (makeRunnable())
      type.addInterface(typeRunnable);
    if (! module.staticInitRun())
      type.addInterface(typeRunnableModule);
    type.setSuper(sup);

    module.compiledType = type;
    addClass(type);
  }

  public final Method getConstructor (LambdaExp lexp)
  {
    return getConstructor(lexp.getHeapFrameType(), lexp);
  }

  public static final Method getConstructor (ClassType clas, LambdaExp lexp)
  {
    Method meth = clas.getDeclaredMethod("<init>", 0);
    if (meth != null)
      return meth;
    Type[] args;
    if (lexp instanceof ClassExp && lexp.staticLinkField != null)
      {
	args = new Type[1];
	args[0] = lexp.staticLinkField.getType();
      }
    else
      args = apply0args;
    return clas.addMethod("<init>", Access.PUBLIC, args, Type.voidType);
  }

  public final void generateConstructor (LambdaExp lexp)
  {
    generateConstructor (lexp.getHeapFrameType(), lexp);
  }

  public final void generateConstructor (ClassType clas, LambdaExp lexp)
  {
    Method save_method = method;
    Variable callContextSave = callContextVar;
    callContextVar = null;
    ClassType save_class = curClass;
    curClass = clas;
    Method constructor_method = getConstructor(clas, lexp);
    clas.constructor = constructor_method;
    method = constructor_method;
    CodeAttr code = constructor_method.startCode();

    if (lexp instanceof ClassExp && lexp.staticLinkField != null)
      {
	code.emitPushThis();
	code.emitLoad(code.getCurrentScope().getVariable(1));
	code.emitPutField(lexp.staticLinkField);
      }
    ClassType superClass = clas.getSuperclass();
    ClassExp.invokeDefaultSuperConstructor(superClass, this, lexp);

    if (curClass == mainClass
        // Optimization: No point in calling ModuleInfo.register if we aren't
        // compiling a named module.
        && getMinfo() != null && getMinfo().sourcePath != null
        && ! getModule().getFlag(ModuleExp.USE_DEFINED_CLASS))
      {
	code.emitPushThis();
	code.emitInvokeStatic(ClassType.make("gnu.expr.ModuleInfo")
                              .getDeclaredMethod("register", 1));
      }

    if (lexp != null && lexp.initChain != null)
      {
	// Create dummy lambda, for its closureEnv.  This may be needed
	// if init.value contains a reference that uses our heap frame.
	LambdaExp save = curLambda;
	curLambda = new LambdaExp();
	curLambda.closureEnv = code.getArg(0);
	curLambda.setOuter(save);
        Initializer init;
	while ((init = lexp.initChain) != null)
	  {
	    lexp.initChain = null;
	    dumpInitializers(init);
	  }
	curLambda = save;
      }

    if (lexp instanceof ClassExp)
      {
	ClassExp cexp = (ClassExp) lexp;
	callInitMethods(cexp.getCompiledClassType(this),
                        new ArrayList<ClassType>(10));
      }

    code.emitReturn();
    method = save_method;
    curClass = save_class;
    callContextVar = callContextSave;
  }

  /** In an <init> for a generated ClassExp, emit $finit$ calls.
   * This recursively traverses superclasses, and also calls their $finit$.
   * @param clas Class to search for $finit$, and to search supertypes.
   * @param seen array of seen classes, to avoid duplicate $finit$ calls.
   */
  void callInitMethods (ClassType clas, ArrayList<ClassType> seen)
  {
    if (clas == null)
      return;

    String name = clas.getName();
    if ("java.lang.Object".equals(name))
      return;
    // Check for duplicates.
    for (int i = seen.size();  --i >= 0; )
      if (seen.get(i).getName() == name)
	return;
    seen.add(clas);

    // Recusive call to emit $finit$ of super-types.  However, don't do that
    // for clas.getSuperclass(), because our <init> will automatically call
    // the super-class's <init>, which will call its $finit$.
    ClassType[] interfaces = clas.getInterfaces();
    if (interfaces != null)
      {
	int n = interfaces.length;
	for (int i = 0;  i < n;  i++)
	  callInitMethods(interfaces[i], seen);
      }

    int clEnvArgs = 1;
    if (clas.isInterface())
      {	
        if (clas instanceof PairClassType)
          clas = ((PairClassType) clas).instanceType;
        else
          {
            try
              {
                clas = ((ClassType)
                        Type.make(Class.forName(clas.getName() + "$class")));
              }
            catch (Exception ex)
              {
                return;
              }
          }
      }
    else
      clEnvArgs = 0;
    Method meth = clas.getDeclaredMethod("$finit$", clEnvArgs);
    if (meth != null)
      {
	CodeAttr code = getCode();
	code.emitPushThis();
	code.emitInvoke(meth);
      }
  }

    public Method generateCheckMethod(LambdaExp lexp, LambdaExp parent) {
        Method saveMethod = method;
	LambdaExp saveLambda = curLambda;
	ClassType saveClass = curClass;
	curLambda = lexp;
        Method primMethod = lexp.getMainMethod();
	curClass = primMethod.getDeclaringClass();
	/* if not using MethodHandles:
	if (! (curClass.getSuperclass().isSubtype(typeModuleBody)))
	    curClass = moduleClass;
	*/
	Variable callContextSave = callContextVar;
        String primName = primMethod.getName();
        int primNameLength = primName.length();
        if (primNameLength >= 3 && primName.charAt(primNameLength-2) == '$'
            && (primName.charAt(primNameLength-1) == 'V'
                || primName.charAt(primNameLength-1) == 'P'))
            primName = primName.substring(0, primNameLength-2);
        String checkName = primName  + "$check";
        Type[] checkArgs = { typeProcedure, typeCallContext };

        int mcount = 0;
        while (curClass.getDeclaredMethod(checkName, checkArgs) != null) {
            checkName = primMethod.getName() + "$" + (++mcount) + "$check";
        }
        Method checkMethod =
            curClass.addMethod(checkName, Access.PUBLIC|Access.STATIC,
                               checkArgs, (Type) Type.objectType);
        this.method = checkMethod;
	CodeAttr code = method.startCode();
	SourceLocator saveLoc1 = messages.swapSourceLocator(lexp);
	int line = lexp.getLineNumber();
	if (line > 0)
	    code.putLineNumber(lexp.getFileName(), line);
	Variable ctxVar = code.getArg(method.getStaticFlag()?1:2);
        callContextVar = ctxVar;
	//int kin = 0;
	//int scopesToPop = 0;
	int needsThis = primMethod.getStaticFlag() ? 0 : 1;
	int explicitFrameArg = lexp.getNeedsClosureEnv() ? 1-needsThis : 0;
        //   singleArgs + (varArgs ? 2 : 1) < primArgTypes.length ? 1 : 0;
	if (needsThis + explicitFrameArg > 0) {
	    code.emitLoad(code.getArg(0));
            code.emitCheckcast(Compilation.typeCompiledProc);
            code.emitInvoke(Compilation.typeCompiledProc
                            .getDeclaredMethod("getModule", 0));
            //code.emitCheckcast(primMethod.getDeclaringClass()); // FIXME
            if (needsThis > 0)
                code.emitCheckcast(primMethod.getDeclaringClass()); // FIXME
            else
                code.emitCheckcast(lexp.closureEnv.getType());
	}
        Scope scope = code.pushScope();
        lexp.scope = scope;
        Declaration[] keyDecls = generateCheckKeywords(lexp);
        ArrayList<Variable> argVariables = new ArrayList<Variable>();
	Declaration param = lexp.firstDecl();
        if (param != null && param.isThisParameter())
            param = param.nextDecl();
        generateCheckArg(param, lexp, 0, code, keyDecls, argVariables, null);
	messages.swapSourceLocator(saveLoc1);
        this.method = saveMethod;
	curLambda = saveLambda;
	curClass = saveClass;
	callContextVar = callContextSave;
        return checkMethod;
    }

   private static final Comparator<Declaration> keyComparator = 
        new Comparator<Declaration> () {
            public int compare(Declaration d1, Declaration d2) {
                String s1 = (String) d1.getSymbol();
                String s2 = (String) d2.getSymbol();
                return s1.compareTo(s2);
            }
        };

    private Declaration[] generateCheckKeywords(LambdaExp lexp) {
        int nkeys = lexp.keywords == null ? 0 : lexp.keywords.length;
        if (nkeys == 0)
            return null;
        CodeAttr code = getCode();
	Variable ctxVar = callContextVar;
        Scope scope = lexp.scope;
        // A helper array of fake Declarations.
        // We use Declaration as a convenient type for sorting.
        Declaration[] keyDecls = new Declaration[nkeys];
        Declaration[] tmpDecls = new Declaration[nkeys];
        int pin = 0;
        int kin = 0;
        boolean allowOtherKeys = lexp.getFlag(LambdaExp.ALLOW_OTHER_KEYWORDS);
        for (Declaration decl = lexp.firstDecl();
	     decl != null; decl = decl.nextDecl()) {
            if (kin >= nkeys)
                break;
            if (! decl.getFlag(Declaration.IS_PARAMETER)
                || decl.getFlag(Declaration.IS_REST_PARAMETER))
                continue;
            if (pin >= lexp.min_args+lexp.opt_args) {
                Object key = lexp.keywords[kin];
                if (key instanceof Keyword)
                    key = ((Keyword) key).getName();
                Declaration keyDecl = new Declaration(key);
                keyDecl.base = decl;
                keyDecl.evalIndex = kin;
                keyDecls[kin++] = keyDecl;
            }
            pin++;
        }
        java.util.Arrays.sort(keyDecls, keyComparator);
        for (kin = 0;  kin < nkeys;  kin++) {
            Declaration keyDecl = keyDecls[kin];
            Declaration param = keyDecl.base;
            Expression dfault = param.getInitValue();
            boolean simple = dfault instanceof QuoteExp
                && ! param.getFlag(Declaration.IS_SUPPLIED_PARAMETER);
            Variable var;
            keyDecl.setSimple(simple);
            code.emitLoad(ctxVar);
            code.emitPushString((String) keyDecl.getSymbol());
            String mname = allowOtherKeys ? "nextKeywordAllowOthers" : "nextKeyword";
            if (simple) {
                var = scope.addVariable(code, Type.objectType, null);
                dfault.compile(this, Target.pushObject);
                code.emitInvoke(typeCallContext.getDeclaredMethod(mname, 2));
            } else {
                var = scope.addVariable(code, Type.intType, null);
                code.emitInvoke(typeCallContext.getDeclaredMethod(mname, 1));
            }
            keyDecl.var = var;
            code.emitStore(var);
            tmpDecls[keyDecl.evalIndex] = keyDecl;
        }
        System.arraycopy(tmpDecls, 0, keyDecls, 0, nkeys);
        return keyDecls;
    }

    private void generateCheckArg(Declaration param, LambdaExp lexp, int kin, CodeAttr code, Declaration[] keyDecls, ArrayList<Variable> argVariables, Variable suppliedParameterVar) {
	Variable ctxVar = callContextVar;
        if (lexp.keywords != null
            && ! lexp.getFlag(LambdaExp.ALLOW_OTHER_KEYWORDS)
            && param != null && param.getFlag(Declaration.IS_REST_PARAMETER)) {
            code.emitLoad(ctxVar);
            code.emitInvoke(typeCallContext.getDeclaredMethod("checkKeywordsDone", 0));
        }
	if (param == null) {
            code.emitLoad(ctxVar);
            code.emitInvoke(typeCallContext.getDeclaredMethod("checkDone", 0));
            code.emitIfIntNotZero();
            code.emitLoad(ctxVar);
            code.emitReturn();
            code.emitFi();

            generateCheckCall(lexp, code, lexp.getMainMethod(), argVariables);
	    return;
        }
	boolean recurseNeeded = true;
	Type ptype = param.getType();

        // Actual primMethod is already generated.
        // Now we're re-using the parameter declaration to set them.
        // So they should always be plain variables.
        // FIXME: If the parameter is computed by an init expression,
        // we have a problem - which we should handle by lambda lifting?
        param.setSimple(true);

        Variable incoming = null;
        boolean convertNeeded = ptype != Type.objectType;
        Scope scope = code.pushAutoPoppableScope();
        int knext = kin;
        Type itype = param.getType().getImplementationType();

        if (param.getFlag(Declaration.PATTERN_NESTED)) {
                Expression init = param.getInitValue();
                init.compile(this, init.getType());

            } else if (param.getFlag(Declaration.IS_REST_PARAMETER)) {
		int singleArgs = lexp.min_args; // FIXME
                Type lastArgType = ptype.getRawType();
		if (lastArgType instanceof ArrayType) {
		    varArgsToArray(lexp, singleArgs, null/*counter*/, ptype, ctxVar, param.getFlag(Declaration.KEYWORDS_OK));
		    convertNeeded = false;
		} else if (ptype == LangObjType.argVectorType
                           || ptype == LangObjType.argListType) {
		    code.emitLoad(ctxVar);
                    String mname = ptype == LangObjType.argListType
                        ? "getRestArgsList" : "getRestArgsVector";
		    code.emitInvokeVirtual(typeCallContext.getDeclaredMethod(mname, 0));
		    convertNeeded = false;
                }
		else if ("gnu.lists.LList".equals
			 (lastArgType.getName())) {     
		    code.emitLoad(ctxVar);
		    //code.emitDup(); //
		    //code.emitPushInt(singleArgs);
                    //String mname = "peekRestArgsList";// if followed by #!key?
                    String mname = true /* FIXME*/ ? "getRestArgsList"
                        : "getRestPlainList";
		    code.emitInvokeVirtual(typeCallContext.getDeclaredMethod(mname, 0));
		    convertNeeded = false; // FIXME - may need convert if list[T]
		} else {
		    // FIXME
		    throw new Error("unsupported #!rest conversion in "+lexp+" param:"+param+" pt:"+ptype+" cl:"+curClass);
		}
            } else if (param.getFlag(Declaration.IS_SUPPLIED_PARAMETER)
                       && ! param.getFlag(Declaration.IS_PARAMETER)) {
                incoming = suppliedParameterVar;
                convertNeeded = false;
	    } else if (kin >=lexp.min_args && kin < lexp.min_args+lexp.opt_args) {
                // Optional parameter
                code.emitLoad(ctxVar);
                code.emitInvoke(typeCallContext.getDeclaredMethod("haveArg", 0));
                if (param.getFlag(Declaration.IS_SUPPLIED_PARAMETER)) {
                    code.emitDup();
                    suppliedParameterVar =
                        scope.addVariable(code, Type.booleanType, null);
                    code.emitStore(suppliedParameterVar);
                }
                if (lexp.primMethods.length == 1) {
                    code.emitIfIntNotZero();
                    code.emitLoad(ctxVar);
                    code.emitInvoke(getNextArgMethod);
                    code.emitElse();
                    Expression defaultArg = param.getInitValue();
                    defaultArg.compile(this, param.getType());
                    code.emitFi();
                } else {
                    code.emitIfIntEqZero();
                    code.emitLoad(ctxVar);
                    code.emitInvoke(typeCallContext.getDeclaredMethod("checkDone", 0));
                    code.emitIfIntNotZero();
                    code.emitLoad(ctxVar);
                    code.emitReturn();
                    code.emitFi();
                    generateCheckCall(lexp, code,
                                      lexp.primMethods[kin-lexp.min_args], argVariables);
                    code.emitFi();
                    code.emitLoad(ctxVar);
                    code.emitInvoke(getNextArgMethod);
                }
                knext++;
            } else if (kin >= lexp.min_args+lexp.opt_args) {
                // keyword parameter
                int kindex = kin - (lexp.min_args+lexp.opt_args);
                Declaration keyDecl = keyDecls[kindex];
                if (keyDecl.isSimple()) {
                    if (convertNeeded)
                        code.emitLoad(keyDecl.var);
                    else
                        incoming = keyDecl.var;
                } else {
                    code.emitLoad(ctxVar);
                    code.emitLoad(keyDecl.var);
                    if (param.getFlag(Declaration.IS_SUPPLIED_PARAMETER)) {
                        code.emitDup();
                        suppliedParameterVar =
                            scope.addVariable(code, Type.booleanType, null);
                        // {suppliedParameterVar} = {keyDecl.var} >= 0,
                        // where the latter is equal to ({keyDecl.var}>>31)+1.
                        code.emitPushInt(31);
                        code.emitShr();
                        code.emitStore(suppliedParameterVar);
                        code.emitInc(suppliedParameterVar, (short) 1);
                    }
                    code.emitIfIntGEqZero();
                    code.emitLoad(ctxVar);
                    code.emitLoad(keyDecl.var);
                    code.emitInvoke(typeCallContext.getDeclaredMethod("getArgAsObject", 1));;
                    code.emitElse();
                    Expression defaultArg = param.getInitValue();
                    defaultArg.compile(this, param.getType());
                    code.emitFi();
                }
                knext++;
            } else {
                // Required parameter
                knext++;
		code.emitLoad(ctxVar);
                code.emitInvoke(getNextArgMethod);
            }
	    //param.allocateVariable(code, true);
            //boolean saveSimple = param.isSimple();
            //param.setSimple(true);
            int line = param.getLineNumber();
            if (line > 0)
                code.putLineNumber(param.getFileName(), line);
            if (incoming != null && ! convertNeeded)
                param.var = incoming;
            else {
                param.var = scope.addVariable(code, itype, null/*vname*/);
            }
            if (param.parameterForMethod())
                argVariables.add(param.var);
	    if (! convertNeeded) {
                if (incoming == null)
                    code.emitStore(param.var);
	    }
	    else if (ptype instanceof TypeValue ||
                     ptype instanceof PrimType /*FIXME only if number */) {
                // what if incoming!=null && convertNeeded
                if (incoming != null)
                    throw new InternalError();
		StackTarget.forceLazyIfNeeded(this, Type.objectType, ptype);
                incoming = code.addLocal(Type.pointer_type);
                code.emitStore(incoming);
                if (ptype instanceof TypeValue) {
                    ((TypeValue) ptype).emitTestIf(incoming, param, this);
                }
                else {
                    code.emitLoad(incoming);
                    LangPrimType.emitTestIfNumber(incoming, param,
                                                  ptype, this);
                }
                generateCheckArg(param.nextDecl(), lexp, knext, code, keyDecls, argVariables, suppliedParameterVar);
                recurseNeeded = false;
		code.emitElse();
                if (line > 0)
                    code.putLineNumber(param.getFileName(), line);
		code.emitLoad(ctxVar);
                code.emitDup();
		code.emitPushInt(MethodProc.NO_MATCH_BAD_TYPE|kin);
		code.emitInvoke(typeCallContext.getDeclaredMethod("matchError", 1));
		code.emitReturn();
		code.emitFi();
	    } else {
                if (incoming != null)
                    code.emitLoad(incoming);
		StackTarget.forceLazyIfNeeded(this, Type.objectType, ptype);
                // FIXME
		if (ptype instanceof ObjectType &&
		    ptype != Type.objectType
		    && ptype != Type.toStringType) { // FIXME
		    code.emitDup();
		    ptype.getRawType().emitIsInstance(code);
		    code.emitIfIntEqZero();
		    code.emitLoad(ctxVar);
		    code.emitPushInt(MethodProc.NO_MATCH_BAD_TYPE|kin);
		    code.emitInvoke(typeCallContext.getDeclaredMethod("matchError", 1));
		    code.emitLoad(ctxVar);
		    code.emitReturn();
		    //code.emitElse();
		    code.emitFi();
		}
                ptype.emitCoerceFromObject(code);
                code.emitStore(param.var);
	    }
	if (recurseNeeded)
	    generateCheckArg(param.nextDecl(), lexp, knext, code, keyDecls, argVariables, suppliedParameterVar);
    }

    private void generateCheckCall(LambdaExp lexp, CodeAttr code, Method primMethod, ArrayList<Variable> argVariables) {
	boolean usingCallContext = lexp.usingCallContext();
	Variable ctxVar = callContextVar;
        for (Variable var : argVariables)
            code.emitLoad(var);
        for (Declaration param = lexp.firstDecl();
	     param != null; param = param.nextDecl()) {
            param.var = null;
        }
        if (primMethod == lexp.getMainMethod())
            code.popScope();
	if (usingCallContext)
            code.emitLoad(ctxVar); // get $ctx
	code.emitInvoke(primMethod);

	if (usingCallContext) {
            code.emitPushNull();
        } else {
            Target.pushObject.compileFromStack(this,
                                               lexp.getReturnType());
        }
	code.emitReturn();
    }

    public void generateCheckMethods(LambdaExp parent) {
        int numApplyMethods
            = parent.applyMethods == null ? 0 : parent.applyMethods.size();
        if (numApplyMethods == 0)
            return;
        for (int j = 0;  j < numApplyMethods;  ++j) {
            LambdaExp source = parent.applyMethods.get(j);
            generateCheckMethod(source, parent);
        }
    }

  /** Copy incoming arguments to varargs/#!rest array.
   */
  private void varArgsToArray (LambdaExp source, int singleArgs,
                               Variable counter, Type lastArgType,
                               Variable ctxVar, boolean keywordsOk)
  {
    CodeAttr code = getCode();
    Type elType = ((ArrayType) lastArgType).getComponentType();
    boolean mustConvert = ! "java.lang.Object".equals(elType.getName());
    if (! mustConvert)
      {
        code.emitLoad(ctxVar);
        //code.emitPushInt(singleArgs);
        String mname = keywordsOk ? "getRestArgsArray" : "getRestPlainArray";
        code.emitInvokeVirtual(typeCallContext.getDeclaredMethod(mname, 0));
      }
    else
      {
        code.pushScope();
        if (counter == null)
          {
            counter = code.addLocal(Type.intType);
            code.emitLoad(ctxVar);
            code.emitInvoke(typeCallContext.getDeclaredMethod("getArgCount", 0));
            if (singleArgs != 0)
              {
                code.emitPushInt(singleArgs);
                code.emitSub(Type.intType);
              }
            code.emitStore(counter);
          }
        code.emitLoad(counter);
        code.emitNewArray(elType.getImplementationType());
        Label testLabel = new Label(code);
        Label loopTopLabel = new Label(code);
        loopTopLabel.setTypes(code);
        code.emitGoto(testLabel);
        loopTopLabel.define(code);

        code.emitDup(1); // new array
        code.emitLoad(counter);
        code.emitLoad(ctxVar);
        code.emitInvokeVirtual(typeCallContext.getDeclaredMethod("getNextArg", 0));
        if (mustConvert)
          {
            CheckedTarget.emitCheckedCoerce
              (this, source, source.getName(),
               0, elType, null);
          }
        code.emitArrayStore(elType);
        testLabel.define(code);
        code.emitInc(counter, (short) (-1));
        code.emitLoad(counter);
        code.emitGotoIfIntGeZero(loopTopLabel);
        code.popScope();	
      }
  }

  private Method startClassInit ()
  {
    method = curClass.addMethod ("<clinit>", apply0args, Type.voidType,
				 Access.PUBLIC|Access.STATIC);

    CodeAttr code = method.startCode();

    if (generateMainMethod() || generatingApplet() || generatingServlet())
      {
	ClassType languageType
	  = (ClassType) Type.make(getLanguage().getClass());
	Method registerMethod
	  = languageType.getDeclaredMethod("registerEnvironment", 0);
	if (registerMethod != null)
	  code.emitInvokeStatic(registerMethod);
      }
    return method;
  }

  /** Parse/visit/compile this module as needed and requested.
   * This method does not process any dependent modules (expect indirectly,
   * such as may be done by a require form).
   * @param wantedState the desired value of getState().
   */
  public void process (int wantedState)
  {
    Compilation saveCompilation = Compilation.setSaveCurrent(this);
    try
      {
        ModuleExp mexp = getModule();
        if (wantedState >= BODY_PARSED && getState() < BODY_PARSED-1)
          {
            setState(BODY_PARSED-1);
            language.parse(this, 0);
            mexp.classFor(this);
            if (lexer != null)
                lexer.close();
            lexer = null;
            setState(messages.seenErrors() ? ERROR_SEEN : BODY_PARSED);
            if (pendingImports != null)
              return;
          }
        if (wantedState >= RESOLVED && getState() < RESOLVED)
          {
            language.resolve(this);
            // Doing addMainClass is a bit flakey in the case that
            // ModuleExp.alwaysCompile is false.  We don't want to
            // call addMainClass *unless* we're compiling, but when
            // dealing with eval, mutually recursive modules, etc
            // it doesn't quite work.
            addMainClass(mexp);
            if (generateMainMethod() && mexp.staticInitRun()) {
                error('e', "a static init-run module cannot have a 'main' method");
            }
            setState(RESOLVED);
          }

        // Avoid writing class needlessly.
        if (! explicit && ! immediate
            && getMinfo().checkCurrent(ModuleManager.getInstance(), System.currentTimeMillis()))
          {
            getMinfo().cleanupAfterCompilation();
            setState(CLASS_WRITTEN);
          }

        if (wantedState >= PRE_WALKED && getState() < PRE_WALKED)
          {
            if (debugPrintExpr) {
                OutPort dout = OutPort.errDefault();
                dout.println("[Module:" + mexp.getName());
                mexp.print(dout);
                dout.println(']');
                dout.flush();
            }
            PushApply.pushApply(mexp, this);  
            setState(PRE_WALKED);
          }

        if (wantedState >= WALKED && getState() < WALKED)
          {
            InlineCalls.inlineCalls(mexp, this);
            if (enableANF)
                ANormalize.aNormalize(mexp, this);
            if (debugPrintANF) {
                options.set("warn-void-used", Boolean.FALSE);
                OutPort dout = OutPort.errDefault();
                dout.println ("[Normalized module: "+mexp.getName()
                             + " to " + mainClass.getName() + ":");
                mexp.print(dout);
                dout.println(']');
                dout.flush();
            }
            ChainLambdas.chainLambdas(mexp, this);
            FindTailCalls.findTailCalls(mexp, this);
            setState(WALKED);
          }

        if (wantedState >= COMPILE_SETUP && getState() < COMPILE_SETUP)
          {
            litTable = new LitTable(this);
            mexp.setCanRead(true);
            FindCapturedVars.findCapturedVars(mexp, this);
            mexp.allocFields(this);
            mexp.allocChildMethods(this);
            setState(COMPILE_SETUP);
          }
        if (wantedState >= COMPILED && messages.seenErrors())
            setState(ERROR_SEEN);
        if (wantedState >= COMPILED && getState() < COMPILED)
          {
            if (mexp.subModulesOnly())
              {
                setState(wantedState < CLASS_WRITTEN ? COMPILED : CLASS_WRITTEN);
              }
            else
              {
                if (immediate)
                  {
                    ClassLoader parentLoader = ObjectType.getContextClassLoader();
                    loader = new ArrayClassLoader(parentLoader);
                  }
                generateBytecode();
                setState(messages.seenErrors() ? ERROR_SEEN : COMPILED);
              }
          }
        if (wantedState >= CLASS_WRITTEN && getState() < CLASS_WRITTEN
            && ! mexp.subModulesOnly())
          {
            outputClass(ModuleManager.getInstance().getCompilationDirectory());
            setState(CLASS_WRITTEN);
          }
      }
    catch (gnu.text.SyntaxException ex)
      {
        setState(ERROR_SEEN);
        if (ex.getMessages() != getMessages())
          throw new RuntimeException ("confussing syntax error: "+ex);
        // otherwise ignore it - it's already been recorded in messages.
      }
    catch (java.io.IOException ex)
      {
        ex.printStackTrace();
        error('f', "caught "+ex);
        setState(ERROR_SEEN);
      }
    finally
      {
        Compilation.restoreCurrent(saveCompilation);
      }
  }

  /** The guts of compiling a module to one or more classes.
   * Assumes walkModule has been done.
   */
  void generateBytecode ()
  {
    ModuleExp module = getModule();
    if (debugPrintFinalExpr)
      {
	OutPort dout = OutPort.errDefault();
	dout.println ("[Compiling final "+module.getName()
                     + " to " + mainClass.getName() + ":");
	module.print(dout);
	dout.println(']');
	dout.flush();
      }

    ClassType neededSuper = getModuleType();
    moduleClass = mainClass;

    curClass = module.compiledType;
    LambdaExp saveLambda = curLambda;
    curLambda = module;

    CodeAttr code;
    Variable heapFrame = module.heapFrame;
    boolean staticModule = module.isStatic();

    if (curClass.getSuperclass() != typeModuleBody) {
        Field runDoneField = curClass.addField("$runDone$", Type.booleanType,
                             Access.PROTECTED);
        Method runDoneMethod =
            curClass.addMethod ("checkRunDone", new Type[] { Type.booleanType },
                                Type.booleanType, Access.PUBLIC);
        method = runDoneMethod;
        code = method.startCode();
        code.emitLoad(code.getArg(0));
        code.emitGetField(runDoneField);
        code.emitLoad(code.getArg(0));
        code.emitLoad(code.getArg(1));
        code.emitPutField(runDoneField);
        code.emitReturn();
    }
    
    Method apply_method;
    if (module.staticInitRun()) {
        apply_method = curClass.addMethod("$runBody$", Type.typeArray0, Type.voidType,
				       Access.PUBLIC+Access.STATIC);
    } else {
        Type[] arg_types = { typeCallContext };
        apply_method = curClass.addMethod ("run", arg_types, Type.voidType,
                                           Access.PUBLIC+Access.FINAL);
    }
    method = apply_method;
    // For each parameter, assign it to its proper slot.
    // If a parameter !isSimple(), we cannot assign it to a local slot,
    // so instead create an artificial Variable for the incoming argument.
    // Below, we assign the value to the slot.
    method.initCode();
    code = getCode();
    // if (usingCPStyle())   code.addParamLocals();

    thisDecl = method.getStaticFlag() ? null : module.declareThis(module.compiledType);
    module.closureEnv = module.thisVariable;
    module.heapFrame = module.isStatic() ? null : module.thisVariable;
    module.allocChildClasses(this);

    if (module.isHandlingTailCalls() || usingCPStyle())
      {
	callContextVar = new Variable ("$ctx", typeCallContext);
	module.getVarScope().addVariableAfter(thisDecl, callContextVar);
	callContextVar.setParameter(true);
      }

    int line = module.getLineNumber();
    if (line > 0)
      code.putLineNumber(module.getFileName(), line);

    module.allocParameters(this);
    module.enterFunction(this);
    if (usingCPStyle())
      {
	loadCallContext();
        code.emitGetField(pcCallContextField);
        fswitch = code.startSwitch();
	fswitch.addCase(0, code);
      }

    module.compileBody(this);
    module.compileEnd(this);

    Label startLiterals = null;
    Label afterLiterals = null;
    Method initMethod = null;

      {
	Method save_method = method;
        Variable callContextSave = callContextVar;
        callContextVar = null;

	initMethod = startClassInit();
        clinitMethod = initMethod;
	code = getCode();

        startLiterals = new Label(code);
        afterLiterals = new Label(code);
        code.fixupChain(afterLiterals, startLiterals);

	if (staticModule && ! module.staticInitRun())
	  {
            if (! module.getFlag(ModuleExp.USE_DEFINED_CLASS))
              generateConstructor(module);
            else if (moduleClass.constructor == null)
              moduleClass.constructor = getConstructor(module);
              
	    code.emitNew(moduleClass);
	    code.emitDup(moduleClass);
	    code.emitInvokeSpecial(moduleClass.constructor);
            // The $instance field needs to be public so
            // ModuleContext.findInstance can find it.
            // It needs to be non-final in case moduleClass!=mainClass.
            // (The latter should probably be fixed by moving this code
            // to moduleClass's <clinit>.)
	    moduleInstanceMainField
	      = moduleClass.addField("$instance", moduleClass,
				     Access.STATIC|Access.PUBLIC);
	    code.emitPutStatic(moduleInstanceMainField);
	  }
        Initializer init;
        while ((init = clinitChain) != null)
          {
            clinitChain = null;
            dumpInitializers(init);
          }

	if (module.staticInitRun())
            code.emitInvoke(apply_method);

	code.emitReturn();

        method = save_method;
        callContextVar = callContextSave;
      }

    module.generateApplyMethods(this);

    curLambda = saveLambda;

    module.heapFrame = heapFrame;  // Restore heapFrame.
    if (usingCPStyle())
      {
	code = getCode();
	fswitch.finish(code);
      }

    if (startLiterals != null || callContextVar != null)
      {
	method = initMethod;
	code = getCode();

	Label endLiterals = new Label(code);
	code.fixupChain(startLiterals, endLiterals);

        if (callContextVarForInit != null)
          {
            code.emitInvokeStatic(getCallContextInstanceMethod);
            code.emitStore(callContextVarForInit);
          }

	try
	  {
            if (immediate)
              {
                code.emitPushInt(registerForImmediateLiterals(this));
                code.emitInvokeStatic(ClassType.make("gnu.expr.Compilation")
                                      .getDeclaredMethod("setupLiterals", 1));
              }
            else
              litTable.emit();
	  }
	catch (Exception ex)
	  {
	    error('e', "Literals: Internal error:" + ex);
	  }
	code.fixupChain(endLiterals, afterLiterals);
      }

    if (generateMainMethod() && curClass == mainClass)
      {
	Type[] args = { new ArrayType(javaStringType) };
	method = curClass.addMethod("main", Access.PUBLIC|Access.STATIC,
				    args, Type.voidType);
				    
	code = method.startCode();

	if (Shell.defaultFormatName != null)
	  {
	    code.emitPushString(Shell.defaultFormatName);
	    code.emitInvokeStatic(ClassType.make("kawa.Shell")
				  .getDeclaredMethod("setDefaultFormat", 1));
	  }
	code.emitLoad(code.getArg(0));
	code.emitInvokeStatic(ClassType.make("gnu.expr.ApplicationMainSupport")
                              .getDeclaredMethod("processArgs", 1));
	if (moduleInstanceMainField != null)
	  code.emitGetStatic(moduleInstanceMainField);
	else
	  {
	    code.emitNew(curClass);
	    code.emitDup(curClass);
	    code.emitInvokeSpecial(curClass.constructor);
	  }
        Method runAsMainMethod = null;
        ClassType superClass = curClass.getSuperclass();
        if (superClass != typeModuleBody)
           runAsMainMethod = superClass.getDeclaredMethod("runAsMain", 0);
        if (runAsMainMethod == null)
            runAsMainMethod = typeModuleBody.getDeclaredMethod("runAsMain", 1);
        code.emitInvoke(runAsMainMethod);
	code.emitReturn();
      }

    String uri;
    if (getMinfo() != null && (uri = getMinfo().getNamespaceUri()) != null)
      {
        // Need to generate a ModuleSet for this class, so XQuery can find
        // this module and other modules in the same namespace.
        ModuleManager manager = ModuleManager.getInstance();
        String mainPrefix = mainClass.getName();
        int dot = mainPrefix.lastIndexOf('.');
        if (dot < 0)
          {
            mainPrefix = "";
          }
        else
          {
            String mainPackage = mainPrefix.substring(0, dot);
            try
              {
                manager.loadPackageInfo(mainPackage);
              }
            catch (ClassNotFoundException ex)
              {
                // Do nothing.
              }
            catch (Exception ex)
              {
                error('e', "error loading map for "+mainPackage+" - "+ex);
              }
            mainPrefix = mainPrefix.substring(0, dot+1);
          }
        ClassType mapClass = new ClassType(mainPrefix + ModuleSet.MODULES_MAP);
        ClassType typeModuleSet = ClassType.make("gnu.expr.ModuleSet");
        mapClass.setSuper(typeModuleSet);
        registerClass(mapClass);

        method = mapClass.addMethod("<init>", Access.PUBLIC,
                                apply0args, Type.voidType);
        Method superConstructor
          = typeModuleSet.addMethod("<init>", Access.PUBLIC,
                                    apply0args, Type.voidType);
        code = method.startCode();
        code.emitPushThis();
        code.emitInvokeSpecial(superConstructor);
        code.emitReturn();

        ClassType typeModuleManager = ClassType.make("gnu.expr.ModuleManager");
        Type[] margs = { typeModuleManager };
        method = mapClass.addMethod("register", margs, Type.voidType,
                                    Access.PUBLIC);
        code = method.startCode();
        Method reg = typeModuleManager.getDeclaredMethod("register", 3);

        for (int i = manager.numModules;  --i >= 0; )
          {
            ModuleInfo mi = manager.modules[i];
            String miClassName = mi.getClassName();
            if (miClassName == null
                || ! miClassName.startsWith(mainPrefix))
              continue;
            String moduleSource = mi.sourcePath;
            String moduleUri = mi.getNamespaceUri();
            code.emitLoad(code.getArg(1));
            compileConstant(miClassName);
            if (! Path.valueOf(moduleSource).isAbsolute())
              try
                {
                  // If the source path was relative, emit it as relative.
                  // But make it relative to the compilation directory,
                  // to allow sources to be moved along with binaries.
                  String path = Path.toURL(manager.getCompilationDirectory())
                      + mainPrefix.replace('.', '/');
                  int plen = path.length();
                  if (plen > 0 && path.charAt(plen-1) != '/')
                    path = path + '/';
                  String sourcePath =
                      Path.toURL(mi.getSourceAbsPathname()).toString();
                  moduleSource = Path.relativize(sourcePath, path);
                }
              catch (Exception ex)
                {
                  throw new WrappedException("exception while fixing up '"
                                             +moduleSource+'\'',
                                             ex);
                }
            compileConstant(moduleSource);
            compileConstant(moduleUri);
            code.emitInvokeVirtual(reg);
          }
        code.emitReturn();
      }
  }

  int localFieldIndex; 
  public Field allocLocalField (Type type, String name)
  {
    if (name == null)
      name = "tmp_"+(++localFieldIndex);
    Field field = curClass.addField(name, type, 0);
    return field;
  }

  /** If non-null, contains the value of the current CallContext. */
  Variable callContextVar;
  Variable callContextVarForInit;

  /** Generate code to push the current CallContext on the JVM stack. */
  public final void loadCallContext()
  {
    CodeAttr code = getCode();
    if (callContextVar != null && ! callContextVar.dead())
      code.emitLoad(callContextVar);
    // We're cautious about re-using a previously extracted CallContext,
    // because it's tricky to manage the variables safely.
    // A possible solution is to inject a Variable into the current scope,
    // and making sure each separate straight-line block has its own scope.
    // (If the current scope is in the same "basic block" as an outer scope,
    // we can use that instead.)  FIXME
    else if (method == clinitMethod)
      {
        // The variable is initialized just after literals.
        callContextVar = new Variable("$ctx", typeCallContext);
        // To make sure it doesn't clash with variables that have already
        // allocated and freed for previous initialzier.
        callContextVar.reserveLocal(code.getMaxLocals(), code);
        code.emitLoad(callContextVar);
        callContextVarForInit = callContextVar;
      }
    else
      {
        code.emitInvokeStatic(getCallContextInstanceMethod);
        code.emitDup();
        callContextVar = new Variable("$ctx", typeCallContext);
        code.getCurrentScope().addVariable(code, callContextVar);
        code.emitStore(callContextVar);
      }
  }

  public void freeLocalField (Field field)
  {
    // FIXME
  }

  /** This may not make sense, except for Lisp-like languages.
   * For those, 'input' an s-expression  from the reader. */
  public Expression parse (Object input)
  {
    throw new Error("unimeplemented parse");
  }

  protected Language language;
  public Language getLanguage() { return language; }

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  public final ModuleExp getModule() { return mainLambda; }
  public void setModule(ModuleExp mexp) { mainLambda = mexp; }

  public boolean isStatic() { return mainLambda.isStatic(); }

    public boolean isInteractive() {
        return mainLambda != null && mainLambda.getFlag(ModuleExp.INTERACTIVE);
    }

  /** The same as getModule, until we allow nested modules. */
  public ModuleExp currentModule() { return current_scope.currentModule(); }

  /** Note that we have seen a construct that must be compiled, not evaluated.
   * If we are not inside a lambda (which is always compiled), but
   * only inside the outer-most ModuleExp, note that it must be compiled.
   */
  public void mustCompileHere ()
  {
    if (! mustCompile && ! ModuleExp.compilerAvailable())
      error('e', "this expression must be compiled, but compiler is unavailable");
    mustCompile = true;
  }

  public ScopeExp currentScope() { return current_scope; }

  /** Set <code>currentScope()</code>.
   * Also update the <code>nesting</code> object.
   */
  public void setCurrentScope (ScopeExp scope)
  {
    int scope_nesting = ScopeExp.nesting(scope);
    int current_nesting = ScopeExp.nesting(current_scope);
    while (current_nesting > scope_nesting)
      {
	pop(current_scope);
	current_nesting--;
      }
    ScopeExp sc = scope;
    while (scope_nesting > current_nesting)
      {
	sc = sc.getOuter();
	scope_nesting--;
      }
    while (sc != current_scope)
      {
	pop(current_scope);
        sc = sc.getOuter();
      }
    pushChain(scope, sc);
  }

    public ScopeExp setPushCurrentScope (ScopeExp scope) {
        ScopeExp old = currentScope();
        lexical.pushSaveTopLevelRedefs();
        setCurrentScope(scope);
        return old;
    }

    public void setPopCurrentScope (ScopeExp old) {
        setCurrentScope(old);
        lexical.popSaveTopLevelRedefs();
    }

  void pushChain (ScopeExp scope, ScopeExp limit)
  {
    if (scope != limit)
      {
        pushChain(scope.getOuter(), limit);
        pushScope(scope);
        lexical.push(scope);
      }
  }

  public ModuleExp pushNewModule (Lexer lexer)
  {
    this.lexer = lexer;
    String filename = lexer == null ? null : lexer.getName();
    ModuleExp module = new ModuleExp();
    if (filename != null)
      module.setFile(filename);
    if (generatingApplet() || generatingServlet())
      module.setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
    mainLambda = module;
    if (immediate)
      {
        module.setFlag(ModuleExp.IMMEDIATE);
        ModuleInfo minfo = new ModuleInfo();
        minfo.setCompilation(this);
      }
    push(module);
    return module;
  }

  public void push (ScopeExp scope)
  {
    pushScope(scope);
    lexical.push(scope);
  }

  public final void pushScope (ScopeExp scope)
  {
    if (! mustCompile
        && (scope.mustCompile()
            || (ModuleExp.compilerAvailable()
                // We set mustCompile if we see a LambdaExp - not because
                // we must but because it is usually desirable.
                && scope instanceof LambdaExp
                && ! (scope instanceof ModuleExp))))
      mustCompileHere();
    scope.setOuter(current_scope);
    current_scope = scope;
  }

  public void pop (ScopeExp scope)
  {
    lexical.pop(scope);
    current_scope = scope.getOuter();
  }

  public final void pop ()
  {
    pop(current_scope);
  }

  public void push (Declaration decl)
  {
    lexical.push(decl);
  }

  public Declaration lookup(Object name, int namespace)
  {
    return lexical.lookup(name, namespace);
  }

  /** Called for classes referenced in bytecode.
   * Since this only does something when immediate, we only care about
   * classes referenced in the bytecode when immediate.
   * It is used to ensure that we can inherit from classes defined when in
   * immediate mode (in Scheme using define-class or similar).
   */
    public void usedClass(Type type) {
        while (type instanceof ArrayType)
            type = ((ArrayType) type).getComponentType();
        if (immediate && type instanceof ClassType) {
            ClassType cl = (ClassType) type;
            for (;;) {
                loader.addClass(cl);
                ClassType enc = cl.getDeclaringClass();
                if (enc == null)
                    break;
                cl = enc;
            }
        }
    }

    /** Set module name - which sets name of generated class. */
    public void setModuleName(String name) {
        getModule().setName(name);
    }

    /** Generate and set unique module name suitable for an interactive session. */
    public void setInteractiveName() {
        setModuleName(ModuleManager.getInstance().getNewInteractiveName());
    }

    /** Generate and set unique module name suitable for a call to eval. */
    public void setEvalName() {
        setModuleName(ModuleManager.getInstance().getNewEvalName());
    }

  public SourceMessages getMessages() { return messages; }
  public void setMessages (SourceMessages messages)
  { this.messages = messages; }
 
  public void error(char severity, String message, SourceLocator location)
  {
    String file = location.getFileName();
    int line = location.getLineNumber();
    int column = location.getColumnNumber();
    if (file == null || line <= 0)
      {
        file = getFileName();
        line = getLineNumber();
        column = getColumnNumber();
      }

    if (severity == 'w' && warnAsError())
      severity = 'e';
    messages.error(severity, file, line, column, message);
  }

  public void error(char severity, String message)
  {
    if (severity == 'w' && warnAsError())
      severity = 'e';
    
    messages.error(severity, this, message);
  }

  public void error(char severity, Declaration decl, String msg1, String msg2)
  {
    error(severity, msg1 + decl.getName() + msg2, null, decl);
  }

  public void error(char severity, String message,
                    String code, SourceLocator decl)
  {
    if (severity == 'w' && warnAsError())
      severity = 'e';
    
    String filename = getFileName();
    int line = getLineNumber();
    int column = getColumnNumber();
    int decl_line = decl.getLineNumber();
    if (decl_line > 0)
      {
	filename = decl.getFileName();
	line = decl_line;
	column = decl.getColumnNumber();
      }
    messages.error(severity, filename, line, column, message, code);
  }

  /**
   * Handle syntax errors (at rewrite time).
   * @param message an error message to print out
   * @return an ErrorExp
   */
  public ErrorExp syntaxError (String message)
  {
    error('e', message);
    return new ErrorExp (message);
  }

  public final int getLineNumber()  { return messages.getLineNumber(); }
  public final int getColumnNumber() { return messages.getColumnNumber(); }
  public final String getFileName() { return messages.getFileName(); }
  public String getPublicId() { return messages.getPublicId(); }
  public String getSystemId() { return messages.getSystemId(); }
  public boolean isStableSourceLocation() { return false; }

  public void setFile(String filename) { messages.setFile(filename); }
  public void setLine(int line) { messages.setLine(line); }
  public void setColumn(int column) { messages.setColumn(column); }
  public final void setLine(Expression position)
  { messages.setLocation(position); }
  public void setLine (Object location)
  {
    if (location instanceof SourceLocator)
      messages.setLocation((SourceLocator) location);
  }
  public final void setLocation (SourceLocator position)
  { messages.setLocation(position); }

  public void setLine(String filename, int line, int column)
  {
    messages.setLine(filename, line, column);
  }

    /** Get source filename as an absolute Path, or null.
     * Return null if there is no actual file, such as a {@code <string>}.
     * Note the {@link ModuleInfo#getSourceAbsPath} is similar,
     * but this version is not canonicalized.
     */
    public Path getSourceAbsPath() {
        String currentFileName = getFileName();
        if (currentFileName != null) {
            ModuleInfo info = getMinfo();
            // info.getSourceAbsPath() is null if the source port is
            // a string or a console, in which we should return null.
            if (info != null && info.getSourceAbsPath() != null) {
                return Path.valueOf(currentFileName).getAbsolute();
            }
        }
        return null;
    }

  /** A help vector for building expressions. */
  public Stack<Expression> exprStack;

  public void letStart ()
  {
    pushScope(new LetExp());
  }

  public Declaration letVariable (Object name, Type type, Expression init)
  {
    Declaration decl = new Declaration(name, type);
    letVariable(decl, init);
    return decl;
  }

  public void letVariable (Declaration decl, Expression init)
  {
    LetExp let = (LetExp) current_scope;
    let.add(decl);
    decl.setInitValue(init);
  }

  public void letEnter ()
  {
    LetExp let = (LetExp) current_scope;
    // Set a flag which letDone uses to check if letEnter has been called.
    let.setFlag(Expression.VALIDATED);
    for (Declaration decl = let.firstDecl();
	 decl != null;  decl = decl.nextDecl())
      {
        Expression init = decl.getInitValue();
        if (init != QuoteExp.undefined_exp)
          decl.noteValueFromLet(let);
      }
    lexical.push(let);
  }

  public LetExp letDone (Expression body)
  {
    LetExp let = (LetExp) current_scope;
    // Check if letEnter has been called.
    if (! let.getFlag(Expression.VALIDATED))
      letEnter();
    let.setFlag(false, Expression.VALIDATED);
    let.body = body;
    pop(let);
    return let;
  }

    private void checkLoop() {
        if (((LambdaExp) current_scope).getName() != "%do%loop")
            throw new Error("internal error - bad loop state");
    }

    /** Start a new loop.
     * This provides the functionality of Scheme 'named let'. 
     */
    public LambdaExp loopStart() {
        if (exprStack == null)
            exprStack = new Stack<Expression>();
        LambdaExp loopLambda = new LambdaExp();
        LetExp let = new LetExp();
        String fname = "%do%loop";
        Declaration fdecl = let.addDeclaration(fname);
        fdecl.setInitValue(loopLambda);
        fdecl.noteValueFromLet(let);
        loopLambda.setName(fname);
        let.setOuter(current_scope);
        loopLambda.setOuter(let);
        current_scope = loopLambda;
        return loopLambda;
    }

    /** Add a new loop variable, with initializer. */
    public Declaration loopVariable(Object name, Type type, Expression init) {
        checkLoop();
        LambdaExp loopLambda = (LambdaExp) current_scope;
        Declaration decl = loopLambda.addDeclaration(name, type);
        exprStack.push(init);
        loopLambda.min_args++;
        return decl;
    }

    /** Done handling loop variables, and pushes them into the lexical scope.
     * Ready to parse the loop condition.
     */ 
    public void loopEnter() {
        checkLoop();
        LambdaExp loopLambda = (LambdaExp) current_scope;
        int ninits = loopLambda.min_args;
        loopLambda.max_args = ninits;
        Expression[] inits = new Expression[ninits];
        for (int i = ninits;  --i >= 0; )
            inits[i] = (Expression) exprStack.pop();
        LetExp let = (LetExp) loopLambda.getOuter();
        Declaration fdecl = let.firstDecl();  // The decls for loopLambda.
        let.setBody(new ApplyExp(new ReferenceExp(fdecl), inits));
        lexical.push(loopLambda);
    }

    @Deprecated
    public void loopCond(Expression cond) {
        checkLoop();
        exprStack.push(cond);
    }

    @Deprecated
    public void loopBody(Expression body) {
        LambdaExp loopLambda = (LambdaExp) current_scope;
        loopLambda.body = body;
    }

    /** Recurse to next iteration of specified loop. */
    public Expression loopRepeat(LambdaExp loop, Expression... exps) {
        ScopeExp let = loop.getOuter();
        Declaration fdecl = let.firstDecl();  // The decls for loopLambda.
        return new ApplyExp(new ReferenceExp(fdecl), exps);
    }

    /** Finish building a loop and return resulting expression. */
    public Expression loopDone(Expression body) {
        LambdaExp loopLambda = (LambdaExp) current_scope;
        ScopeExp let = loopLambda.getOuter();
        loopLambda.body = body;
        lexical.pop(loopLambda);
        current_scope = let.getOuter();
        return let;
    }

    /** Combine loopRepeat and loopDone.
     * Assume loopCond and loopBody have been called.
     */
    public Expression loopRepeatDone(Expression... exps) {
        LambdaExp loopLambda = (LambdaExp) current_scope;
        ScopeExp let = loopLambda.getOuter();
        Expression cond = (Expression) exprStack.pop();
        Expression recurse = loopRepeat(loopLambda, exps);
        loopLambda.body = new IfExp(cond,
                                    new BeginExp(loopLambda.body, recurse),
                                    QuoteExp.voidExp);
        lexical.pop(loopLambda);
        current_scope = let.getOuter();
        return let;
    }

    /** A language-dependent "apply" function for generic application.
     */
    public Expression applyFunction(Expression func) {
        return null;
    }

  public QuoteExp makeQuoteExp (Object value)
  {
    return QuoteExp.getInstance(value, this);
  }

  /**
   * Convenience method to make an Expression that coerces a value.
   * @param value to be coerced
   * @param type to coerce value to
   * @return expression that coerces value to type
   */
  public static ApplyExp makeCoercion(Expression value, Expression type)
  {
    Expression[] exps = new Expression[2];
    exps[0] = type;
    exps[1] = value;
    QuoteExp c = new QuoteExp(Convert.cast);
    return new ApplyExp(c, exps);
  }

    /**
     * Convenience method to make an Expression that coerces a value.
     * @param value to be coerced
     * @param type to coerce value to
     * @return expression that coerces value to type
     */
    public static ApplyExp makeCoercion(Expression value, Type type) {
        return makeCoercion(value, new QuoteExp(type));
    }

  /** If non-null, a helper method generated by getForNameHelper. */
  Method forNameHelper;

  /** Generate code to load a named Class without initializing it.
   */
  public void loadClassRef (ObjectType clas)
  {
    CodeAttr code = getCode();
    // Try an optimization
    if (curClass.getClassfileVersion() >= ClassType.JDK_1_5_VERSION)
      code.emitPushClass(clas);
    else if (clas == mainClass && mainLambda.isStatic()
        // moduleInstanceMainField may not have been set yet.
        && moduleInstanceMainField != null)
      {
        code.emitGetStatic(moduleInstanceMainField);
        code.emitInvokeVirtual(Type.objectType.getDeclaredMethod("getClass", 0));
      }
    else
      {
        String name = clas instanceof ClassType ? clas.getName()
          : clas.getInternalName().replace('/', '.');
        code.emitPushString(name);
        code.emitInvokeStatic(getForNameHelper());
      }
  }

  /** Generate a method to find a named Class without initializing it.
   * Generate a static helper method "class$" like javac generates for
   * 'CLASS.class', but does not initialize CLASS.  Also, we don't bother
   * catching exceptions, since the JVM doesn't require us to.  I.e. generates:
   * public static class $(String name)
   * { return Class.forName(name, false,
   *                        Class.forName(THISCLASSNAME).getClassLoader()); }
   * Note that we want the result to use the same ClassLoader as the caller,
   * which is why we generate a static helper method.
   */
  public Method getForNameHelper ()
  {
    if (forNameHelper == null)
      {
	/* #ifdef JAVA2 */
	Method save_method = method;
	method = curClass.addMethod("class$", Access.PUBLIC|Access.STATIC,
				    string1Arg, typeClass);
	forNameHelper = method;
	CodeAttr code = method.startCode();
	code.emitLoad(code.getArg(0));
	code.emitPushInt(0);
	code.emitPushString(mainClass.getName());
	code.emitInvokeStatic(typeClass.getDeclaredMethod("forName", 1));
	code.emitInvokeVirtual(typeClass.getDeclaredMethod("getClassLoader", 0));
	code.emitInvokeStatic(typeClass.getDeclaredMethod("forName", 3));
	code.emitReturn();
	method = save_method;
	/* #else */
	// forNameHelper = typeClass.getDeclaredMethod("forName", 1);
	/* #endif */
      }
    return forNameHelper;
  }

    public Environment getGlobalEnvironment() { return Environment.getCurrent(); }

  public Object resolve(Object name, boolean function)
  {
    Environment env = getGlobalEnvironment();
    Symbol symbol;
    if (name instanceof String)
      symbol = env.defaultNamespace().lookup((String) name);
    else
      symbol = (Symbol) name;
    if (symbol == null)
      return null;
    if (function && getLanguage().hasSeparateFunctionNamespace())
      return env.getFunction(symbol, null);
    return env.get(symbol, null);
  }

  /** A key we can pass from the compiler to identity a Compilation. */
  private int keyUninitialized;
  /** Chain of immediate Compilation whose setupLiterals hasn't been called. */
  private static Compilation chainUninitialized;
  /** Next in chain headed by chainUninitialized. */
  private Compilation nextUninitialized;

  /** Call-back from compiled code to initialize literals in immediate mode.
   * In non-immediate mode (i.e. generating class files) the compiler emits
   * code to "re-construct" literal values.  However, in immediate mode
   * that would be wasteful, plus we would get values that are similar (equals)
   * to but not necessarily identical (eq) to the compile-time literal.
   * So we need to pass the literal values to the compiled code, by using
   * reflection to initialize various static fields.  This method does that.
   * It is called at the start of the generated static initializer, which
   * helps makes things more consistent between immediate and non-immediate
   * mode.
   */
  public static void setupLiterals (int key)
  {
    Compilation comp = Compilation.findForImmediateLiterals(key);
    try
      {
        Class clas = comp.loader.loadClass(comp.mainClass.getName());

	/* Pass literal values to the compiled code. */
	for (Literal init = comp.litTable.literalsChain;  init != null;
	     init = init.next)
	  {
	    /* DEBUGGING:
	    OutPort out = OutPort.errDefault();
	    out.print("init["+init.index+"]=");
	    out.print(init.value);
	    out.println();
	    */
            clas.getDeclaredField(init.field.getName())
              .set(null, init.value);
	  }
        comp.litTable = null;
      }
    catch (Throwable ex)
      {
        WrappedException.rethrow(ex);
      }
  }

  public static synchronized int
  registerForImmediateLiterals (Compilation comp)
  {
    int i = 0;
    for (Compilation c = chainUninitialized;  c != null;  c = c.nextUninitialized)
      {
        if (i <= c.keyUninitialized)
          i = c.keyUninitialized + 1;
      }
    comp.keyUninitialized = i;
    comp.nextUninitialized = chainUninitialized;
    chainUninitialized = comp;
    return i;
  }

  public static synchronized Compilation findForImmediateLiterals (int key)
  {
    Compilation prev = null;
    for (Compilation comp = chainUninitialized; ; )
      {
        Compilation next = comp.nextUninitialized;
        if (comp.keyUninitialized == key)
          {
            if (prev == null)
              chainUninitialized = next;
            else
              prev.nextUninitialized = next;
            comp.nextUninitialized = null;
            return comp;
          }
        prev = comp;
        comp = next;
      }
  }

  /** Current lexical scope - map name to Declaration. */
  public NameLookup lexical;

  protected ScopeExp current_scope;

  protected SourceMessages messages;

  private static final ThreadLocal<Compilation> current =
    new InheritableThreadLocal<Compilation>();

  public static Compilation getCurrent ()
  {
    return current.get();
  }

  public static void setCurrent (Compilation comp)
  {
    current.set(comp);
  }

  public static Compilation setSaveCurrent (Compilation comp)
  {
    Compilation save = current.get();
    current.set(comp);
    return save;
  }

  public static void restoreCurrent (Compilation saved)
  {
    current.set(saved);
  }

  public String toString ()
  {
    return "<compilation "+mainLambda+">";
  }

    public ModuleInfo getMinfo() {
        return mainLambda.info;
    }
}
