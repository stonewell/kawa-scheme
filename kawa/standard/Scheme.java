package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.*;
import gnu.text.SourceMessages;
import gnu.kawa.lispexpr.*;
import gnu.kawa.format.AbstractFormat;
import gnu.kawa.functions.*;
import gnu.kawa.io.CharArrayInPort;
import gnu.kawa.io.InPort;
import gnu.kawa.reflect.LazyType;
import gnu.kawa.reflect.MultValuesType;
import gnu.kawa.servlet.HttpRequestContext;

public class Scheme extends LispLanguage {
    public static final int FOLLOW_R5RS = 5;
    public static final int FOLLOW_R6RS = 6;
    public static final int FOLLOW_R7RS = 7;

    int standardToFollow;

    public int getStandardToFollow() { return standardToFollow; }

    private static Environment r5Environment;
    protected static final SimpleEnvironment kawaEnvironment =
        Environment.make("kawa-environment");

    public static final Scheme instance = new Scheme(kawaEnvironment);
    private static Scheme r5rsInstance;
    private static Scheme r6rsInstance;
    private static Scheme r7rsInstance;

    public static final LangPrimType booleanType =
        new LangPrimType(Type.booleanType, instance);

    public static final ApplyToArgs applyToArgs =
        new ApplyToArgs("apply-to-args", instance);
    public static final Apply apply =
        new Apply("apply", applyToArgs);

    public static final gnu.kawa.reflect.InstanceOf instanceOf =
        new gnu.kawa.reflect.InstanceOf(instance, "instance?");
    public static final Not not =
        new Not(instance, "not");

    public static final gnu.kawa.functions.IsEq isEq =
        new gnu.kawa.functions.IsEq(instance, "eq?");
    public static final gnu.kawa.functions.IsEqv isEqv =
        new gnu.kawa.functions.IsEqv(instance, "eqv?", isEq);
    public static final gnu.kawa.functions.IsEqual isEqual =
        new gnu.kawa.functions.IsEqual(instance, "equal?");
   
    public static final gnu.kawa.functions.Map map =
        new gnu.kawa.functions.Map(true, applyToArgs, isEq);
    public static final gnu.kawa.functions.Map forEach =
        new gnu.kawa.functions.Map(false, applyToArgs, isEq);
    public static final NumberCompare numEqu =
        NumberCompare.make(instance, "=", NumberCompare.TRUE_IF_EQU);
    public static final NumberCompare numGrt =
        NumberCompare.make(instance, ">", NumberCompare.TRUE_IF_GRT);
    public static final NumberCompare numGEq =
        NumberCompare.make(instance, ">=",
                           NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_EQU);
    public static final NumberCompare numLss =
        NumberCompare.make(instance, "<", NumberCompare.TRUE_IF_LSS);
    public static final NumberCompare numLEq =
        NumberCompare.make(instance, "<=",
                           NumberCompare.TRUE_IF_LSS|NumberCompare.TRUE_IF_EQU);
    public static final NumberPredicate isOdd =
        new NumberPredicate(instance, "odd?", NumberPredicate.ODD);
    public static final NumberPredicate isEven =
        new NumberPredicate(instance, "even?", NumberPredicate.EVEN);

    private static final String[] uniformVectorTags =
    {"s8", "s16", "s32", "s64", "u8", "u16", "u32", "u64", "f32", "f64" };

    public static final String emptyStringLeft = new String();
    public static final String emptyStringRight = new String();

    static {
        instance.initScheme();
    }

  public static Scheme getInstance()
  {
    return instance;
  }

  private static Scheme newStandardInstance (int standardToFollow)
  {
    Scheme instance = new Scheme(getStdEnvironment());
    instance.standardToFollow = standardToFollow;
    return instance;
  }

    public static Exception loadClass(String path, Environment env) {
        Environment saveEnv = Environment.setSaveCurrent(env);
        try {
            instance.loadClass(path);
        } catch (java.lang.ClassNotFoundException ex) {
            return ex;
        } finally {
            Environment.restoreCurrent(saveEnv);
        }
        return null;
    }

    public static synchronized Environment getR5rsEnvironment() {
        if (r5Environment == null) {
            r5Environment = Environment.make("r5rs-environment");
            Exception ex = loadClass("kawa.lib.scheme.r5rs", r5Environment);
            if (ex != null)
                throw new RuntimeException(ex);
        }
        return r5Environment;
    }

    static Environment stdEnvironment;
    public static synchronized Environment getStdEnvironment() {
        if (stdEnvironment == null) {
            stdEnvironment = Environment.make("standard-environment");
            Exception ex = loadClass("kawa.lib.kawa.base", stdEnvironment);
            if (ex == null)
                ex = loadClass("kawa.lib.kawa.mstrings", stdEnvironment);
            if (ex != null )
                throw new RuntimeException(ex);
            stdEnvironment.setLocked();
        }
        return stdEnvironment;
    }

    public static synchronized Scheme getR5rsInstance() {
        if (r5rsInstance == null)
            r5rsInstance = newStandardInstance(FOLLOW_R5RS);
        return r5rsInstance;
    }

    public static synchronized Scheme getR6rsInstance() {
        if (r6rsInstance == null)
            r6rsInstance = newStandardInstance(FOLLOW_R6RS);
        return r6rsInstance;
    }

    public static synchronized Scheme getR7rsInstance() {
        if (r7rsInstance == null)
            r7rsInstance = newStandardInstance(FOLLOW_R7RS);
        return r7rsInstance;
    }

  public static Environment builtin ()
  {
    return kawaEnvironment;
  }

    private void initScheme() {
        environ = kawaEnvironment;

        Environment saveEnv = Environment.setSaveCurrent(kawaEnvironment);
        try {
            loadClass("kawa.lib.kawa.base");
        } catch (java.lang.ClassNotFoundException ex) {
            // Ignore exception - happens while building kawa/lib.
            defAliasStFld("$construct$", "gnu.kawa.lispexpr.LispLanguage", "constructNamespace");

            defSntxStFld("object", "kawa.standard.object", "objectSyntax");
            defSntxStFld("module-export", "kawa.standard.export", "module_export");
            defSntxStFld("module-name", "kawa.standard.module_name",
                         "module_name");
            defSntxStFld("export", "kawa.standard.export", "export");
            defSntxStFld("import", "kawa.standard.ImportFromLibrary", "instance");
            defSntxStFld("require", "kawa.standard.require", "require");
            defSntxStFld("include", "kawa.standard.Include", "include");
        }
        finally {
            Environment.restoreCurrent(saveEnv);
        }

        kawaEnvironment.setLocked();

        int withServlets = HttpRequestContext.importServletDefinitions;
        if (withServlets > 0) {
            try {
                loadClass(withServlets > 1 ? "gnu.kawa.servlet.servlets"
                          : "gnu.kawa.servlet.HTTP");
            } catch (Exception ex) {
            }
        }
    }

  public Scheme ()
  {
    environ = kawaEnvironment;
    userEnv = getNewEnvironment();
  }

  protected Scheme (Environment env)
  {
    environ = env;
  }

  public String getName()
  {
    switch (standardToFollow)
      {
      case FOLLOW_R5RS:
        return "Scheme-r5rs";
      case FOLLOW_R6RS:
        return "Scheme-r6rs";
      case FOLLOW_R7RS:
        return "Scheme-r7rs";
      default:
        return "Scheme";
      }
  }

  public String getCompilationClass () { return "kawa.standard.SchemeCompilation"; }

  /** Evaluate Scheme expressions from string.
   * @param string the string containing Scheme expressions
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Language.voidObject if none. */
  public static Object eval (String string, Environment env)
  {
    return eval (new CharArrayInPort(string), env);
  }

  /** Evaluate Scheme expressions from stream.
   * @param port the port to read Scheme expressions from
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Language.voidObject if none. */
  public static Object eval (InPort port, Environment env)
  {
    SourceMessages messages = new SourceMessages();
    try
      {
	LispReader lexer = (LispReader)
	  Language.getDefaultLanguage().getLexer(port, messages);
	Object body = ReaderParens.readList(lexer, null, 0, 1, -1, -1);
        if (messages.seenErrors())
          throw new gnu.text.SyntaxException(messages);
	return Eval.evalBody(body, env, messages);
      }
    catch (gnu.text.SyntaxException e)
      {
	// The '\n' is because a SyntaxException includes a line number,
	// and it is better if that starts the line.  FIXME OBSOLETE
	throw new RuntimeException("eval: errors while compiling:\n"
				   +e.getMessages().toString(20));
      }
    catch (java.io.IOException e)
      {
	throw new RuntimeException("eval: I/O exception: "
				   + e.toString ());
      }
    catch (Throwable ex)
      {
	throw WrappedException.rethrow(ex);
      }
  }

  /** Evaluate Scheme expressions from an "S expression."
   * @param sexpr the S expression to evaluate
   * @param env the Environment to evaluate the string in
   * @return result of the expression. */
  public static Object eval (Object sexpr, Environment env)
  {
    try
      {
	return Eval.eval (sexpr, env);
      }
    catch (Throwable ex)
      {
	throw WrappedException.rethrow(ex);
      }
  }

  @Override
  public AbstractFormat getFormat(boolean readable)
  {
    return readable ? DisplayFormat.schemeWriteFormat
        : DisplayFormat.schemeDisplayFormat;
  }

  @Override
  public LispReader getLexer(InPort inp, SourceMessages messages)
  {
    LispReader reader = super.getLexer(inp, messages);
    if (reader.getReadCase() == '\0'
        && standardToFollow == FOLLOW_R5RS)
      reader.setReadCase('D');
    return reader;
  }

  @Override
  public int getNamespaceOf (Declaration decl)
  {
    return FUNCTION_NAMESPACE+VALUE_NAMESPACE;
  }

  /** If exp is a "constant" Type, return that type, otherwise return null. */
  public static Type getTypeValue (Expression exp)
  {
    return getInstance().getTypeFor(exp);
  }

    private HashMap<String,Type> types;
    private HashMap<Type,String> typeToStringMap;

    @Override
    protected synchronized HashMap<String, Type> getTypeMap() {
        if (types == null) {
            types = new HashMap<String, Type>(128); // Bit more wiggle room
            types.put("boolean", booleanType);
            types.put("parameter", Compilation.typeLocationProc);
            types.putAll(super.getTypeMap());
            for (int i = uniformVectorTags.length; --i >= 0;) {
                String tag = uniformVectorTags[i];
                String cname = "gnu.lists." + tag.toUpperCase() + "Vector";
                types.put(tag + "vector", ClassType.make(cname));
            }
        }
        return types;
    }

    public String formatType(Type type) {
        // FIXME synchronize
        if (type instanceof LazyType) {
            LazyType ltype = (LazyType) type;
            return formatType(ltype.getRawType())
                +'['+formatType(ltype.getValueType())+']';
        }
        if (type instanceof MultValuesType) {
            MultValuesType mtype = (MultValuesType) type;
            StringBuilder sbuf = new StringBuilder();
            sbuf.append("values[");
            int n = mtype.getValueCount();
            for (int i = 0; i < n; i++) {
                if (i > 0)
                    sbuf.append(' ');
                Type etype = mtype.getValueType(i);
                sbuf.append(etype == null ? "unspecified" : formatType(etype));
            }
            sbuf.append(']');
            return sbuf.toString();
        }
        if (type instanceof GenArrayType) {
            GenArrayType atype = (GenArrayType) type;
            StringBuilder sbuf = new StringBuilder("array");
            int rank = atype.rank();
            if (rank >= 0)
                sbuf.append(rank);
            Type elementType = atype.getComponentType();
            if (elementType != null && elementType != Type.objectType) {
                sbuf.append('[');
                sbuf.append(formatType(elementType));
                sbuf.append(']');
            }
            return sbuf.toString();
        }
        if (typeToStringMap == null)  {
            typeToStringMap = new HashMap<Type,String>();
            // Invert the map returned by getTypeMap.
            for (java.util.Map.Entry<String,Type> e : getTypeMap().entrySet())
                typeToStringMap.put(e.getValue(), e.getKey());
        }
        String str = typeToStringMap.get(type);
        if (str != null)
            return str;
        return super.formatType(type);
    }

 /** Convert expression to a Type.
   * Allow {@code "TYPE"} or {@code 'TYPE} or {@code <TYPE>}.
   */
  public static Type exp2Type (Expression exp)
  {
    return getInstance().getTypeFor(exp);
  }

  public Symbol asSymbol (String ident)
  {
    return Namespace.EmptyNamespace.getSymbol(ident);
  }

  /** Should the values of body/block be appended as multiple values?
   * Otherwise, just return the result of the final expression.
   */
  public boolean appendBodyValues () { return false; }

    @Override
    public boolean keywordsAreSelfEvaluating() { return false; }

  public ReadTable createReadTable ()
  {
    ReadTable tab = ReadTable.createInitial();
    int std =  standardToFollow;
    ReaderDispatch dispatchTable = (ReaderDispatch) tab.lookup('#');
    ReaderDispatchSyntaxQuote sentry = new ReaderDispatchSyntaxQuote();
    dispatchTable.set('\'', sentry);
    dispatchTable.set('`', sentry);
    dispatchTable.set(',', sentry);
    tab.putReaderCtorFld("path", "gnu.kawa.lispexpr.LangObjType", "pathType");
    tab.putReaderCtorFld("filepath", "gnu.kawa.lispexpr.LangObjType", "filepathType");
    tab.putReaderCtorFld("URI", "gnu.kawa.lispexpr.LangObjType", "URIType");
    tab.putReaderCtor("symbol", ClassType.make("gnu.mapping.Symbol"));
    tab.putReaderCtor("namespace", ClassType.make("gnu.mapping.Namespace"));
    tab.putReaderCtorFld("duration", "kawa.lib.numbers", "duration");
    if (std == FOLLOW_R5RS || std == FOLLOW_R6RS || std == FOLLOW_R7RS)
      {
      }
    else
      {
        tab.postfixLookupOperator = ':';
        tab.setFinalColonIsKeyword(true);
        tab.extraFlags = LispReader.SCM_LEXPONENT_IS_BIGDECIMAL;
        tab.set('@', new ReaderQuote(LispLanguage.splice_sym,
                                     ':', LispLanguage.splice_colon_sym,
                                     ReadTable.NON_TERMINATING_MACRO));
      }
    return tab;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(getInstance());
  }
}
