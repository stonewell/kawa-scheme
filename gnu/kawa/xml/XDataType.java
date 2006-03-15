// Copyright (c) 2006  Per M.A. Bothner.
// This is free software;  for specifics see ../../../COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.mapping.Printable;
import gnu.math.*;
import java.math.BigDecimal;

/** An atomic type as used in XML Schema and related languages.
 * For example the {code xs:decimal} type is {@code XDataType.decimalType}.
 */

public class XDataType extends Type implements TypeValue
{
  Type implementationType;

  Object name;

  /** The "parent" type. */
  XDataType baseType;

  /** One of the {@code XXXX_TYPE_CODE} constants. */
  int typeCode;

  public static final int DECIMAL_TYPE_CODE = 2;
  public static final int INTEGER_TYPE_CODE = 3;
  public static final int NON_POSITIVE_INTEGER_TYPE_CODE = 4;
  public static final int NEGATIVE_INTEGER_TYPE_CODE = 5;
  public static final int LONG_TYPE_CODE = 6;
  public static final int INT_TYPE_CODE = 7;
  public static final int SHORT_TYPE_CODE = 8;
  public static final int BYTE_TYPE_CODE = 9;
  public static final int NONNEGATIVE_INTEGER_TYPE_CODE = 10;
  public static final int UNSIGNED_LONG_TYPE_CODE = 11;
  public static final int UNSIGNED_INT_TYPE_CODE = 12;
  public static final int UNSIGNED_SHORT_TYPE_CODE = 13;
  public static final int UNSIGNED_BYTE_TYPE_CODE = 14;
  public static final int POSITIVE_INTEGER_TYPE_CODE = 15;

  public static final int FLOAT_TYPE_CODE = 16;
  public static final int DOUBLE_TYPE_CODE = 17;

  public static final int DATE_TIME_TYPE_CODE = 18;
  public static final int DATE_TYPE_CODE = 19;
  public static final int TIME_TYPE_CODE = 20;
  public static final int G_YEAR_MONTH_TYPE_CODE = 21;
  public static final int G_YEAR_TYPE_CODE = 22;
  public static final int G_MONTH_DAY_TYPE_CODE = 23;
  public static final int G_DAY_TYPE_CODE = 24;
  public static final int G_MONTH_TYPE_CODE = 25;
  public static final int DURATION_TYPE_CODE = 26;
  public static final int YEAR_MONTH_DURATION_TYPE_CODE = 27;
  public static final int DAY_TIME_DURATION_TYPE_CODE = 28;

  public static final int BOOLEAN_TYPE_CODE = 29;

  public static final int QNAME_TYPE_CODE = 30;
  public static final int ANY_URI_TYPE_CODE = 31;
  public static final int BASE64_BINARY_TYPE_CODE = 32;
  public static final int HEX_BINARY_TYPE_CODE = 33;
  public static final int NOTATION_TYPE_CODE = 34;

  public static final int UNTYPED_ATOMIC_TYPE_CODE = 35;

  public static final int STRING_TYPE_CODE = 36;

  public XDataType (Object name, Type implementationType, int typeCode)
  {
    super(implementationType);
    this.name = name;
    if (name != null)
      setName(name.toString());
    this.implementationType = implementationType;
    this.typeCode = typeCode;
  }

  public static final XDataType stringType =
    new XDataType("string", ClassType.make("java.lang.String"),
                  STRING_TYPE_CODE);

  public static final XDataType untypedAtomicType =
    new XDataType("string", ClassType.make("gnu.kawa.xml.UntypedAtomic"),
                  UNTYPED_ATOMIC_TYPE_CODE);

  public static final XDataType hexBinaryType =
    new XDataType("hexBinary", ArrayType.make(Type.byte_type),
                  HEX_BINARY_TYPE_CODE);

  public static final XDataType booleanType =
    new XDataType("boolean", ClassType.make("java.lang.Boolean"),
                  BOOLEAN_TYPE_CODE);

  public static final XDataType anyURIType =
    new XDataType("anyURI",
                  /* #ifdef use:java.net.URI */
                  ClassType.make("java.net.URI"),
                  /* #else */
                  // ClassType.make("java.lang.String"),
                  /* #endif */
                  ANY_URI_TYPE_CODE);

  public static final XDataType decimalType =
    new XDataType("decimal", ClassType.make("java.math.BigDecimal"),
                  DECIMAL_TYPE_CODE);

  public static final XDataType floatType =
    new XDataType("float", ClassType.make("java.lang.Float"), FLOAT_TYPE_CODE);

  public static final XDataType doubleType =
    new XDataType("double", ClassType.make("java.lang.Double"), DOUBLE_TYPE_CODE);

  public static final XDataType durationType =
    new XDataType("duration", ClassType.make("gnu.math.Duration"),
                  DURATION_TYPE_CODE);

  public static final XDataType yearMonthDurationType =
    new XDataType("yearMonthDuration", ClassType.make("gnu.math.Duration"),
                  YEAR_MONTH_DURATION_TYPE_CODE);

  public static final XDataType dayTimeDurationType =
    new XDataType("dayTimeDuration", ClassType.make("gnu.math.Duration"),
                  DAY_TIME_DURATION_TYPE_CODE);

  public java.lang.Class getReflectClass()
  {
    return implementationType.getReflectClass();
  }

  public Type getImplementationType()
  {
    return implementationType;
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    Compilation comp = Compilation.getCurrent();
    comp.compileConstant(this, Target.pushObject);
    Method meth = ClassType.make("gnu.kawa.xml.XDataType")
      .getDeclaredMethod("coerceFromObject", 1);
    code.emitSwap();
    code.emitInvokeVirtual(meth);
    // Needed to avoid VerifyErrors.
    implementationType.emitCoerceFromObject(code);
  }

  public void emitTestIf(Variable incoming, Declaration decl, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    comp.compileConstant(this, Target.pushObject);
    if (incoming == null)
      code.emitSwap();
    else
      code.emitLoad(incoming);
    if (decl != null)
      {
        code.emitDup();
        decl.compileStore(comp);
      }
    code.emitInvokeVirtual(Compilation.typeType
                           .getDeclaredMethod("isInstance", 1));
    code.emitIfIntNotZero();
  }

  public boolean isInstance (Object obj)
  {
    switch (typeCode)
      {
      case STRING_TYPE_CODE:
        return obj instanceof java.lang.String;
      case UNTYPED_ATOMIC_TYPE_CODE:
        return obj instanceof gnu.kawa.xml.UntypedAtomic;
      case ANY_URI_TYPE_CODE:
        /* #ifdef use:java.net.URI */
        return obj instanceof java.net.URI;
        /* #else */
        // return obj instanceof String;
        /* #endif */
      case BOOLEAN_TYPE_CODE:
        return obj instanceof java.lang.Boolean;
      case FLOAT_TYPE_CODE:
        return obj instanceof java.lang.Float;
      case DOUBLE_TYPE_CODE:
        return obj instanceof java.lang.Double;
      case DECIMAL_TYPE_CODE:
        return obj instanceof java.math.BigDecimal
          || obj instanceof gnu.math.IntNum;
      case DURATION_TYPE_CODE:
        return obj instanceof Duration;
      case YEAR_MONTH_DURATION_TYPE_CODE:
        return obj instanceof Duration
          && ((Duration) obj).unit() == Unit.month;
      case DAY_TIME_DURATION_TYPE_CODE:
        return obj instanceof Duration
          && ((Duration) obj).unit() == Unit.second;
      default:
        return super.isInstance(obj);
      }
  }

  public void emitIsInstance(Variable incoming,
			     Compilation comp, Target target)
  {
    gnu.kawa.reflect.InstanceOf.emitIsInstance(this, incoming, comp, target);
  }

  public String toString (Object value)
  {
    return value.toString();
  }

  public void print (Object value, java.io.PrintWriter out)
  {
    if (value instanceof Printable)
      ((Printable) value).print(out);
    else
      //out.print(value);
      out.print(toString(value));
  }

  public boolean castable (Object value)
  {
    try
      {
        // FIXME - inefficient!
        cast(value);
        return true;
      }
    catch (Throwable ex)
      {
        return false;
      }
  }

  public Object cast (Object value)
  {
    value = KNode.atomicValue(value);
    if (value instanceof UntypedAtomic)
      {
        if (typeCode == UNTYPED_ATOMIC_TYPE_CODE)
          return value;
        return valueOf(value.toString());
      }
    if (value instanceof String)
      return valueOf(value.toString());
    switch (typeCode)
      {
      case STRING_TYPE_CODE:
        return StringValue.stringValue(value);
      case UNTYPED_ATOMIC_TYPE_CODE:
        return new UntypedAtomic(StringValue.stringValue(value));
      case ANY_URI_TYPE_CODE:
        return toURI(value);
      case BOOLEAN_TYPE_CODE:
        if (value instanceof Boolean)
          return (((Boolean)value).booleanValue() ? Boolean.TRUE
                  : Boolean.FALSE);
        if (value instanceof Number)
          {
            double d = ((Number) value).doubleValue();
            return d == 0.0 || Double.isNaN(d) ? Boolean.FALSE : Boolean.TRUE;
          }
        break;
      case DECIMAL_TYPE_CODE:
        // Partly duplicates Arithmetic asBigDecimal.
        if (value instanceof java.math.BigDecimal)
          return value;
        if (value instanceof gnu.math.RealNum)
          return ((gnu.math.RealNum) value).asBigDecimal();
        if (value instanceof Float || value instanceof Double)
          {
            double d = ((Number) value).doubleValue();
            /* #ifdef JAVA5 */
            // return BigDecimal.valueOf(d);
            /* #else */
            return new BigDecimal(d);
            /* #endif */
          }
        if (value instanceof Boolean)
          return cast(((Boolean)value).booleanValue() ? IntNum.one()
                      : IntNum.zero());
        break;
      case FLOAT_TYPE_CODE:
        if (value instanceof java.lang.Float)
          return value;
        if (value instanceof java.lang.Number)
          // Wrong for complex numbers with non-zero imaginary part.  FIXME.
          return makeFloat(((Number) value).floatValue());
        if (value instanceof Boolean)
          return ((Boolean)value).booleanValue() ? FLOAT_ONE : FLOAT_ZERO;
        break;
      case DOUBLE_TYPE_CODE:
        if (value instanceof java.lang.Double)
          return value;
        if (value instanceof java.lang.Number)
          // Wrong for complex numbers with non-zero imaginary part.  FIXME.
          return makeDouble(((Number) value).doubleValue());
        if (value instanceof Boolean)
          return ((Boolean)value).booleanValue() ? DOUBLE_ONE : DOUBLE_ZERO;
        break;
      case G_YEAR_TYPE_CODE:
      case G_YEAR_MONTH_TYPE_CODE:
      case G_MONTH_TYPE_CODE:
      case G_MONTH_DAY_TYPE_CODE:
      case G_DAY_TYPE_CODE:
        if (value instanceof DateTime)
          {
            int dstMask = XTimeType.components(((XTimeType) this).typeCode);
            DateTime dt = (DateTime) value;
            int srcMask = dt.components();
            if (dstMask == srcMask
                || (srcMask & DateTime.DATE_MASK) == DateTime.DATE_MASK)
              return dt.cast(dstMask);
            throw new ClassCastException();
          }
        break;
      case DATE_TYPE_CODE:
      case TIME_TYPE_CODE:
      case DATE_TIME_TYPE_CODE:
        if (value instanceof DateTime)
          {
            int mask = XTimeType.components(((XTimeType) this).typeCode);
            return ((DateTime) value).cast(mask);
          }
        break;
      case DURATION_TYPE_CODE:
        return castToDuration(value, Unit.duration);
      case YEAR_MONTH_DURATION_TYPE_CODE:
        return castToDuration(value, Unit.month);
      case DAY_TIME_DURATION_TYPE_CODE:
        return castToDuration(value, Unit.second);
      }
    return coerceFromObject(value);
  }

  Duration castToDuration (Object value, Unit unit)
  {
    if (value instanceof Duration)
      {
        Duration dur = (Duration) value;
        if (dur.unit() == unit)
          return dur;
        int months = dur.getTotalMonths();
        long seconds = dur.getTotalSeconds();
        int nanos = dur.getNanoSecondsOnly();
        if (unit == Unit.second)
          months = 0;
        if (unit == Unit.month)
          {
            seconds = 0;
            nanos = 0;
          }
        return Duration.make(months, seconds, nanos, unit);
      }
    return (Duration) coerceFromObject(value);
  }

  public Object coerceFromObject (Object obj)
  {
    if (! isInstance(obj))
      throw new ClassCastException("cannot cast "+obj+" to "+name);
    return obj;
  }

  public int compare(Type other)
  {
    if (this == other)
      return 0;
    return implementationType.compare(other); // FIXME
  }

  /* #ifdef use:java.net.URI */
  private static java.net.URI toURI (Object value)
  {
    try
      {
        return gnu.text.URI_utils.toURI(value);
      }
    catch (java.net.URISyntaxException ex)
      {
        return (java.net.URI) value;
      }
  }
  /* #else */
  // private static String toURI (Object value)
  // {
  //   return gnu.text.URI_utils.toURI(value);
  // }
  /* #endif */

  public Object valueOf (String value)
  {
    switch (typeCode)
      {
      case STRING_TYPE_CODE:
        return value;
      case UNTYPED_ATOMIC_TYPE_CODE:
        return new UntypedAtomic(value);
      case ANY_URI_TYPE_CODE:
        return toURI(value);
      case BOOLEAN_TYPE_CODE:
        value = value.trim();
        if (value.equals("true") || value.equals("1"))
          return Boolean.TRUE;
        if (value.equals("false") || value.equals("0"))
          return Boolean.FALSE;
        throw new IllegalArgumentException("not a valid boolean: '"+value+"'");
      case FLOAT_TYPE_CODE:
      case DOUBLE_TYPE_CODE:
        value = value.trim();
        if ("INF".equals(value)) value = "Infinity";
        else if ("-INF".equals(value)) value = "-Infinity";
        return typeCode ==  FLOAT_TYPE_CODE ? (Object) Float.valueOf(value)
          : (Object) Double.valueOf(value);
      case DECIMAL_TYPE_CODE:
        value = value.trim();
        // FIXME - this is a little too liberal.
        return new java.math.BigDecimal(value);
      case DURATION_TYPE_CODE:
        return Duration.parseDuration(value);
      case YEAR_MONTH_DURATION_TYPE_CODE:
        return Duration.parseYearMonthDuration(value);
      case DAY_TIME_DURATION_TYPE_CODE:
        return Duration.parseDayTimeDuration(value);
      case HEX_BINARY_TYPE_CODE:
        return parseHexBinary(value);
      default:
        throw new RuntimeException("valueOf not implemented for "+name);
      }
  }

  static byte[] parseHexBinary (String str)
  {
    str = str.trim();
    int len = str.length();
    if ((len & 1) != 0)
      throw new IllegalArgumentException("hexBinary string length not a multiple of 2");
    len = len >> 1;
    byte[] result = new byte[len];
    for (int i = 0;  i < len;  i++)
      {
        int d1 = Character.digit(str.charAt(2*i), 16);
        int d2 = Character.digit(str.charAt(2*i+1), 16);
        int bad = -1;
        if (d1 < 0)  bad = 2*i;
        else if (d2 < 0)  bad = 2*i+1;
        if (bad >= 0)
          throw new IllegalArgumentException("invalid hexBinary character at position "+bad);
        result[i] = (byte) (16 * d1 + d2);
      }
    return result;
  }

  public static Float makeFloat (float value)
  {
    /* #ifdef JAVA5 */
    // return Float.valueOf(value);
    /* #else */
    return new Float(value);
    /* #endif */
  }

  public static Double makeDouble (double value)
  {
    /* #ifdef JAVA5 */
    // return Double.valueOf(value);
    /* #else */
    return new Double(value);
    /* #endif */
  }

  public static final Double DOUBLE_ZERO = makeDouble(0);
  public static final Double DOUBLE_ONE = makeDouble(1);
  public static final Float FLOAT_ZERO = makeFloat(0);
  public static final Float FLOAT_ONE = makeFloat(1);
  public static final BigDecimal DECIMAL_ONE = BigDecimal.valueOf(1);
}