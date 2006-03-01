package gnu.math;
import java.io.*;

public class Duration extends Quantity implements Externalizable
{
  public Unit unit;

  /** Number of whole months.  May be negative. */
  int months;

  /** Does not include any leap seconds.
   * I.e. @code{sign * ((24 * days + hours) * 60 + minutes) * 60 + seconds},
   * where {@code hours >= 0 && hours < 24 && minutes >= 0 && minutes < 60
   * && secconds >= 0 && minutes > 60}.
   */
  int seconds;

  /** Number of nanoseconds.
   * We could possibly include leap seconds in here. */
  int nanos;

  public static Duration make (int months, int seconds, int nanos, Unit unit)
  {
    Duration d = new Duration();
    d.months = months;
    d.seconds = seconds;
    d.nanos = nanos;
    d.unit = unit;
    return d;
  }

  public static Duration makeMonths(int months)
  {
    Duration d = new Duration();
    d.unit = Unit.month;
    d.months = months;
    return d;
  }

  public static Duration makeMinutes (int minutes)
  {
    Duration d = new Duration();
    d.unit = Unit.second;
    d.seconds = 60 * minutes;
    return d;
  }

  public static Duration parse (String str, Unit unit)
  {
    Duration d = Duration.valueOf(str, unit);
    if (d == null)
      throw new IllegalArgumentException("not a valid "+unit.getName()+": '"+str+"'");
    return d;
  }

  public static Duration parseDuration (String str)
  {
    return parse(str, Unit.duration);
  }

  public static Duration parseYearMonthDuration (String str)
  {
    return parse(str, Unit.month);
  }


  public static Duration parseDayTimeDuration (String str)
  {
    return parse(str, Unit.second);
  }

  /** Parse a duration lexical value as specified by XML Schama.
   * Return null if invalid syntax.
   */
  public static Duration valueOf (String str, Unit unit)
  {
    str = str.trim();
    int pos = 0;
    int len = str.length();
    boolean negative;
    if (pos < len && str.charAt(pos) == '-')
      {
        negative = true;
        pos++;
      }
    else
      negative = false;
    if (pos == len || str.charAt(pos) != 'P')
      return null;
    pos++;
    int months = 0, seconds = 0, nanos = 0;
    long part = scanPart(str, pos);
    pos = ((int) part) >> 16;
    char ch = (char) part;
    if (unit == Unit.second && (ch == 'Y' || ch == 'M'))
      return null;
    if (ch == 'Y')
      {
        months = 12 * (int) (part >> 32);
        pos = ((int) part) >> 16;
        part = scanPart(str, pos);
        ch = (char) part;
      }
    if (ch == 'M')
      {
        months += (part >> 32);
        pos = ((int) part) >> 16;
        part = scanPart(str, pos);
        ch = (char) part;
      }
    if (ch == 'D')
      {
        seconds = (24 * 60 * 60) * (int) (part >> 32);
        pos = ((int) part) >> 16;
        part = scanPart (str, pos);
      }
    if (part != (pos << 16))
      return null;
    if (pos == len)
      {
        // No time part
      }
    else if (str.charAt(pos) != 'T')
      return null;
    else // saw 'T'
      {
        if (unit == Unit.month)
          return null;
        pos++; // Skip 'T'.
        part = scanPart (str, pos);
        ch = (char) part;
        if (ch == 'H')
          {
            seconds += (60 * 60) * (int) (part >> 32);
            pos = ((int) part) >> 16;
            part = scanPart (str, pos);
            ch = (char) part;
          }
        if (ch == 'M')
          {
            seconds += 60 * (int) (part >> 32);
            pos = ((int) part) >> 16;
            part = scanPart (str, pos);
            ch = (char) part;
          }
        if (ch == 'S')
          {
            seconds += (int) (part >> 32);
            pos = ((int) part) >> 16;
          }
        if (pos < len && str.charAt(pos) == '.')
          {
            // FIXME handle fraction
          }
      }
    Duration d = new Duration();
    if (negative)
      {
        months = -months;
        seconds = -seconds;
        nanos = -nanos;
      }
    d.months = months;
    d.seconds = seconds;
    d.nanos = nanos;
    d.unit = unit;
    return d;
  }

  public Numeric add (Object y, int k)
  {
    if (y instanceof Duration)
      return Duration.add (this, (Duration) y, k);
    throw new IllegalArgumentException ();
  }

  public Numeric mul (Object y)
  {
    if (y instanceof RealNum)
      return Duration.times(this, ((RealNum) y).doubleValue());
    return ((Numeric)y).mulReversed (this);
  }

  public Numeric mulReversed (Numeric x)
  {
    if (! (x instanceof RealNum))
      throw new IllegalArgumentException ();
    return Duration.times(this, ((RealNum) x).doubleValue());
  }

  public Numeric div (Object y)
  {
    if (y instanceof RealNum)
      return Duration.times(this, 1.0 / ((RealNum) y).doubleValue());
    return ((Numeric)y).divReversed (this);
  }

  public static Duration add (Duration x, Duration y, int k)
  {
    long months = (long) x.months + k * (long) y.months;
    // FIXME does not handle leap-seconds represented as multiples of
    // 10^9 in the nanos field.
    long nanos = x.seconds * 1000000000L + (long) x.nanos
      + k * (y.seconds * 1000000000L + y.nanos);
    // FIXME check for overflow
    // FIXME handle inconsistent signs.
    Duration d = new Duration();
    d.months = (int) months;
    d.seconds = (int) (nanos / 1000000000L);
    d.nanos = (int) (nanos % 1000000000L);
    d.unit = x.unit == y.unit ? x.unit : Unit.duration;
    return d;
  }

  public static Duration times (Duration x, double y)
  {
    double months = x.months * y;
    double nanos = (x.seconds * 1000000000L + x.nanos) * y;
    Duration d = new Duration();
    long n = (long) (nanos < 0 ? nanos - 0.5 : nanos + 0.5);
    d.months = (int) (months < 0 ? months - 0.5 : months + 0.5);
    d.seconds = (int) (nanos / 1000000000L);
    d.nanos = (int) (nanos % 1000000000L);
    d.unit = x.unit;
    return d;
  }

  public static int compare (Duration x, Duration y)
  {
    long months = (long) x.months - (long) y.months;
    long nanos = x.seconds * 1000000000L + (long) x.nanos
      - (y.seconds * 1000000000L + y.nanos);
    if (months < 0 && nanos <= 0)
      return -1;
    if (months > 0 && nanos >= 0)
      return 1;
    if (months == 0)
      return nanos < 0 ? -1 : nanos > 0 ? 1 : 0;
    return -3;
  }

  public int compare (Object obj)
  {
    if (obj instanceof Duration)
      return compare(this, (Duration) obj);
    // Could also compare other Quanties if units match appropriately.  FIXME.
    throw new IllegalArgumentException ();
  }

  public String toString ()
  {
    /* #ifdef JAVA5 */
    // StringBuilder sbuf = new StringBuilder();
    /* #else */
    StringBuffer sbuf = new StringBuffer();
    /* #endif */
    int m = months;
    int s = seconds;
    int n = nanos;
    boolean neg = m < 0 || s < 0 || n < 0;
    if (neg)
      {
        m = -m;
        s = -s;
        n = -n;
        sbuf.append('-');
      }
    sbuf.append('P');
    int y = m / 12;
    if (y != 0)
      {
        sbuf.append(y);
        sbuf.append('Y');
        m -= y * 12;
      }
    if (m != 0)
      {
        sbuf.append(m);
        sbuf.append('M');
      }
    int d = s / (24 * 60 * 60);
    if (d != 0)
      {
        sbuf.append(d);
        sbuf.append('D');
        s -= 24 * 60 * 60 * d;
      }
    if (s != 0 || n != 0)
      {
        sbuf.append('T');
        int hr = s / (60 * 60);
        if (hr != 0)
          {
            sbuf.append(hr);
            sbuf.append('H');
            s -= 60 * 60 * hr;
          }
        int mn = s / 60;
        if (mn != 0)
          {
            sbuf.append(mn);
            sbuf.append('M');
            s -= 60 * mn;
          }
        if (s != 0 || n != 0)
          {
            sbuf.append(s);
            if (n != 0)
              {
                sbuf.append('.');
                int pos = sbuf.length();
                sbuf.append(n);
                int len = sbuf.length();
                int pad = pos + 9 - len;
                while (--pad >= 0)
                  sbuf.insert(pos, '0');
                len = pos + 9;
                do { --len; } while (sbuf.charAt(len) == '0');
                sbuf.setLength(len+1);
              }
            sbuf.append('S');
          }
      }
    else if (sbuf.length() == 1)
      sbuf.append(unit == Unit.month ? "0Y" : "0S");
    return sbuf.toString();
  }

  /** Parse digits following by a terminator char
   * @return {@code (VALUE << 32)|(FOLLOWING_POS<<16)|FOLLOWING_CHAR}.
   * If there are no digits return @code{START<<16}.
   * Otherwise, on overflow or digits followed by end-of-string, return -1.
   */
  private static long scanPart (String str, int start)
  {
    int i = start;
    long val = -1;
    int len = str.length();
    while (i < len)
      {
        char ch = str.charAt(i);
        i++;
        int dig = Character.digit(ch, 10);
        if (dig < 0)
          {
            if (val < 0) return start << 16;
            return (val << 32) | (i << 16) | ((int) ch);
          }
        val = val < 0 ? dig : 10 * val + dig;
        if (val > Integer.MAX_VALUE)
          return -1; // overflow
      }
    return val < 0 ? (start << 16) : -1;
  }

  /** The number of years in the canonical representation. */
  public int getYears ()
  {
    return months / 12;
  }

  public int getMonths()
  {
    return months % 12;
  }

  public int getDays ()
  {
    return seconds / (24 * 60 * 60);
  }

  public int getHours ()
  {
    int hours = seconds / (60 * 60);
    return hours % 24;
  }

  public int getMinutes ()
  {
    int minutes = seconds / 60;
    return minutes % (24 * 60);
  }

  public int getSecondsOnly ()
  {
    return seconds % (24 * 60 * 60);
  }

  public int getNanoSecondsOnly ()
  {
    return nanos;
  }

  public long getNanoSeconds ()
  {
    return seconds * 1000000000L + nanos;
  }

  public boolean isZero ()
  {
    return months == 0 && seconds == 0 && nanos == 0;
  }

  public boolean isExact ()
  {
    return false;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeInt(months);
    out.writeInt(seconds);
    out.writeInt(nanos);
    out.writeObject(unit);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    months = in.readInt();
    seconds = in.readInt();
    nanos = in.readInt();
    unit = (Unit) in.readObject();
  }

  public Unit unit() { return unit; }
  public Complex number ()
  {
    throw new Error("number needs to be implemented!");
  }
}