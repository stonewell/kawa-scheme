package gnu.xquery.utils;
import gnu.kawa.xml.*;
import gnu.math.*;
import gnu.mapping.WrongType;
import gnu.mapping.Values;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.TimeZone;

public class TimeUtils
{
  static DateTime coerceToDateTime (String fun, Object value)
  {
    if (XTimeType.dateTimeType.isInstance(value))
      return (DateTime) value;
    throw new WrongType(fun, 1, "xs:dateTime");
  }

  static DateTime coerceToDate (String fun, Object value)
  {
    if (XTimeType.dateType.isInstance(value))
      return (DateTime) value;
    throw new WrongType(fun, 1, "xs:date");
  }

  static DateTime coerceToTime (String fun, Object value)
  {
    if (XTimeType.timeType.isInstance(value))
      return (DateTime) value;
    throw new WrongType(fun, 1, "xs:time");
  }

  static Duration coerceToDuration (String fun, Object value)
  {
    if (value instanceof Duration)
      return (Duration) value;
    throw new WrongType(fun, 1, "xs:duration");
  }

  static Object timeZoneFromXTime (DateTime time)
  {
    if (time.isZoneUnspecified())
      return Values.empty;
    return Duration.makeMinutes(time.getZoneMinutes());
  }

  static IntNum asInteger(int value)
  {
    return IntNum.make(value);
  }

  public static Object yearFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDateTime("year-from-dateTime", arg).getYear());
  }

  public static Object monthFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDateTime("month-from-dateTime", arg).getMonth());
  }

  public static Object dayFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDateTime("day-from-dateTime", arg).getDay());
  }

  public static Object hoursFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDateTime("hours-from-dateTime", arg).getHours());
  }

  public static Object minutesFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDateTime("minutes-from-dateTime", arg).getMinutes());
  }

  static Number getSeconds (DateTime date)
  {
    int seconds = date.getSecondsOnly();
    long nanos = date.getNanoSecondsOnly();
    if (nanos == 0)
      return IntNum.make(seconds);
    nanos += seconds * 1000000000L;
    return new BigDecimal(BigInteger.valueOf(nanos), 9);
  }

  public static Object secondsFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return getSeconds(coerceToDateTime("seconds-from-dateTime", arg));
  }

  public static Object timezoneFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return timeZoneFromXTime(coerceToDateTime("timezone-from-datetime", arg));
  }

  public static Object yearFromDate (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDate("year-from-date", arg).getYear());
  }

  public static Object monthFromDate (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDate("month-from-date", arg).getMonth());
  }

  public static Object dayFromDate (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDate("day-from-date", arg).getDay());
  }

  public static Object timezoneFromDate (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return timeZoneFromXTime(coerceToDate("timezone-from-date", arg));
  }

  public static Object hoursFromTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToTime("hours-from-time", arg).getHours());
  }

  public static Object minutesFromTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToTime("minutes-from-time", arg).getMinutes());
  }

  public static Object secondsFromTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return getSeconds(coerceToTime("seconds-from-time", arg));
  }

  public static Object timezoneFromTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return timeZoneFromXTime(coerceToTime("timezone-from-time", arg));
  }

  public static Object yearsFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDuration("years-from-duration", arg).getYears());
  }

  public static Object monthsFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDuration("months-from-duration", arg).getMonths());
  }

  public static Object daysFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDuration("days-from-duration", arg).getDays());
  }

  public static Object hoursFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDuration("hours-from-duration", arg).getHours());
  }

  public static Object minutesFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDuration("minutes-from-duration", arg).getMinutes());
  }

  public static Object secondsFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    Duration d = coerceToDuration("minutes-from-duration", arg);
    int s = d.getSecondsOnly();
    int n = d.getNanoSecondsOnly();
    if (n == 0)
      return asInteger(s);
    else
      {
        int scale = 9;
        long ns = s * 1000000000L + n;
        while (ns % 10 == 0)
          {
            ns = ns / 10;
            scale--;
          }
        return new BigDecimal(BigInteger.valueOf(ns), scale);
      }
  }

  public static Duration getImplicitTimezone ()
  {
    return Duration.makeMinutes(TimeZone.getDefault().getRawOffset() / 60000);
  }

  public static Object adjustDateTimeToTimezone (Object time)
  {
    return adjustDateTimeToTimezone(time, getImplicitTimezone());
  }

  public static Object adjustDateTimeToTimezone (Object time, Object zone)
  {
    if (time == Values.empty || time == null)
      return time;
    DateTime dtime = (DateTime) time;
    if (zone == Values.empty || zone == null)
      return dtime.withZoneUnspecified();
    else
      return dtime.adjustTimezone((int) ((Duration) zone).getTotalMinutes());
  }
}