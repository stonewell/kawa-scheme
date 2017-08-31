package gnu.jemacs.lang;
import gnu.math.*;
import gnu.mapping.*;

public class AddOp extends ProcedureN
{
  int plusOrMinus = 1;

  public AddOp(String name, int plusOrMinus)
  {
    setName(name);
    this.plusOrMinus = plusOrMinus;
  }

  public static final AddOp PLUS = new AddOp("+", 1);
  public static final AddOp MINUS = new AddOp("-", -1);

  public static Object apply2(int plusOrMinus, Object arg1, Object arg2)
  {
    Numeric num1 = ELisp.asNumber(arg1);
    Numeric num2 = ELisp.asNumber(arg2);
    return num1.add(num2, plusOrMinus);
  }

  public static Object PLUS(Object arg1, Object arg2)
  {
    return apply2(1, arg1, arg2);
  }

  public static Object MINUS(Object arg1, Object arg2)
  {
    return apply2(-1, arg1, arg2);
  }

  public static Object MINUS(Object arg1)
  {
    return ELisp.asNumber(arg1).neg();
  }

  public static Object PLUS$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return applyN(1, apply2(1,apply2(1, arg1, arg2), arg3), rest);
  }

  public static Object MINUS$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return applyN(-1, apply2(-1,apply2(-1, arg1, arg2), arg3), rest);
  }

  public static Object applyN(int plusOrMinus, Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.zero ();
    Object result = args[0];
    if (len == 1 && plusOrMinus < 0)
      return MINUS(result);
    for (int i = 1; i < len; i++)
      result = apply2(plusOrMinus, result, args[i]);
    return result;
  }

  public static Object applyN(int plusOrMinus, Object init, Object[] args)
  {
    int len = args.length;
    Object result = init;
    for (int i = 0; i < len; i++)
      result = apply2(plusOrMinus, result, args[i]);
    return result;
  }

  public Object applyN (Object[] args)
  {
    return applyN(plusOrMinus, args);
  }
}
