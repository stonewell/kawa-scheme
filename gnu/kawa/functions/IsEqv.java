package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.text.Char;
import gnu.expr.*;

/** Implement that standard Scheme function "eqv?". */

public class IsEqv extends Procedure2 implements CanInline
{
  Interpreter interpreter;
  IsEq isEq;

  public IsEqv(Interpreter interpreter, String name, IsEq isEq)
  {
    this.interpreter = interpreter;
    this.isEq = isEq;
    setName(name);
  }

  public static boolean apply (Object arg1, Object arg2) 
  {
    if (arg1==arg2)
      return true;
    if (arg1 instanceof Char || arg1 instanceof Numeric)
      return arg1.equals (arg2);
    return false;
   }

  public Object apply2 (Object arg1, Object arg2)
  {
    return interpreter.booleanObject(apply(arg1, arg2));
   }

  private static boolean nonNumeric(Expression exp)
  {
    if (exp instanceof QuoteExp)
      {
        Object value = ((QuoteExp) exp).getValue();
        return ! (value instanceof Numeric || value instanceof Char);
      }
    return false;
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    if (nonNumeric(args[0]) || nonNumeric(args[1]))
      return new ApplyExp(isEq, args);
    return exp;
  }
}
