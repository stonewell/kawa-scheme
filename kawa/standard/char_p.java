package kawa.standard;
import kawa.lang.*;
             
public class char_p extends Procedure1
{
  public char_p()
  {
    super("char?");
  }

  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof Char)
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
