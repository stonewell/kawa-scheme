package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Type;
import gnu.bytecode.CodeAttr;

/** Implement the standard Scheme procedure "not". */

public class not extends Procedure1 implements Inlineable
{
  public Object apply1 (Object arg1)
   {
     return Interpreter.boolObject (arg1 == Scheme.falseObject);
   }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression arg = exp.getArgs()[0];
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarget = (ConditionalTarget) target;
	ConditionalTarget sub_target
	  = new ConditionalTarget(ctarget.ifFalse, ctarget.ifTrue);
	sub_target.trueBranchComesFirst = ! ctarget.trueBranchComesFirst;
	arg.compile(comp, sub_target);
	return;
      }
    CodeAttr code = comp.getCode();
    Type type = target.getType();
    if (target instanceof StackTarget && type.getSignature().charAt(0) == 'Z')
      {
	arg.compile(comp, target);
	code.emitNot();
      }
    else
      {
	IfExp.compile(arg, QuoteExp.falseExp, QuoteExp.trueExp, comp, target);
      }
  }
}
