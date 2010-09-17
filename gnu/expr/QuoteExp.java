package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.Type;

/**
 * An Expression that evaluates to a constant value.
 * @author	Per Bothner
 */

public class QuoteExp extends Expression
{
  Object value;

  public final Object getValue() { return value; }

  public final Object valueIfConstant() { return value; }

  protected Type type;
  public final gnu.bytecode.Type getRawType() { return type; }
  public final gnu.bytecode.Type getType()
  {
    if (type != null)
      return type;
    if (value == Values.empty)
      return Type.voidType;
    else if (value == null)
      return Type.nullType;
    else if (this == undefined_exp)
      return Type.pointer_type;
    else
      return Type.make(value.getClass());
  }

  static public QuoteExp undefined_exp =
    new QuoteExp (Special.undefined);
  static public QuoteExp abstractExp =
    new QuoteExp(Special.abstractSpecial);
  static public QuoteExp voidExp =
    new QuoteExp (Values.empty, Type.voidType);
  static public QuoteExp trueExp = new QuoteExp(Boolean.TRUE);
  static public QuoteExp falseExp = new QuoteExp(Boolean.FALSE);
  static public QuoteExp nullExp =
    new QuoteExp(null, Type.nullType);
  public static final QuoteExp classObjectExp =
    new QuoteExp(Type.pointer_type);

  public static QuoteExp getInstance (Object value)
  {
    if (value == null)
      return nullExp;
    if (value == Type.pointer_type)
      return classObjectExp;
    if (value == Special.undefined)
      return undefined_exp;
    if (value == Values.empty)
      return voidExp;
    // Note we deliberately don't map abstractSpecial to abstractExp.
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue() ? trueExp : falseExp;
    return new QuoteExp(value);
  }

  public QuoteExp (Object val) { value = val; }

  public QuoteExp (Object val, Type type) { value = val; this.type = type; }
  
  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx)
  {
    ctx.writeValue(value);
  }

  public void compile (Compilation comp, Target target)
  {
    if (type == null || type == Type.pointer_type
        || target instanceof IgnoreTarget
        || (type instanceof gnu.bytecode.ObjectType
            && type.isInstance(value)))
      comp.compileConstant(value, target);
    else
      {
        comp.compileConstant(value, StackTarget.getInstance(type));
        target.compileFromStack(comp, type);
      }
  }
 
  public Expression deepCopy (gnu.kawa.util.IdentityHashTable mapper)
  {
    return this;
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitQuoteExp(this, d);
  }

  public Expression validateApply (ApplyExp exp, InlineCalls visitor,
                                   Type required, Declaration decl)
  {
    if (this == QuoteExp.undefined_exp)
      return exp;
    Object fval = getValue();
    if (! (fval instanceof Procedure))
      return visitor.noteError(decl == null || fval == null ? "called value is not a procedure"
			      : ("calling " + decl.getName()
				 + " which is a "+fval.getClass().getName()));
    Procedure proc = (Procedure) fval;
    int nargs = exp.getArgCount();
    String msg = WrongArguments.checkArgCount(proc, nargs);
    if (msg != null)
      return visitor.noteError(msg);
    Expression inlined = visitor.maybeInline(exp, required, proc);
    if (inlined != null)
      return inlined;
    exp.args = visitor.visitExps(exp.args, exp.args.length, null);
    if (exp.getFlag(ApplyExp.INLINE_IF_CONSTANT))
      {
	Expression e = exp.inlineIfConstant(proc, visitor);
	if (e != exp)
	  return visitor.visit(e, required);
      }
    Compilation comp = visitor.getCompilation();
    if (comp.inlineOk(proc))
      {
	if (ApplyExp.asInlineable(proc) != null)
          {
            if (exp.getFunction() == this)
              return exp;
            return new ApplyExp(this, exp.getArgs()).setLine(exp);
          }
	PrimProcedure mproc
	  = PrimProcedure.getMethodFor(proc, decl, exp.args,
				       comp.getLanguage());
	if (mproc != null)
	  {
	    ApplyExp nexp;
	    if (mproc.getStaticFlag() || decl == null)
	      nexp = new ApplyExp(mproc, exp.args);
	    else if (decl.base == null)
	      return exp;
	    else
	      {
		Expression[] margs = new Expression[1 + nargs];
		System.arraycopy(exp.getArgs(), 0, margs, 1, nargs);
		margs[0] = new ReferenceExp(decl.base);
		nexp = new ApplyExp(mproc, margs);
	      }
	    return nexp.setLine(exp);
	  }
      }
    return exp;
  }

  public boolean side_effects () { return false; }

  public String toString ()
  {
    return "QuoteExp["+value+"]";
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Quote", ")", 2);
    out.writeSpaceLinear();
    Object val = this.value;
    if (val instanceof Expression)
      val = val.toString(); // To avoid cycles.
    gnu.lists.AbstractFormat saveFormat = out.objectFormat;
    try
      {
	out.objectFormat = Language.getDefaultLanguage().getFormat(true);
	out.print(val);
        /*
        if (value != null)
          {
            out.print(" ::");
            out.print(value.getClass().getName());
          }
        */
      }
    finally
      {
	out.objectFormat = saveFormat;
      }
    out.endLogicalBlock(")");
  }
}
