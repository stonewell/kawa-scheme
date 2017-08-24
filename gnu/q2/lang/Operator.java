package gnu.q2.lang;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.kawa.reflect.*;
import gnu.mapping.*;

/** A syntactic operator, with precedence, like for example '+'. */

public class Operator extends Syntax
{
  int lprio;
  int rprio;
  Object function;
  int flags;

  static final int RHS_NEEDED = 2;
  static final int ASSIGN_OP = 4;
  static final int UNARY_PRIO = 88;

  public static final Operator FENCE = new Operator("<fence>", 0, -1, 0, null);
  public static final Operator SEMI = new Operator(";", 1, 1, 0, null);
  public static final Operator PLUS =
    new Operator("+", 5, 5, RHS_NEEDED, 
                 new StaticFieldLocation("gnu.kawa.functions.AddOp", "$Pl"));
  public static final Operator MINUS =
    new Operator("-", 5, 5, RHS_NEEDED, 
                 new StaticFieldLocation("gnu.kawa.functions.AddOp", "$Mn"));
  public static final Operator STAR =
    new Operator("*", 6, 6, RHS_NEEDED, 
                 new StaticFieldLocation("gnu.kawa.functions.MultiplyOp", "$St"));
  public static final Operator SLASH =
    new Operator("/", 6, 6, RHS_NEEDED, 
                 new StaticFieldLocation("gnu.kawa.functions.DivideOp", "$Sl"));
  public static final Operator LT =
    new Operator("<", 3, 3, RHS_NEEDED, 
                 new StaticFieldLocation("kawa.standard.Scheme", "numLss"));
  public static final Operator GT =
    new Operator(">", 3, 3, RHS_NEEDED, 
                 new StaticFieldLocation("kawa.standard.Scheme", "numGrt"));
  public static final Operator LE =
    new Operator("<=", 3, 3, RHS_NEEDED, 
                 new StaticFieldLocation("kawa.standard.Scheme", "numLEq"));
  public static final Operator GE =
    new Operator(">=", 3, 3, RHS_NEEDED, 
                 new StaticFieldLocation("kawa.standard.Scheme", "numGEq"));
  // FIXME should use IsEqual
  public static final Operator EQ =
    new Operator("==", 3, 3, RHS_NEEDED, 
                 new StaticFieldLocation("kawa.standard.Scheme", "numEqu"));
  public static final Operator ASSIGN =
    new Operator(":=", 2, 2, RHS_NEEDED|ASSIGN_OP, null);
  public static final Operator UNIFY =
    new Operator("=", 2, 2, RHS_NEEDED,
                 new StaticFieldLocation("gnu.q2.lang.Q2", "unify"));
  public static final Operator IF_THEN =
    new Operator("?>", 2, 2, RHS_NEEDED,
                 new StaticFieldLocation("kawa.lib.prim_imports", "if"));

  public Operator (String name, int lprio, int rprio, int flags,
                   Object function)
  {
    super(name);
    this.lprio = lprio;
    this.rprio = rprio;
    this.flags = flags;
    this.function = function;
  }
    static String LAMBDA_NAME = "$lambda$";
    static Operator makeLambda(Object params) {
        return new Operator(LAMBDA_NAME, UNARY_PRIO, 2, RHS_NEEDED, params);
    }

  public Object combine (Object largs, Object rargs, PairWithPosition op)
  {
    Object funop = function;
    LList args;
    if ((flags & ASSIGN_OP) != 0)
      {
        funop = Symbol.valueOf("set!"); // FIXME nonhygienic
        if (largs instanceof Pair && ((Pair) largs).getCdr() == LList.Empty)
          {
            ((Pair) largs).setCdrBackdoor(LList.list1(rargs));
            args = (Pair) largs;
          }
        else
          args = LList.list2(largs, rargs);
        return new PairWithPosition(op, funop, args);
      }
    if (this == SEMI)
      {
        funop = kawa.standard.begin.begin;
      }
    else if (function instanceof FieldLocation)
      {
        funop = new ReferenceExp(Symbol.valueOf(getName()),
                                 ((FieldLocation) function).getDeclaration());
      }
    if (largs == LList.Empty && rargs == LList.Empty)
      {
        return this == SEMI ? QuoteExp.voidExp : funop;
      }
    if (this.getSymbol() == LAMBDA_NAME) {
        funop = kawa.standard.SchemeCompilation.lambda;
        args = new Pair(this.function, LList.list1(rargs));
    }
    else if (largs == LList.Empty)
      args = LList.list1(rargs);
    else if (rargs == LList.Empty)
      args = LList.list1(largs);
    else
      args = LList.list2(largs, rargs);
    return new PairWithPosition(op, funop, args);
  }

  public String toString() { return "Operator["+getName()+"]"; }
}
