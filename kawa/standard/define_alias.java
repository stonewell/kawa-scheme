package kawa.standard;
import kawa.lang.*;
import gnu.kawa.util.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class define_alias extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Translator tr)
  {
    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	if (p1.car instanceof String && p1.cdr instanceof Pair)
	  {
            String name = (String) p1.car;
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == LList.Empty)
	      {
		Expression arg = location.rewriteArg(p2.car, tr);
		Expression loc = location.rewrite(arg, tr);
                if (tr.currentScope() instanceof ModuleExp)
                  {
                    Expression[] args = new Expression[2];
                    args[0] = new QuoteExp(name);
                    args[1] = loc;
                    Type[] argTypes = new Type[2];
                    argTypes[0] = Compilation.javaStringType;
                    argTypes[1] = Compilation.typeProcedure;
                    ClassType typeIndirectBinding
                      = ClassType.make("gnu.mapping.IndirectConstraint");
                    Method meth = typeIndirectBinding
                      .addMethod("define", argTypes,
                                 Scheme.voidType, Access.PUBLIC|Access.STATIC);
                    Expression proc = new QuoteExp(new PrimProcedure(meth));
                    return new ApplyExp(proc, args);
                  }
                else
                  {
                    Object binding = tr.environ.get(name);
                    // Hygenic macro expansion may bind a renamed (uninterned)
                    // symbol to the original symbol.
                    if (binding == null || binding instanceof String)
                      return tr.syntaxError ("invalid use of define");
                    SetExp sexp = new SetExp(name, loc);
                    sexp.binding = (Declaration) binding;
                    // sexp.binding.noteValue(loc);
                    sexp.setDefining (true);
                    return sexp;
                  }
	      }
	  }
      }
    return tr.syntaxError ("invalid syntax for define-alias");
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (! (st.cdr instanceof Pair)
        || (tr.currentScope() instanceof ModuleExp)
        || ! (((Pair) st.cdr).car instanceof String))
      return super.scanForDefinitions(st, forms, defs, tr);
    Object name = ((Pair) st.cdr).car;
    Type typeLocation = ClassType.make("gnu.mapping.Location");
    Declaration decl = defs.addDeclaration((String) name, typeLocation);
    decl.setIndirectBinding(true);
    forms.addElement(st);
    return true;
  }
}
