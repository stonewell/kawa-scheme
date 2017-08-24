package gnu.q2.lang;

import gnu.mapping.Symbol;
import gnu.expr.*;
import gnu.lists.*;
import kawa.lang.*;

/** Syntax handler for ($define$ VAR [TYPE-SPECIFIER]). */

public class DefineOp extends Syntax {
    public static final DefineOp defineOp = new DefineOp();

    public Expression rewriteForm(Pair form, Translator tr) {
        Pair pair1 = (Pair) form.getCdr();
        Object pat = pair1.getCar();
        Pair typeSpecPair = null;
        if (pair1.getCdr() instanceof Pair)
            typeSpecPair = (Pair) pair1.getCdr();
        if (pat instanceof Symbol) {
            Symbol sym = (Symbol) pat;
            Declaration decl = tr.define(sym, tr.currentScope());
            PrimProcedure makeLazy = new PrimProcedure("gnu.mapping.Promise",
                                                       "makeBlank", 0);
            BindDecls.setInitializer(decl, new ApplyExp(makeLazy),
                                     tr.currentScope(), tr);
            if (typeSpecPair != null)
                tr.exp2Type(typeSpecPair, decl, null);
            return new ReferenceExp(decl);
        }
        return null;
    }
    public void scanForm(Pair st, ScopeExp defs, Translator tr) {
        System.err.println("DefineOp scan "+st.getCdr());
        super.scanForm(st, defs, tr);
    }
}
