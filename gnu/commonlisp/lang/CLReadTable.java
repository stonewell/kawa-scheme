// Copyright (c) 2017  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.commonlisp.lang;
import gnu.kawa.lispexpr.*;
import gnu.mapping.*;
import gnu.text.Lexer;
import gnu.text.SyntaxException;
import java.io.IOException;
import gnu.expr.Keyword;
import gnu.lists.Pair;
import gnu.lists.LList;

/**
 * Readtable for Common Lisp syntax.
 */
class CLReadTable extends ReadTable {

    CLReadTable() {
	initialize(false);
	setInitialColonIsKeyword(true);
	ReaderDispatch sharp = (ReaderDispatch)lookup('#');
	ReadTableEntry plusminus = new SharpPlus();
	sharp.set('+', plusminus);
	sharp.set('-', plusminus);
    }

    @Override
    protected Object makeSymbol(String token) {
	LispPackage pkg = null;
	int colon = token.indexOf(':');
	if (colon == -1)
	    pkg = LispPackage.currentPackage.get();
	else {
	    String pkgName = token.substring(0, colon);
	    pkg = LispPackage.findPackage (pkgName);
	    if (pkg == null)
		throw new RuntimeException("no package with name: " + pkgName);
	    token = token.substring(colon + 1);
	}
	return LispPackage.intern(token, pkg);
    }
}

/**
 * Readtable entry for #+ and #-.
 */
class SharpPlus extends ReadTableEntry {

    @Override
    public Object read(Lexer in, int ch, int count)
	throws IOException, SyntaxException {
	LispReader r = (LispReader)in;
	Object exp = readFeatureExpression (r);
	boolean featurep = featurep (exp);
	boolean skip = (ch == '-') ? featurep : !featurep;
	if (skip)
	    r.readObject();
	return Values.empty;
    }

    static Object readFeatureExpression (LispReader reader)
	throws IOException, SyntaxException {
	ThreadLocation<LispPackage> pkg = LispPackage.currentPackage;
	Object old = pkg.setWithSave(LispPackage.KeywordNamespace);
	try {
	    return reader.readObject();
	} finally {
	    pkg.setRestore (old);
	}
    }

    static Keyword OR = Keyword.make("or");
    static Keyword AND = Keyword.make("and");
    static Keyword NOT = Keyword.make("not");

    // Evaluate the feature expression EXP.
    static boolean featurep(Object exp) {
	if (exp instanceof Symbol)
	    return CommonLisp.features.get().contains(exp);
	else if (exp instanceof Pair) {
	    Pair pair = (Pair)exp;
	    Object head = pair.getCar(), tail = pair.getCdr();
	    if (head == OR || head == AND) {
                while (tail instanceof Pair) {
                    pair = (Pair) tail;
                    boolean val = featurep(pair.getCar());
                    if (val == (head == OR))
                        return val;
                    tail = pair.getCdr();
                }
		if (tail == Pair.Empty) return (head == AND);
	    }
	    else if (head == NOT && tail instanceof Pair) {
		pair = (Pair) tail;
		if (pair.getCdr() == Pair.Empty)
		    return !featurep (pair.getCar());
	    }
	}
	throw new RuntimeException("invalid feature expression: " + exp);
    }
}
