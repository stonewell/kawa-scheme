package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the Scheme "set!" primitive.
 * @author	Per Bothner
 */

public class set_b extends Syntax implements Printable
{

  static private Pattern pattern = new ListPat (2, 2);

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return tr.syntaxError ("missing or extra arguments to set!");
    if (! (match[0] instanceof String))
      return tr.syntaxError ("first set! argument is not a variable name");
    String sym = (String) match[0];
    Expression value = tr.rewrite (match[1]);
    Object binding = tr.current_decls.get (sym);
    // Hygenic macro expansion may bind a renamed (uninterned) symbol
    // to the original symbol.  Here, use the original symbol.
    if (binding != null && binding instanceof String)
      return new SetExp ((String) binding, value);
    SetExp sexp = new SetExp (sym, value);
    sexp.binding = tr.resolve (sym, (Declaration) binding);
    if (sexp.binding != null)
      sexp.binding.noteValue (value);
    return sexp;
  }
}
