package gnu.kawa.functions;

import gnu.expr.Language;
import gnu.mapping.*;
import java.util.ArrayList;
import java.util.Map;

/** Operator to unify two values, in the logic programming sense.
 * If a value or sub-value is a blank promise, it is bound or linked.
 */

public class Unify extends IsEqual {
    public static Object OK = Values.empty;

    public Unify(Language language, String name) {
        super(language, name);
    }

    public boolean apply (Object arg1, Object arg2,
                          Map<Object,ArrayList<Object>> map) {
        while (arg1 instanceof Promise) {
            Promise p1 = (Promise) arg1;
            if (p1.isBlank()) {
                // FIXME record trail so we can backtrack
                if (arg2 instanceof Lazy)
                    p1.setAlias((Lazy) arg2);
                else
                    p1.setValue(arg2);
                return true;
            }
            Lazy l1 = p1.checkAlias();
            if (l1 == null)
                break;
            arg1 = l1; 
        }
        while (arg2 instanceof Promise) {
            Promise p2 = (Promise) arg2;
            if (p2.isBlank()) {
                // FIXME record trail so we can backtrack
                if (arg1 instanceof Lazy)
                    p2.setAlias((Lazy) arg1);
                else
                    p2.setValue(arg1);
                return true;
            }
            Lazy l2 = p2.checkAlias();
            if (l2 == null)
                break;
            arg2 = l2; 
        }
        if (arg1 instanceof Lazy) {
            arg1 = Promise.force(arg1);
        }
        if (arg2 instanceof Lazy) {
            arg2 = Promise.force(arg2);
        }
        return match(arg1, arg2, map);
    }

    public Object apply2(Object arg1, Object arg2) {
        if (apply(arg1, arg2))
            return OK;
        else
            throw new RuntimeException("match failure");
    }
}
