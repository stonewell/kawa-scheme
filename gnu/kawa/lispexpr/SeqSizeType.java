package gnu.kawa.lispexpr;

import gnu.bytecode.*;
import gnu.lists.Sequences;
import java.util.List;

/** This matches a sequences of a specific length.
 * This is used for {@code [pat1 ... panN]} patterns.
 */

public class SeqSizeType extends LangObjType {
    int requiredSize;
    boolean requiredExact;

    public SeqSizeType(String name, int requiredSize, boolean requiredExact, String implClass) {
        super(name, implClass, -1);
        this.requiredSize = requiredSize;
        this.requiredExact = requiredExact;
    }

    public SeqSizeType(int requiredSize, boolean requiredExact) {
        this((requiredExact ? "list#=" : "list#>=") + requiredSize,
             requiredSize, requiredExact, "java.util.List");
    }

    @Override
    public Object coerceFromObject (Object obj) {
        List list = (List) Sequences.coerceToSequence(obj);
        int size = list.size();
        if (requiredExact ? size == requiredSize : size >= requiredSize)
            return list;
        throw new ClassCastException();
    }

    public static void checkSizeEq (java.util.List list, int requiredSize) {
        int sz = list.size();
        if (sz != requiredSize)
            throw new ClassCastException("sequence has size "+sz+" should be "+requiredSize);
    }

    public static void checkSizeGe (java.util.List list, int requiredSize) {
        int sz = list.size();
        if (sz < requiredSize)
            throw new ClassCastException("sequence has size "+sz+" should be at least "+requiredSize);
    }

    public static java.util.List coerceEq(Object object, int requiredSize) {
        List list = Sequences.coerceToSequence(object);
        checkSizeEq(list, requiredSize);
        return list;        
    }

    public static java.util.List coerceGe(Object object, int requiredSize) {
        List list = Sequences.coerceToSequence(object);
        checkSizeGe(list, requiredSize);
        return list;        
    }
    public static java.util.List coerceEqOrNull(Object object, int requiredSize) {
        List list;
        if (object instanceof List) {
            list = (List) object;
        } else {
            list = Sequences.asSequenceOrNull(object);
            if (list == null)
                return null;
        }
        return list.size() == requiredSize ? list : null;
    }

    public static java.util.List coerceGeOrNull(Object object, int requiredSize) {
        List list;
        if (object instanceof List) {
            list = (List) object;
        } else {
            list = Sequences.asSequenceOrNull(object);
            if (list == null)
                return null;
        }
        return list.size() >= requiredSize ? list : null;
    }
 
    @Override
    public void emitCoerceFromObject (CodeAttr code) {
        code.emitPushInt(requiredSize);
        ClassType thisCl = ClassType.make("gnu.kawa.lispexpr.SeqSizeType");
        code.emitInvokeStatic(thisCl.getDeclaredMethod(requiredExact ? "coerceEq" : "coerceGe", 2));
    }

    public boolean emitCoercionOrNull(CodeAttr code) {
        ClassType thisCl = ClassType.make("gnu.kawa.lispexpr.SeqSizeType");
        code.emitPushInt(requiredSize);
        code.emitInvokeStatic(thisCl.getDeclaredMethod(requiredExact ? "coerceEqOrNull" : "coerceGeOrNull", 2));
        return true;
    }

    @Override
    public int isCompatibleWithValue(Type valueType) {
        return Type.isCompatibleWithValue(this, valueType);
    }

    @Override
    public boolean isInterface() { return false; }
   
    @Override
    public int compare(Type other) {
        if (other instanceof SeqSizeType) {
            SeqSizeType sother = (SeqSizeType) other;
            if (requiredSize != sother.requiredSize) {
                if ((requiredExact && sother.requiredExact)
                    || (requiredExact && requiredSize < sother.requiredSize)
                    || (sother.requiredExact
                        && requiredSize > sother.requiredSize)) {
                    return -3;
                }
            }
            if (getImplementationType() == sother.getImplementationType()) {
                if (requiredSize == sother.requiredSize
                    && requiredExact && sother.requiredExact)
                    return 0;
                if (requiredSize < sother.requiredSize && !requiredExact)
                    return 1;
                if (requiredSize > sother.requiredSize && !sother.requiredExact)
                    return -1;
            }
        }
        int r = LangObjType.sequenceType.compare(other);
        return r == 0 || r == -1 ? -1 : r == -3 ? -3 : -2;
    }
}
