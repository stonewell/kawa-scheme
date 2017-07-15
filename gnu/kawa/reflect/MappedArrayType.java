package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.mapping.Procedure;
import static  gnu.kawa.lispexpr.LangObjType.sequenceType;

/** An array type where elements are copied from a sequence.
 * A MappedArrayType[T] is implemented using a T (native) array.
 * It is compatible with a sequence whose elements are compatible with T.
 * Each element is copied from the sequence into the array.
 */

public class MappedArrayType extends ObjectType implements TypeValue {
    Type elementType;
    protected ObjectType implementationType;

    static ClassType utilType = ClassType.make("gnu.kawa.functions.MakeSplice");
    static Method coundiftMethod = utilType.getDeclaredMethod("count", 1);

    public MappedArrayType(Type elementType) {
        this.elementType = elementType;
        this.implementationType =
            ArrayType.make(elementType.getImplementationType());
        this.setSignature(implementationType.getSignature());
    }

    public Type getComponentType() { return elementType; }

    @Override
    public Type getImplementationType() { return implementationType; }
    @Override
    public Type getRealType() { return implementationType; }

    public void emitTestIf(Variable incoming, Declaration decl,
                           Compilation comp) {
        emitTestCoerce(true, incoming, decl, comp, comp.getCode());
    }

    void emitTestCoerce(boolean testing,
                        Variable incoming, Declaration decl,
                        Compilation comp, CodeAttr code) {
        Scope scope = code.pushScope();
        Label failureLabel = testing ? code.emitIfRaw() : null;
        Variable sizeVar = scope.addVariable(code, Type.intType, null);
        if (incoming == null) {
            incoming = scope.addVariable(code, Type.objectType, null);
            code.emitStore(incoming);
        }
        code.emitLoad(incoming);
        code.emitInvoke(countMethod);
        code.emitStore(sizeVar);
        Variable incomingElementVar = scope.addVariable(code, Type.objectType, null);
        Variable elementVar = scope.addVariable(code, elementType, null);
        Declaration elementDecl = testing ? new Declaration(elementVar) : null;
        Variable indexVar = scope.addVariable(code, Type.intType, null);
        code.emitPushInt(0);
        code.emitStore(indexVar);
        Variable arrayVar = scope.addVariable(code,
                                             ArrayType.make(elementType), null);
        code.emitLoad(sizeVar);
        code.emitNewArray(elementType.getRawType());
        code.emitStore(arrayVar);
        ClassType iteratorClass = ClassType.make("java.util.Iterator");
        Method getIteratorMethod = ClassType.make("gnu.lists.Sequences")
            .getDeclaredMethod("getIterator", 1);
        code.emitLoad(incoming);
        code.emitInvoke(getIteratorMethod);
        Variable iteratorVar = scope.addVariable(code, iteratorClass, null);
        code.emitStore(iteratorVar);
        Label topLabel = new Label(code);
        topLabel.define(code);
        code.emitLoad(iteratorVar);
        code.emitInvoke(iteratorClass.getDeclaredMethod("hasNext", 0));
        code.emitIfIntNotZero();
        code.emitLoad(iteratorVar);
        code.emitInvoke(iteratorClass.getDeclaredMethod("next", 0));
        code.emitStore(incomingElementVar);
        if (testing && elementType instanceof TypeValue) {
            ((TypeValue) elementType).emitTestIf(incomingElementVar, elementDecl, comp);
        } else {
            if (testing) {
                code.emitLoad(incomingElementVar);
                elementType.emitIsInstance(code);
                code.emitIfIntNotZero();
            }
            code.emitLoad(incomingElementVar);
            elementType.emitCoerceFromObject(code);
            code.emitStore(elementVar);
        }
        code.emitLoad(arrayVar);
        code.emitLoad(indexVar);
        code.emitLoad(elementVar);
        code.emitArrayStore();
        code.emitInc(indexVar, (short) 1);
        code.emitGoto(topLabel);
        if (testing) {
            code.emitElse();
            code.emitGoto(failureLabel);
            code.emitFi();
        }
        code.emitFi();
        code.emitLoad(arrayVar);
        if (testing) {
            decl.compileStore(comp);
        }
        code.popScope();
    }

    public String toString() {
        return "scan-array["+elementType+"]";
    }
    public String getName() {
        return "scan-array-"+elementType;
    }

    @Override
    public int isCompatibleWithValue(Type valueType) {
        return sequenceType.isCompatibleWithValue(valueType);
    }

    @Override
    public Expression convertValue(Expression value) { return null; }
    @Override
    public Procedure getConstructor() { return null; }
    @Override
    public void emitIsInstance(Variable incoming,
                               Compilation comp, Target target) {
        InstanceOf.emitIsInstance(this, incoming, comp, target);
    };

    public String encodeType(Language language) {
        String etype = language.encodeType(elementType);
        if (etype == null)
            etype = elementType.getName();
        return "$scan-array$["+etype+"]";
    }

    public void emitCoerceFromObject (CodeAttr code) {
        //code.emitCheckcast(this); // FIXME
        emitTestCoerce(false, null, null, null, code);
    }
}
