// Copyright (c) 2015  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import java.lang.reflect.*;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.MethodHandle;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */
import kawa.SourceMethodType;

public class CompiledProc extends MethodProc {
    protected int numArgs;
    private Object module; // rename to frame? or context?

    public Object getModule() { return module; }

    public Class getModuleClass() {
        if (module == null || module instanceof Class)
            return (Class) module;
        else
            return module.getClass();
        // FIXME If Java 8 use MethodHandles.Lookup.revealDirect
        // to get MethodHandleInfo, and then getDeclaringClass.
    }

    public CompiledProc(Object module, boolean resultGoesToConsumer, MethodHandle applyMethod, Object name, int numArgs) {
        init(resultGoesToConsumer, applyMethod, name, numArgs);
        this.module = module;
    }

    public CompiledProc(boolean resultGoesToConsumer, MethodHandle applyMethod, Object name,
                        int numArgs, Object argTypes) {
        init(resultGoesToConsumer, applyMethod, name, numArgs);
        this.argTypes = argTypes;
    }

    public static CompiledProc makeResultToObject(Object module, MethodHandle applyMethod, Object name, int numArgs) {
        return new CompiledProc(module, false, applyMethod, name, numArgs);
    }

    public static CompiledProc makeResultToConsumer(Object module, MethodHandle applyMethod, Object name, int numArgs) {
        return new CompiledProc(module, true, applyMethod, name, numArgs);
    }

    public void initResultToObject(Object module, MethodHandle applyMethod, Object name, int numArgs) {
        init(false, applyMethod, name, numArgs);
        this.module = module;
    }

    public void initResultToConsumer(Object module, MethodHandle applyMethod, Object name, int numArgs) {
        init(true, applyMethod, name, numArgs);
        this.module = module;
    }

    public CompiledProc init(boolean resultGoesToConsumer,
                             MethodHandle applyMethod,
                             Object name, int numArgs)   {
        if (resultGoesToConsumer) {
            applyToConsumerMethod = applyMethod;
            applyToObjectMethod = applyToObjectDefault;
        } else {
            applyToObjectMethod = applyMethod;
            applyToConsumerMethod = applyToConsumerDefault;
        }
        this.numArgs = numArgs;
        if (name != null)
            setSymbol(name);
        return this;
    }

    /** Figure out parameter types.
     * Uses reflection to get method parameter types.
     * INCOMPLETE - does not handle procedures with optional or rest args,
     or with patterns. */
    protected void resolveParameterTypes() {
        Method method = null;
        String name = getName();
        if (name != null) {
            try {
                Class moduleClass = getModuleClass();
                Method[] methods = moduleClass.getDeclaredMethods();
                String mangledName = Mangling.mangleMethod(name);
                for (int i = methods.length;  --i >= 0; ) {
                    if (methods[i].getName().equals(mangledName)) {
                        if (method != null) {
                            method = null;
                            break;
                        }
                        method = methods[i];
                    }
                }
                if (method != null) {
                    Language lang = Language.getDefaultLanguage();
                    if (lang != null) {
                        Class[] parameterClasses = method.getParameterTypes();
                        int numParamTypes = parameterClasses.length;
                        gnu.bytecode.Type[] atypes = new gnu.bytecode.Type[numParamTypes];
                        String[] annotTypes;
                        try {
                            SourceMethodType sourceType = method.getAnnotation(SourceMethodType.class);
                            annotTypes = sourceType == null ? null : sourceType.value();
                        } catch (Throwable ex) {
                            annotTypes = null;
                        }
                        for (int i = numParamTypes;  --i >= 0; ) {
                            atypes[i] = lang.getTypeFor(parameterClasses[i]);
                            if (annotTypes != null)
                                atypes[i] =
                                    PrimProcedure.decodeType(atypes[i],
                                                             annotTypes, i+1,
                                                             null, lang);
                        }
                        this.argTypes = atypes;
                    }
                }
            } catch (Exception ex) {
            }
        }
        if (argTypes == null)
            super.resolveParameterTypes();
    }

    public int numArgs() { return numArgs; }
}
