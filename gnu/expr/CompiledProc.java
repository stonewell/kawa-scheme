// Copyright (c) 2015  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import java.lang.reflect.*;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #endif */

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

    // FIXME duplicates code from Procedure class
    public Object apply0() throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApply(this);
        return ctx.runUntilValue();
    }

    public Object apply1(Object arg1) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApply(this, arg1);
        return ctx.runUntilValue();
    }

    public Object apply2(Object arg1,Object arg2) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApply(this, arg1, arg2);
        return ctx.runUntilValue();
    }

    public Object apply3(Object arg1, Object arg2,
                         Object arg3) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApply(this, arg1, arg2, arg3);
        return ctx.runUntilValue();
    }

    public Object apply4(Object arg1, Object arg2,
                         Object arg3, Object arg4) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApply(this, arg1, arg2, arg3, arg4);
        return ctx.runUntilValue();
    }

    public Object applyN(Object[] args) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        ctx.setupApplyAll(this, args);
        return ctx.runUntilValue();
    }

    // FIXME missing functionality of ModuleMethod.resolveParameterTypes

    public int numArgs() { return numArgs; }
}
