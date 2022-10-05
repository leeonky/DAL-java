package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.runtime.CurryingMethod;
import com.github.leeonky.dal.runtime.JavaClassPropertyAccessor;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.util.BeanClass;

import java.util.Set;

class CurryingMethodPropertyAccessor extends JavaClassPropertyAccessor<CurryingMethod> {

    private final RuntimeContextBuilder runtimeContextBuilder;

    public CurryingMethodPropertyAccessor(RuntimeContextBuilder runtimeContextBuilder) {
        super(BeanClass.create(CurryingMethod.class));
        this.runtimeContextBuilder = runtimeContextBuilder;
    }

    @Override
    public Object getValue(CurryingMethod curryingMethod, Object property) {
        return curryingMethod.call(property).resolve();
    }

    @Override
    public Set<Object> getPropertyNames(CurryingMethod curryingMethod) {
        return curryingMethod.fetchArgRange(runtimeContextBuilder);
    }
}
