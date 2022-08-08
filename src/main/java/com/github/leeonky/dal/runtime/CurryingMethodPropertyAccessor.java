package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.BeanClass;

import java.util.Set;

class CurryingMethodPropertyAccessor extends JavaClassPropertyAccessor<CurryingMethod> {

    public CurryingMethodPropertyAccessor(RuntimeContextBuilder runtimeContextBuilder) {
        super(runtimeContextBuilder, BeanClass.create(CurryingMethod.class));
    }

    @Override
    public Object getValue(CurryingMethod curryingMethod, Object property) {
        return curryingMethod.call(property, runtimeContextBuilder.getConverter()).resolve(runtimeContextBuilder.getConverter());
    }

    @Override
    public Set<Object> getPropertyNames(CurryingMethod curryingMethod) {
        return curryingMethod.fetchArgRange(runtimeContextBuilder);
    }
}
