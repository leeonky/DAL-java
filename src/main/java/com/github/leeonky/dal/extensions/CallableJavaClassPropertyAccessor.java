package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.runtime.Callable;
import com.github.leeonky.dal.runtime.JavaClassPropertyAccessor;
import com.github.leeonky.util.BeanClass;

import java.util.Collections;
import java.util.Set;

class CallableJavaClassPropertyAccessor extends JavaClassPropertyAccessor<Callable> {
    public CallableJavaClassPropertyAccessor() {
        super(BeanClass.create(Callable.class));
    }

    @Override
    public Set<Object> getPropertyNames(Callable callable) {
        return Collections.emptySet();
    }

    @Override
    public Object getValue(Callable callable, Object property) {
        return callable.apply(property);
    }
}
