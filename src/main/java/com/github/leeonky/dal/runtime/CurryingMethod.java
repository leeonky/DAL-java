package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Suppressor;

import java.lang.reflect.Method;

public class CurryingMethod {
    private final Object instance;
    private final Method method;

    public CurryingMethod(Object instance, Method method) {
        this.instance = instance;
        this.method = method;
    }

    public Object call(Object arg) {
        return Suppressor.get(() -> method.invoke(instance, arg));
    }

    public CurryingMethod currying(Object property) {

        return new CurryingMethod(instance, method) {

            @Override
            public Object call(Object arg) {
                return Suppressor.get(() -> method.invoke(instance, property, arg));
            }
        };
    }
}
