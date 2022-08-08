package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Converter;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Set;

public interface CurryingMethod {
    static CurryingMethod createCurryingMethod(Object instance, Method method) {
        if (Modifier.isStatic(method.getModifiers()))
            return new StaticCurryingMethod(instance, method);
        return new InstanceCurryingMethod(instance, method);
    }

    CurryingMethod call(Object arg, Converter converter);

    Object resolve();

    Set<Object> fetchArgRange(RuntimeContextBuilder runtimeContextBuilder);
}
