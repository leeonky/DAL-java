package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Converter;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Set;

public interface CurryingMethod {
    static InstanceCurryingMethod createCurryingMethod(Object instance, Method method, Converter converter) {
        if (Modifier.isStatic(method.getModifiers()))
            return new StaticCurryingMethod(instance, method, converter);
        return new InstanceCurryingMethod(instance, method, converter);
    }

    CurryingMethod call(Object arg);

    Object resolve();

    Set<Object> fetchArgRange(RuntimeContextBuilder runtimeContextBuilder);

    Object convertToArgType(Object obj);
}
