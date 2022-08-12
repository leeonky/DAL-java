package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Converter;
import com.github.leeonky.util.NumberType;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.stream.Collectors;

import static com.github.leeonky.util.Suppressor.get;

class StaticCurryingMethod extends InstanceCurryingMethod {
    public StaticCurryingMethod(Object instance, Method method, Converter converter) {
        super(instance, method, converter);
    }

    @Override
    protected InstanceCurryingMethod clone() {
        return new StaticCurryingMethod(instance, method, converter);
    }

    @Override
    protected int parameterOffset() {
        return 1;
    }

    @Override
    public Object resolve() {
        return get(() -> method.invoke(null, parameterValues.stream().map(parameterValue -> parameterValue.getArg(converter))
                .collect(Collectors.toCollection(() -> new ArrayList<Object>() {{
                    add(instance);
                }})).toArray()));
    }

    @Override
    public boolean isSameInstanceType() {
        return method.getParameters()[0].getType().equals(NumberType.boxedClass(instance.getClass()));
    }
}
