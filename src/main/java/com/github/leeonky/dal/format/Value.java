package com.github.leeonky.dal.format;

import com.github.leeonky.dal.RuntimeContext;

import java.util.Objects;

public class Value<T> {
    public static <T> Value<T> equalTo(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(RuntimeContext runtimeContext, Object instance) {
                return Objects.equals(value, convertAs(runtimeContext, instance, value));
            }
        };
    }

    //TODO value is null
    //TODO value is null but result value is int long
    public Object convertAs(RuntimeContext runtimeContext, Object instance, T value) {
        return runtimeContext.getConverter().tryConvert(value.getClass(), instance);
    }

    public boolean verify(RuntimeContext runtimeContext, Object instance) {
//        runtimeContext.getConverter().tryConvert()
        return true;
    }
}
