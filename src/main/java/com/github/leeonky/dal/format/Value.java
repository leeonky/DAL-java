package com.github.leeonky.dal.format;

import com.github.leeonky.dal.IllegalFieldException;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.util.BeanClass;

import java.util.Objects;

public class Value<T> {

    public static <T> Value<T> equalTo(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T instance) {
                return Objects.equals(value, instance);
            }
        };
    }

    public static <T> Value<T> nullReference() {
        return equalTo(null);
    }

    public static <T extends Comparable<T>> Value<T> lessThan(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T instance) {
                return Objects.requireNonNull(instance).compareTo(value) < 0;
            }
        };
    }

    public static <T extends Comparable<T>> Value<T> greaterThan(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T instance) {
                return Objects.requireNonNull(instance).compareTo(value) > 0;
            }
        };
    }

    public static <T extends Comparable<T>> Value<T> lessOrEqualTo(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T instance) {
                return Objects.requireNonNull(instance).compareTo(value) <= 0;
            }
        };
    }

    public static <T extends Comparable<T>> Value<T> greaterOrEqualTo(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T instance) {
                return Objects.requireNonNull(instance).compareTo(value) >= 0;
            }
        };
    }

    public T convertAs(RuntimeContext runtimeContext, Object instance, BeanClass<?> type) {
        if (type == null)
            throw new IllegalFieldException();
        return (T) runtimeContext.getConverter().tryConvert(type.getType(), instance);
    }

    public boolean verify(T instance) {
        return true;
    }
}
