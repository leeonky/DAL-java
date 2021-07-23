package com.github.leeonky.dal.format;

import com.github.leeonky.dal.RuntimeContext;

import java.util.Objects;

public class Value<T> {

    public static <T> Value<T> equalTo(T value) {
        return new EqualTo<>(value);
    }

    public Object convertAs(RuntimeContext runtimeContext, Object instance, Class<?> target) {
        return runtimeContext.getConverter().tryConvert(target, instance);
    }

    public boolean verify(T instance) {
        return true;
    }

    public static class EqualTo<T> extends Value<T> {
        private final T value;

        protected EqualTo(T value) {
            this.value = value;
        }

        @Override
        public boolean verify(T instance) {
            return Objects.equals(value, instance);
        }
    }
}
