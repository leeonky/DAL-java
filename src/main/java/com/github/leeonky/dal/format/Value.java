package com.github.leeonky.dal.format;

import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.util.BeanClass;

import java.util.Objects;

//TODO formatter/ type / value refactor
public interface Value<T> {

    static <T> Value<T> equalTo(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.equals(value, actual);
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expecting field `%s` [%s] to be equal to [%s], but was not.", field, actual, value);
            }
        };
    }

    static <T> Value<T> nullReference() {
        return new Value<T>() {
            @Override
            public boolean verify(T actual) {
                return actual == null;
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expecting field `%s` [%s] to be null, but was not.", field, actual);
            }
        };
    }

    static <T extends Comparable<T>> Value<T> lessThan(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.requireNonNull(actual).compareTo(value) < 0;
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expecting field `%s` [%s] to be less than [%s], but was not.", field, actual, value);
            }
        };
    }

    static <T extends Comparable<T>> Value<T> greaterThan(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.requireNonNull(actual).compareTo(value) > 0;
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expecting field `%s` [%s] to be greater than [%s], but was not.", field, actual, value);
            }
        };
    }

    static <T extends Comparable<T>> Value<T> lessOrEqualTo(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.requireNonNull(actual).compareTo(value) <= 0;
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expecting field `%s` [%s] to be less or equal to [%s], but was not.", field, actual, value);
            }
        };
    }

    static <T extends Comparable<T>> Value<T> greaterOrEqualTo(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.requireNonNull(actual).compareTo(value) >= 0;
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expecting field `%s` [%s] to be greater or equal to [%s], but was not.", field, actual, value);
            }
        };
    }

    @SuppressWarnings("unchecked")
    default T convertAs(RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext, Object instance, BeanClass<?> type) {
        if (type == null)
            throw new IllegalFieldException();
        return (T) DALRuntimeContext.getConverter().tryConvert(type.getType(), instance);
    }

    boolean verify(T actual);

    default String errorMessage(String field, Object actual) {
        return String.format("Field `%s` is invalid", field);
    }
}
