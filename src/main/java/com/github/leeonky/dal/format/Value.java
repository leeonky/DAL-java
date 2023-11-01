package com.github.leeonky.dal.format;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.function.Comparator;

import java.util.Objects;

import static java.lang.String.format;

public interface Value<T> extends Type<T> {

    static <T> Value<T> equalTo(T value) {
        return new Value<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.equals(value, actual);
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return format("Expecting field `%s` [%s] to be equal to [%s], but was not.", field, actual, value);
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
                return format("Expecting field `%s` [%s] to be null, but was not.", field, actual);
            }
        };
    }

    static <T extends Comparable<T>> Value<T> lessThan(T value) {
        return compare(value, Comparator.lessThan(0), "less than");
    }

    static <T extends Comparable<T>> Value<T> greaterThan(T value) {
        return compare(value, Comparator.greaterThan(0), "greater than");
    }

    static <T extends Comparable<T>> Value<T> lessOrEqualTo(T value) {
        return compare(value, Comparator.lessOrEqualTo(0), "less or equal to");
    }

    static <T extends Comparable<T>> Value<T> greaterOrEqualTo(T value) {
        return compare(value, Comparator.greaterOrEqualTo(0), "greater or equal to");
    }

    static <T extends Comparable<T>> Value<T> compare(T value, Comparator<Integer> comparator, String valueName) {
        return new ComparableValue<>(comparator, value, valueName);
    }

    @Override
    boolean verify(T actual);

    @Override
    default String errorMessage(String field, Object actual) {
        return format("Field `%s` is invalid", field);
    }

    @SuppressWarnings("unchecked")
    default T convertAs(Data actual, BeanClass<?> type) {
        if (type == null)
            throw new IllegalFieldException();
        return (T) actual.convert(type.getType()).instance();
    }

    class ComparableValue<T extends Comparable<T>> extends ComparableType<T> implements Value<T> {
        public ComparableValue(Comparator<Integer> comparator, T value, String valueName) {
            super(comparator, value, valueName);
        }
    }
}
