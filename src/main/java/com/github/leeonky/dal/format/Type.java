package com.github.leeonky.dal.format;

import java.util.Objects;
import java.util.function.Function;

public interface Type<T> {
    static <T> Type<T> equalTo(T expect) {
        return new Type<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.equals(expect, actual);
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expecting field `%s` [%s] to be equal to [%s], but was not.", field, actual, expect);
            }
        };
    }

    static <T> Type<T> nullReference() {
        return new Type<T>() {
            @Override
            public boolean verify(T obj) {
                return Objects.isNull(obj);
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expecting field `%s` [%s] to be null, but was not.", field, actual);
            }
        };
    }

    static <T extends Comparable<T>> Type<T> lessThan(T value) {
        return compare(value, i -> i < 0, "less than");
    }

    static <T extends Comparable<T>> Type<T> compare(T value, Function<Integer, Boolean> comparator, String message) {
        return new Type<T>() {
            @Override
            public boolean verify(T actual) {
                return comparator.apply(Objects.requireNonNull(actual).compareTo(value));
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expecting field `%s` [%s] to be " + message + " [%s], but was not.", field, actual, value);
            }
        };
    }

    static <T extends Comparable<T>> Type<T> greaterThan(T value) {
        return compare(value, i -> i > 0, "greater than");
    }

    static <T extends Comparable<T>> Type<T> lessOrEqualTo(T value) {
        return compare(value, i -> i <= 0, "less or equal to");
    }

    static <T extends Comparable<T>> Type<T> greaterOrEqualTo(T value) {
        return compare(value, i -> i >= 0, "greater or equal to");
    }

    boolean verify(T value);

    String errorMessage(String field, Object actual);
}
