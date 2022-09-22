package com.github.leeonky.dal.format;

import com.github.leeonky.util.function.Comparator;

import java.util.Objects;

import static java.lang.String.format;

public interface Type<T> {
    static <T> Type<T> equalTo(T expect) {
        return new Type<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.equals(expect, actual);
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return format("Expecting field `%s` [%s] to be equal to [%s], but was not.", field, actual, expect);
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
                return format("Expecting field `%s` [%s] to be null, but was not.", field, actual);
            }
        };
    }

    static <T extends Comparable<T>> Type<T> lessThan(T value) {
        return compare(value, Comparator.lessThan(0), "less than");
    }

    static <T extends Comparable<T>> Type<T> greaterThan(T value) {
        return compare(value, Comparator.greaterThan(0), "greater than");
    }

    static <T extends Comparable<T>> Type<T> lessOrEqualTo(T value) {
        return compare(value, Comparator.lessOrEqualTo(0), "less or equal to");
    }

    static <T extends Comparable<T>> Type<T> greaterOrEqualTo(T value) {
        return compare(value, Comparator.greaterOrEqualTo(0), "greater or equal to");
    }

    static <T extends Comparable<T>> Type<T> compare(T value, Comparator<Integer> comparator, String message) {
        return new ComparableType<>(comparator, value, message);
    }

    boolean verify(T value);

    String errorMessage(String field, Object actual);

    class ComparableType<T extends Comparable<T>> implements Type<T> {
        private final Comparator<Integer> comparator;
        private final T value;
        private final String message;

        public ComparableType(Comparator<Integer> comparator, T value, String message) {
            this.comparator = comparator;
            this.value = value;
            this.message = message;
        }

        @Override
        public boolean verify(T actual) {
            return comparator.compareTo(Objects.requireNonNull(actual).compareTo(value));
        }

        @Override
        public String errorMessage(String field, Object actual) {
            return format("Expecting field `%s` [%s] to be %s [%s], but was not.", field, actual, message, value);
        }
    }
}
