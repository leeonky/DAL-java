package com.github.leeonky.dal.format;

import java.util.Objects;

public interface Type<T> {
    static <T> Type<T> equalTo(T expect) {
        return new Type<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.equals(expect, actual);
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expect field `%s` [%s] to be equal to [%s], but was not.", field, actual, expect);
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
                return String.format("Expect field `%s` [%s] to be null, but was not.", field, actual);
            }
        };
    }

    static <T extends Comparable<T>> Type<T> lessThan(T value) {
        return new Type<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.requireNonNull(actual).compareTo(value) < 0;
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expect field `%s` [%s] to be less than [%s], but was not.", field, actual, value);
            }
        };
    }

    static <T extends Comparable<T>> Type<T> greaterThan(T value) {
        return new Type<T>() {

            @Override
            public boolean verify(T actual) {
                return Objects.requireNonNull(actual).compareTo(value) > 0;
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expect field `%s` [%s] to be greater than [%s], but was not.", field, actual, value);
            }
        };
    }

    static <T extends Comparable<T>> Type<T> lessOrEqualTo(T value) {
        return new Type<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.requireNonNull(actual).compareTo(value) <= 0;
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expect field `%s` [%s] to be less or equal to [%s], but was not.", field, actual, value);
            }
        };
    }

    static <T extends Comparable<T>> Type<T> greaterOrEqualTo(T value) {
        return new Type<T>() {
            @Override
            public boolean verify(T actual) {
                return Objects.requireNonNull(actual).compareTo(value) >= 0;
            }

            @Override
            public String errorMessage(String field, Object actual) {
                return String.format("Expect field `%s` [%s] to be greater or equal to [%s], but was not.", field, actual, value);
            }
        };
    }

    boolean verify(T value);

    String errorMessage(String field, Object actual);
}
