package com.github.leeonky.dal.format;

import java.util.Objects;

public class Type<T> {
    public static <T> Type<T> equalTo(T expect) {
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

    public static <T> Type<T> nullReference() {
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

    public boolean verify(T value) {
        return true;
    }

    public String errorMessage(String field, Object actual) {
        return String.format("Field `%s` is invalid", field);
    }
}

//TODO > >= < <=
