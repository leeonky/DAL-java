package com.github.leeonky.dal.format;

import java.util.Objects;

public interface Type<T> {
    static <T> Type<T> equalTo(T expect) {
        return actual -> Objects.equals(expect, actual);
    }

    static <T> Type<T> nullReference() {
        return Objects::isNull;
    }

    boolean verify(T value);
}
