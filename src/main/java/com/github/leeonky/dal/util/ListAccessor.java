package com.github.leeonky.dal.util;

public interface ListAccessor<T> {
    Iterable<Object> toIterable(T instance);
}
