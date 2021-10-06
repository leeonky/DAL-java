package com.github.leeonky.dal.runtime;

public interface ListAccessor<T> {
    Iterable<?> toIterable(T instance);
}
