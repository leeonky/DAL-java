package com.github.leeonky.dal.runtime;

public interface ListAccessor<T> {
    Iterable<?> toIterable(T instance);

    default int firstIndex() {
        return 0;
    }

    default boolean isList(T instance) {
        return true;
    }
}
