package com.github.leeonky.dal.runtime;

import java.util.Comparator;

public interface DataListFactory<T, E> {
    default boolean isList(T instance) {
        return true;
    }

    DataList<E> create(T instance, Comparator<E> comparator);
}
