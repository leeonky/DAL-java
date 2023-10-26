package com.github.leeonky.dal.runtime;

import java.util.Comparator;

public interface DALCollectionFactory<T, E> {
    default boolean isList(T instance) {
        return true;
    }

    DALCollection<E> create(T instance, Comparator<E> comparator);
}
