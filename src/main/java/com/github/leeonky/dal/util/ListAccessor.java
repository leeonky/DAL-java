package com.github.leeonky.dal.util;

public interface ListAccessor<T> {
    Object get(T t, int index);

    int size(T t);
}
