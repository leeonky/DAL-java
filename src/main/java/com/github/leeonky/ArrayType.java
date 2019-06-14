package com.github.leeonky;

public interface ArrayType<T> {
    Object get(T t, int index);

    int size(T t);
}
