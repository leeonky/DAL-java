package com.github.leeonky.dal;

public class IndexedElement<T> {
    private final int index;
    private final T value;

    public IndexedElement(int index, T value) {
        this.index = index;
        this.value = value;
    }

    public int index() {
        return index;
    }

    public T value() {
        return value;
    }
}
