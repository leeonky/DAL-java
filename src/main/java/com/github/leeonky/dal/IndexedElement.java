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

    public <R> IndexedElement<R> map(Mapper<? super T, ? extends R> mapper) {
        return new IndexedElement<>(index, mapper.apply(index, value));
    }

    @FunctionalInterface
    public interface Mapper<E, R> {
        R apply(int index, E e);
    }
}
