package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import static java.lang.String.format;

public abstract class DataList<E> implements Iterable<IndexedElement<E>> {
    public abstract int size();

    public E getByIndex(int index) {
        try {
            if (index < 0)
                return getByPosition(size() + index);
            return getByPosition(index - firstIndex());
        } catch (IndexOutOfBoundsException e) {
            throw new IndexOutOfBoundsException(format("Index out of bounds (%d), first index is: %d",
                    index, firstIndex()));
        }
    }

    protected int firstIndex() {
        return 0;
    }

    protected abstract E getByPosition(int index);
}
