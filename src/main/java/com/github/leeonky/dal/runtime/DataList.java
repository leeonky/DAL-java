package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.*;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static java.lang.String.format;

public abstract class DataList<E> implements Iterable<IndexedElement<E>> {
    private final Iterator<E> iterator;
    private final List<E> cached = new ArrayList<>();

    public DataList(Comparator<E> comparator, Iterator<E> iterator) {
        this.iterator = StreamSupport.stream(Spliterators.spliteratorUnknownSize(iterator, 0), false)
                .sorted(comparator).iterator();
    }

    @Deprecated
    public Stream<IndexedElement<E>> indexedStream() {
        return StreamSupport.stream(spliterator(), false);
    }

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

    protected Iterator<E> innerIterator() {
        return iterator;
    }

    @Override
    public Iterator<IndexedElement<E>> iterator() {
        return new Iterator<IndexedElement<E>>() {
            private int index = firstIndex();
            private int position = 0;

            @Override
            public boolean hasNext() {
                if (position < cached.size()) {
                    return true;
                }
                return innerIterator().hasNext();
            }

            @Override
            public IndexedElement<E> next() {
                if (position < cached.size())
                    return new IndexedElement<>(index++, cached.get(position++));
                position++;
                E next = innerIterator().next();
                cached.add(next);
                return new IndexedElement<>(index++, next);
            }
        };
    }

    public int size() {
        return (int) StreamSupport.stream(spliterator(), false).count();
    }

    protected abstract E getByPosition(int index);
}
