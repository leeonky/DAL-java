package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.stream.StreamSupport;

public class IterableDataList<E> extends DataList<E> {
    private final Iterator<E> iterator;
    private final List<E> cached = new ArrayList<>();

    public IterableDataList(Iterable<E> iterable, Comparator<E> comparator) {
        iterator = StreamSupport.stream(iterable.spliterator(), false)
                .sorted(comparator).iterator();
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
                return iterator.hasNext();
            }

            @Override
            public IndexedElement<E> next() {
                if (position < cached.size())
                    return new IndexedElement<>(index++, cached.get(position++));
                position++;
                return new IndexedElement<>(index++, getNext());
            }
        };
    }

    private E getNext() {
        E next = iterator.next();
        cached.add(next);
        return next;
    }

    @Override
    protected E getByPosition(int position) {
        if (position < cached.size())
            return cached.get(position);
        while (iterator.hasNext()) {
            getNext();
            if (position < cached.size())
                return cached.get(position);
        }
        throw new IndexOutOfBoundsException();
    }

    @Override
    public int size() {
        return (int) StreamSupport.stream(spliterator(), false).count();
    }
}
