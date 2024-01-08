package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.StreamSupport;

public class IterableDALCollection<E> extends DALCollectionBase<E> {
    private final Iterator<E> iterator;
    private final List<E> cached = new ArrayList<>();

    public IterableDALCollection(Iterable<E> iterable) {
        iterator = StreamSupport.stream(iterable.spliterator(), false).iterator();
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
        return (int) StreamSupport.stream(
                requireLimitedCollection("Not supported for infinite collection").spliterator(), false).count();
    }

    @Override
    public DALCollection<E> filter(Predicate<E> predicate) {
        return new IterableDALCollection<E>(() -> Spliterators.iterator(StreamSupport.stream(
                Spliterators.spliteratorUnknownSize(iterator, Spliterator.ORDERED), false).filter(predicate).spliterator())) {

            @Override
            public int firstIndex() {
                return IterableDALCollection.this.firstIndex();
            }
        };
    }
}
