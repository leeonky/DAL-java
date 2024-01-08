package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class CollectionDALCollection<E> extends DALCollectionBase<E> {
    private final List<E> list;

    public CollectionDALCollection(Collection<E> collection) {
        list = new ArrayList<>(collection);
    }

    @Override
    public int size() {
        return list.size();
    }

    @Override
    protected E getByPosition(int position) {
        return list.get(position);
    }

    @Override
    public Iterator<IndexedElement<E>> iterator() {
        return new Iterator<IndexedElement<E>>() {
            int index = 0;

            @Override
            public boolean hasNext() {
                return index < list.size();
            }

            @Override
            public IndexedElement<E> next() {
                return new IndexedElement<>(index + firstIndex(), list.get(index++));
            }
        };
    }

    @Override
    public List<E> collect() {
        return list;
    }

    @Override
    public CollectionDALCollection<E> filter(Predicate<E> predicate) {
        return new CollectionDALCollection<E>(list.stream().filter(predicate).collect(Collectors.toList())) {
            @Override
            public int firstIndex() {
                return CollectionDALCollection.this.firstIndex();
            }
        };
    }
}
