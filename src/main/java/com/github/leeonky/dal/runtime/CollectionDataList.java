package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

public class CollectionDataList<E> extends DataList<E> {
    private final List<E> list;

    public CollectionDataList(Collection<E> collection, Comparator<E> comparator) {
        list = collection.stream().sorted(comparator).collect(Collectors.toList());
    }

    @Override
    public int size() {
        return list.size();
    }

    @Override
    protected E getByPosition(int index) {
        return list.get(index);
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

}
