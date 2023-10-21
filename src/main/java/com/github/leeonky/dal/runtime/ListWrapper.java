package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.Comparator;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;
import static java.util.stream.StreamSupport.stream;

public class ListWrapper {
    private final ListAccessor<Object> listAccessor;
    private final Object list;
    private final Comparator<Object> listComparator;
    private final int firstIndex;

    public ListWrapper(ListAccessor<Object> listAccessor, Object list, Comparator<Object> listComparator) {
        this.listAccessor = listAccessor;
        this.list = list;
        this.listComparator = listComparator;
        firstIndex = listAccessor.firstIndex(list);
    }

    public int size() {
        return listAccessor.size(list);
    }


    @Deprecated
    public List<Object> listData() {
        return list().collect(toList());
    }

    @Deprecated
    public Stream<Object> list() {
        return (Stream<Object>) stream(listAccessor.toIterable(list).spliterator(), false).sorted(listComparator);
    }

    public Stream<IndexedElement<Object>> indexedList() {
        AtomicInteger index = new AtomicInteger(firstIndex);
        return stream(listAccessor.toIterable(list).spliterator(), false).sorted(listComparator)
                .map(o -> new IndexedElement<>(index.getAndIncrement(), o));
    }

    @Deprecated
    public Object getByPosition(int index) {
        return listData().get(index);
    }

    public Object getByIndex(int index) {
        if (index < 0)
            return getByPosition(size() + index);
        return getByPosition(index - firstIndex);
    }

    @Deprecated
    public int firstIndex() {
        return firstIndex;
    }

}