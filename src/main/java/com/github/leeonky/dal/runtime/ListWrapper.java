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
        if (list instanceof Stream) {
            this.list = ((Stream) list).collect(toList());
            this.listAccessor = instance -> ((List) instance);
        } else {
            this.list = list;
            this.listAccessor = listAccessor;
        }
        this.listComparator = listComparator;
        firstIndex = listAccessor.firstIndex(list);
    }

    public int size() {
        return listAccessor.size(list);
    }

    public Stream<IndexedElement<Object>> indexedList() {
        AtomicInteger index = new AtomicInteger(firstIndex);
        return stream(listAccessor.toIterable(list).spliterator(), false).sorted(listComparator)
                .map(o -> new IndexedElement<>(index.getAndIncrement(), o));
    }

    @Deprecated
    public Object getByPosition(int index) {
        return ((Stream<Object>) stream(listAccessor.toIterable(list).spliterator(), false).sorted(listComparator))
                .collect(toList()).get(index);
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