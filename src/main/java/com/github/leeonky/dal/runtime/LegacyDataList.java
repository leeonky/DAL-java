package com.github.leeonky.dal.runtime;

import java.util.Comparator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static java.util.stream.Collectors.toList;

@Deprecated
public class LegacyDataList extends DataList<Object, Object> {
    private final ListAccessor<Object> listAccessor;
    private final int firstIndex;

    public LegacyDataList(ListAccessor<Object> listAccessor, Object list, Comparator<Object> listComparator) {
        super(list, listComparator);
        this.listAccessor = listAccessor;
        firstIndex = listAccessor.firstIndex(list);
    }

    @Override
    public Stream<Object> stream() {
        return (Stream<Object>) StreamSupport.stream(listAccessor.toIterable(list).spliterator(), false).sorted(comparator);
    }

    @Override
    public int firstIndex() {
        return firstIndex;
    }

    @Override
    protected Object getByPosition(int index) {
        return indexedStream().collect(toList()).get(index).value();
    }
}