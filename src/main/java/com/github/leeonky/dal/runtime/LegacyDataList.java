package com.github.leeonky.dal.runtime;

import java.util.Comparator;
import java.util.Iterator;

import static java.util.stream.Collectors.toList;

@Deprecated
public class LegacyDataList extends DataList<Object> {
    private final int firstIndex;

    public LegacyDataList(ListAccessor<Object> listAccessor, Object list, Comparator<Object> listComparator,
                          Iterator<Object> iterator) {
        super(listComparator, iterator);
        firstIndex = listAccessor.firstIndex(list);
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