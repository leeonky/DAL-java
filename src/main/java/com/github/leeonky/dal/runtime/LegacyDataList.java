package com.github.leeonky.dal.runtime;

import java.util.Comparator;

@Deprecated
public class LegacyDataList extends IterableDataList<Object> {
    private final int firstIndex;

    public LegacyDataList(ListAccessor<Object> listAccessor, Object list, Comparator<Object> listComparator,
                          Iterable<Object> iterable) {
        super(iterable, listComparator);
        firstIndex = listAccessor.firstIndex(list);
    }

    @Override
    public int firstIndex() {
        return firstIndex;
    }
}