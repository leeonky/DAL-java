package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.Iterator;
import java.util.function.Function;

public class AutoMappingList implements Iterable<Object> {
    private final int firstIndex;
    private final Function<Data, Object> mapper;
    private final Data data;

    public AutoMappingList(int firstIndex, Function<Data, Object> mapper, Data data) {
        this.firstIndex = firstIndex;
        this.mapper = mapper;
        this.data = data;
    }

    public int firstIndex() {
        return firstIndex;
    }

    @Override
    public Iterator<Object> iterator() {
        Iterator<IndexedElement<Data>> iterator = data.indexedListData().iterator();
        return new Iterator<Object>() {
            @Override
            public boolean hasNext() {
                return iterator.hasNext();
            }

            @Override
            public Object next() {
                IndexedElement<Data> indexedElement = iterator.next();
                try {
                    return mapper.apply(indexedElement.value());
                } catch (PropertyAccessException e) {
                    throw new ElementAccessException(indexedElement.index(), e);
                } catch (Exception e) {
                    throw new ElementAccessException(indexedElement.index(), new PropertyAccessException(e.getMessage(), e));
                }
            }
        };
    }
}
