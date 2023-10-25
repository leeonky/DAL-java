package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.Iterator;
import java.util.function.Function;

public class AutoMappingList extends DataList<Object> {
    private final Function<Data, Object> mapper;
    private final DataList<Object> dataList;
    private final Data data;

    public AutoMappingList(Function<Data, Object> mapper, Data data) {
        this.mapper = mapper;
        dataList = data.dataList();
        this.data = data;
    }

    @Override
    public int size() {
        return dataList.size();
    }

    @Override
    protected Object getByPosition(int position) {
        int index = position + firstIndex();
        return applyMapper(data.getValue(index), index);
    }

    @Override
    public Iterator<IndexedElement<Object>> iterator() {
        Iterator<IndexedElement<Data>> iterator = data.iterator();
        return new Iterator<IndexedElement<Object>>() {

            @Override
            public boolean hasNext() {
                return iterator.hasNext();
            }

            @Override
            public IndexedElement<Object> next() {
                IndexedElement<Data> next = iterator.next();
                return new IndexedElement<>(next.index(), applyMapper(next.value(), next.index()));
            }
        };
    }

    private Object applyMapper(Data data, int index) {
        try {
            return mapper.apply(data);
        } catch (PropertyAccessException e) {
            throw new ElementAccessException(index, e);
        } catch (Exception e) {
            throw new ElementAccessException(index, new PropertyAccessException(e.getMessage(), e));
        }
    }

    @Override
    protected int firstIndex() {
        return dataList.firstIndex();
    }
}
