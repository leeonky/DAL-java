package com.github.leeonky.dal.util;

import java.util.Iterator;

public interface ArrayAccessor<T> extends ListAccessor<T> {
    Object get(T t, int index);

    int size(T t);

    @Override
    default Iterable<Object> toIterable(T instance) {
        return new Iterable<Object>() {
            private final int length = size(instance);
            private int index = 0;

            @Override
            public Iterator<Object> iterator() {
                return new Iterator<Object>() {

                    @Override
                    public boolean hasNext() {
                        return index < length;
                    }

                    @Override
                    public Object next() {
                        return get(instance, index++);
                    }
                };
            }
        };
    }
}
