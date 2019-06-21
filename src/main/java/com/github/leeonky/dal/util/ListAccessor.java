package com.github.leeonky.dal.util;

import java.util.Iterator;

public interface ListAccessor<T> {
    Object get(T t, int index);

    int size(T t);

    default Iterable toIterable(T instance) {
        return new Iterable() {
            private final int length = size(instance);
            private int index = 0;

            @Override
            public Iterator iterator() {
                return new Iterator() {

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
