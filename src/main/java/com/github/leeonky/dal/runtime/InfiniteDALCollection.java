package com.github.leeonky.dal.runtime;

import java.util.Iterator;
import java.util.function.Supplier;

public class InfiniteDALCollection<E> extends IterableDALCollection<E> {
    public InfiniteDALCollection(Supplier<E> supplier) {
        super(() -> new Iterator<E>() {
            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public E next() {
                return supplier.get();
            }
        });
    }

    @Override
    public boolean infinite() {
        return true;
    }
}
