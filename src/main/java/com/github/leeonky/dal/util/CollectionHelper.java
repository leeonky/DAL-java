package com.github.leeonky.dal.util;

import java.util.Iterator;
import java.util.function.Function;

public class CollectionHelper {
    public static <T, R> Iterable<R> map(Iterable<? extends T> iterable, Function<? super T, ? extends R> mapper) {
        return () -> new Iterator<R>() {
            final Iterator<? extends T> iterator = iterable.iterator();

            @Override
            public boolean hasNext() {
                return iterator.hasNext();
            }

            @Override
            public R next() {
                return mapper.apply(iterator.next());
            }
        };
    }
}
