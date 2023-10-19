package com.github.leeonky.dal.runtime;

import java.util.function.Function;

public interface ListAccessor<T> {
    Iterable<?> toIterable(T instance);

    default int firstIndex(T instance) {
        return 0;
    }

    default boolean isList(T instance) {
        return true;
    }

    //    TODO refactor
    default int size(T instance) {
        int i = 0;
        for (Object object : toIterable(instance)) i++;
        return i;
    }

    static <T extends Iterable<?>> ListAccessor<T> changeFirstIndex(Function<T, Integer> indexFunction) {
        return new ListAccessor<T>() {
            @Override
            public Iterable<?> toIterable(T instance) {
                return instance;
            }

            @Override
            public int firstIndex(T instance) {
                return indexFunction.apply(instance);
            }
        };
    }
}
