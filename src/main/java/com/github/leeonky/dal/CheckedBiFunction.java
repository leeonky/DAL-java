package com.github.leeonky.dal;

@FunctionalInterface
public interface CheckedBiFunction<T, U, R> {
    R apply(T t, U u) throws Exception;

    default T castToParameter0(Object object) {
        return (T) object;
    }
}
