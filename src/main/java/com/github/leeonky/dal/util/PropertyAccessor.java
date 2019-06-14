package com.github.leeonky.dal.util;

import java.util.HashSet;
import java.util.Set;

public interface PropertyAccessor<T> {
    Object getValue(T instance, String name) throws Exception;

    default Set<String> getPropertyNames(T instance) {
        return new HashSet<>();
    }
}
