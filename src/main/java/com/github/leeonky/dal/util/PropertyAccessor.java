package com.github.leeonky.dal.util;

import java.util.Set;

public interface PropertyAccessor<T> {
    Object getValue(T instance, String name) throws Exception;

    Set<String> getPropertyNames(T instance);
}
