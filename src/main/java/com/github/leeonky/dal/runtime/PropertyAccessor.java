package com.github.leeonky.dal.runtime;

import java.util.Set;

public interface PropertyAccessor<T> {

    @SuppressWarnings("unchecked")
    default Object getValueByData(Data data, String name) {
        return getValue((T) data.getInstance(), name);
    }

    Object getValue(T instance, String name);

    Set<String> getPropertyNames(T instance);

    boolean isNull(T instance);
}
