package com.github.leeonky.dal.runtime;

import java.util.Set;

public interface PropertyAccessor<T> {

    @SuppressWarnings("unchecked")
    default Object getValueByData(Data data, Object property) {
        return getValue((T) data.getInstance(), property);
    }

    Object getValue(T instance, Object property);

    Set<Object> getPropertyNames(T instance);

    boolean isNull(T instance);
}
