package com.github.leeonky.dal.runtime;

import java.util.Objects;
import java.util.Set;

public interface PropertyAccessor<T> {

    @SuppressWarnings("unchecked")
    default Object getValueByData(Data data, Object property) {
        return getValue((T) data.instance(), property);
    }

    Object getValue(T instance, Object property);

    Set<Object> getPropertyNames(T instance);

    default boolean isNull(T instance) {
        return Objects.equals(instance, null);
    }
}
