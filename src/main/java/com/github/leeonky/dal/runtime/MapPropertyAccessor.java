package com.github.leeonky.dal.runtime;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

class MapPropertyAccessor implements PropertyAccessor<Map<?, ?>> {
    @Override
    public Object getValue(Map<?, ?> instance, Object property) {
        return instance.get(property);
    }

    @Override
    public Set<Object> getPropertyNames(Map<?, ?> instance) {
        return new LinkedHashSet<>(instance.keySet());
    }

    @Override
    public boolean isNull(Map<?, ?> instance) {
        return instance == null;
    }
}
