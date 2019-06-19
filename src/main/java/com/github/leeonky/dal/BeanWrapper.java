package com.github.leeonky.dal;

import com.github.leeonky.dal.util.BeanUtil;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.dal.util.TypeData;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class BeanWrapper {
    private final Object instance;
    private final Optional<PropertyAccessor> propertyAccessor;
    private final TypeData<PropertyAccessor> propertyAccessors;

    public BeanWrapper(Object instance, TypeData<PropertyAccessor> propertyAccessors) {
        this.instance = instance;
        propertyAccessor = propertyAccessors.getData(instance);
        this.propertyAccessors = propertyAccessors;
    }

    @SuppressWarnings("unchecked")
    public Set<String> getPropertyNames() {
        return propertyAccessor
                .map(f -> f.getPropertyNames(instance))
                .orElseGet(() -> {
                    if (instance instanceof Map)
                        return ((Map) instance).keySet();
                    return BeanUtil.findPropertyNames(instance.getClass());
                });
    }

    public Object getPropertyValue(String name) {
        return instance instanceof Map ?
                ((Map) instance).get(name)
                : getPropertyFromType(name);
    }

    public Object getInstance() {
        return instance;
    }

    public BeanWrapper getPropertyValueWrapper(String name) {
        return new BeanWrapper(getPropertyValue(name), propertyAccessors);
    }

    @SuppressWarnings("unchecked")
    private Object getPropertyFromType(String name) {
        return propertyAccessor
                .map(p -> checkedReturn(() -> p.getValue(instance, name)))
                .orElseGet(() -> checkedReturn(() -> BeanUtil.getPropertyValue(instance, name)));
    }

    private Object checkedReturn(CheckedSupplier supplier) {
        try {
            return supplier.get();
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    interface CheckedSupplier {
        Object get() throws Exception;
    }
}
