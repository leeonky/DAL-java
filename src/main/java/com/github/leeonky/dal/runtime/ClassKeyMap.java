package com.github.leeonky.dal.runtime;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

public class ClassKeyMap<T> extends LinkedHashMap<Class<?>, T> {

    public Optional<T> tryGetData(Object object) {
        if (object == null)
            return Optional.empty();
        T data = get(object.getClass());
        if (data != null)
            return Optional.of(data);
        return entrySet().stream().filter(e -> e.getKey().isInstance(object))
                .sorted(Map.Entry.comparingByKey(this::sortClass))
                .map(Map.Entry::getValue).findFirst();
    }

    private int sortClass(Class<?> c1, Class<?> c2) {
        return c1.isAssignableFrom(c2) ? 1 : -1;
    }

    public boolean containsType(Object object) {
        return tryGetData(object).isPresent();
    }

    public T getData(Object instance) {
        return tryGetData(instance).orElseThrow(IllegalArgumentException::new);
    }

    @Override
    public T put(Class<?> key, T value) {
        return super.put(key, value);
    }
}
