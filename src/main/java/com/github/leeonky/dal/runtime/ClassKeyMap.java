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
                .findFirst().map(Map.Entry::getValue);
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
