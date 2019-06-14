package com.github.leeonky.dal.util;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

public class TypeData<T> extends LinkedHashMap<Class<?>, T> {

    public Optional<T> getData(Object object) {
        return entrySet().stream()
                .filter(e -> e.getKey().isInstance(object))
                .findFirst()
                .map(Map.Entry::getValue);
    }

    public boolean containsType(Object object) {
        return entrySet().stream()
                .anyMatch(e -> e.getKey().isInstance(object));
    }
}
