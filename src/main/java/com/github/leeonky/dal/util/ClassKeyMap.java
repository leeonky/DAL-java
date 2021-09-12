package com.github.leeonky.dal.util;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

public class ClassKeyMap<T> extends LinkedHashMap<Class<?>, T> {

    //TODO equal class first
    public Optional<T> tryGetData(Object object) {
        return entrySet().stream()
                .filter(e -> e.getKey().isInstance(object))
                .findFirst()
                .map(Map.Entry::getValue);
    }

    //TODO equal class first
    public boolean containsType(Object object) {
        return entrySet().stream()
                .anyMatch(e -> e.getKey().isInstance(object));
    }
}
