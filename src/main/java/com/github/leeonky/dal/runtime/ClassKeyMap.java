package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Classes;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import static java.util.Map.Entry.comparingByKey;

public class ClassKeyMap<T> {
    private final LinkedHashMap<Class<?>, T> map = new LinkedHashMap<>();

    public Optional<T> tryGetData(Object object) {
        if (object == null)
            return Optional.empty();
        T data = map.get(object.getClass());
        if (data != null)
            return Optional.of(data);
        return map.entrySet().stream().filter(e -> e.getKey().isInstance(object))
                .sorted(comparingByKey(Classes::compareByExtends))
                .map(Map.Entry::getValue).findFirst();
    }

    public boolean containsType(Object object) {
        return tryGetData(object).isPresent();
    }

    public T getData(Object instance) {
        return tryGetData(instance).orElseThrow(IllegalArgumentException::new);
    }

    public T put(Class<?> type, T value) {
        return map.put(type, value);
    }

    public T get(Class<?> type, Supplier<T> defaultFactory) {
        T result = map.get(type);
        if (result == null) {
            result = defaultFactory.get();
            map.put(type, result);
        }
        return result;
    }
}
