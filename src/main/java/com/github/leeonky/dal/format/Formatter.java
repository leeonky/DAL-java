package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

import java.lang.reflect.ParameterizedType;
import java.util.stream.Stream;

public interface Formatter<T> {
    default boolean isValidType(Object input) {
        return Stream.of(getClass().getGenericInterfaces())
                .filter(ParameterizedType.class::isInstance)
                .map(ParameterizedType.class::cast)
                .filter(c -> c.getRawType().equals(Formatter.class))
                .map(p -> p.getActualTypeArguments()[0])
                .filter(Class.class::isInstance)
                .map(Class.class::cast)
                .map(c -> c.isInstance(input))
                .findFirst()
                .orElseThrow(IllegalTypeException::new);
    }

    Object toValue(T input);

    default boolean isValidValue(T value) {
        if (isValidType(value)) {
            try {
                toValue(value);
                return true;
            } catch (IllegalTypeException ignore) {
            }
        }
        return false;
    }

    default String getFormatterName() {
        return getClass().getSimpleName().replaceFirst("^Formatter", "");
    }
}
