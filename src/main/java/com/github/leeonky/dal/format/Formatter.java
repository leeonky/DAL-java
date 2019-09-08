package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

public interface Formatter<T, R> {

    R toValue(T input);

    boolean isValidType(Object input);

    default boolean isValidValue(T value) {
        if (isValidType(value)) {
            try {
                return verify(toValue(value));
            } catch (IllegalTypeException ignore) {
            }
        }
        return false;
    }

    default boolean verify(R value) {
        return true;
    }

    default String getFormatterName() {
        return getClass().getSimpleName().replaceFirst("^Formatter", "");
    }

}
