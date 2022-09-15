package com.github.leeonky.dal.format;

import com.github.leeonky.dal.runtime.IllegalTypeException;
import com.github.leeonky.util.BeanClass;

public interface Formatter<T, R> {

    @SuppressWarnings("unchecked")
    static Formatter<Object, Object> createFormatter(BeanClass<?> type) {
        return (Formatter<Object, Object>) type.getTypeArguments(0)
                .<Object>map(t -> type.newInstance((Object) t.getType())).orElseGet(type::newInstance);
    }

    R convert(T input);

    boolean isValidType(Object input);

    default boolean isValid(T value) {
        try {
            return isValidValue(transform(value));
        } catch (IllegalTypeException ignore) {
            return false;
        }
    }

    default boolean isValidValue(R value) {
        return true;
    }

    default String getFormatterName() {
        return getClass().getSimpleName().replaceFirst("^Formatter", "");
    }

    default R transform(T o) {
        if (isValidType(o))
            return convert(o);
        throw new IllegalTypeException();
    }
}
