package com.github.leeonky.dal.format;

import com.github.leeonky.dal.runtime.IllegalTypeException;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

public abstract class BaseFormatter<T, R> implements Formatter<T, R> {
    private final Class<T> inputType;

    @SuppressWarnings("unchecked")
    protected BaseFormatter() {
        inputType = (Class<T>) guessInputType(getClass().getGenericSuperclass());
    }

    public static Class<?> guessInputType(Type type) {
        if (type instanceof ParameterizedType) {
            ParameterizedType parameterizedType = (ParameterizedType) type;
            if (parameterizedType.getRawType().equals(BaseFormatter.class))
                return (Class<?>) parameterizedType.getActualTypeArguments()[0];
            else
                return guessInputType(parameterizedType.getRawType());
        } else
            return guessInputType(((Class) type).getGenericSuperclass());
    }

    public static <T, R> R toValueOrThrowIllegalTypeException(T arg, ParseBlock<T, R> parseBlock) {
        try {
            return parseBlock.run(arg);
        } catch (Exception e) {
            throw new IllegalTypeException();
        }
    }

    @Override
    public boolean isValidType(Object input) {
        return inputType.isInstance(input);
    }

    @FunctionalInterface
    public
    interface ParseBlock<T2, R2> {
        R2 run(T2 t) throws Exception;
    }
}
