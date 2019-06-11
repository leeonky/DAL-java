package com.github.leeonky.dal;

import com.github.leeonky.dal.token.IllegalTypeException;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;

public class CompilingContextBuilder {
    private final Map<Class<?>, CheckedBiFunction<?, String, Object>> propertyAccessors = new LinkedHashMap<>();
    private final Map<String, Function<Object, Object>> types = new LinkedHashMap<>();

    private static boolean isGetter(Method m) {
        if (m.getParameters().length == 0) {
            if (m.getName().startsWith("getter") && !m.getReturnType().equals(Void.class))
                return true;
            return m.getName().startsWith("is") && m.getReturnType().equals(Boolean.class);
        }
        return false;
    }

    public <T> CompilingContextBuilder registerPropertyAccessor(Class<T> type, CheckedBiFunction<T, String, Object> propertyAccessor) {
        propertyAccessors.put(type, propertyAccessor);
        return this;
    }

    public CompilingContext build(Object inputValue) {
        return new CompilingContext(inputValue, propertyAccessors, types);
    }

    public CompilingContextBuilder registerStringValueFormat(Class<?> clazz) {
        return registerStringValueFormat(clazz.getSimpleName(), clazz);
    }

    public CompilingContextBuilder registerStringValueFormat(String name, Class<?> clazz) {
        types.put(name, o -> {
            if (o instanceof String) {
                try {
                    return clazz.getConstructor(String.class).newInstance(o);
                } catch (Exception e) {
                    throw new IllegalStateException(String.format("Failed to wrap [%s] to %s. Type Wrapper should have a constructor %s(String)", o, name, name));
                }
            }
            throw new IllegalTypeException();
        });
        return this;
    }

    public CompilingContextBuilder registerSchema(String type, Map<String, String> fieldTypes) {
        types.put(type, o -> {
            Set<String> properties = new HashSet<>();
            Field[] fields = o.getClass().getFields();
            Stream.of(fields).map(Field::getName).forEach(properties::add);
            Method[] methods = o.getClass().getMethods();
            Stream.of(methods).filter(CompilingContextBuilder::isGetter).map(Method::getName).forEach(properties::add);

            if (properties.equals(fieldTypes.keySet()))
                return o;
            else
                throw new IllegalTypeException();
        });
        return this;
    }
}
