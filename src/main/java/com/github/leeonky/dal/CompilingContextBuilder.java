package com.github.leeonky.dal;

import com.github.leeonky.dal.format.PositiveInteger;
import com.github.leeonky.dal.token.IllegalTypeException;
import com.github.leeonky.dal.util.BeanUtil;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.dal.util.TypeData;

import java.net.URL;
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class CompilingContextBuilder {
    private final TypeData<PropertyAccessor> propertyAccessors = new TypeData<>();
    private final Map<String, Function<Object, Object>> typeDefinitions = new LinkedHashMap<>();
    private final TypeData<ListAccessor> listAccessors = new TypeData<>();

    public CompilingContextBuilder() {
        registerStringValueFormat(String.class);
        registerStringValueFormat(URL.class);
        registerStringValueFormat("Instant", Instant::parse);

        registerNumberValueFormat(PositiveInteger.class);
    }

    public static <T> T requiredType(boolean rightType, Supplier<T> supplier) {
        if (rightType)
            return supplier.get();
        throw new IllegalTypeException();
    }

    public CompilingContext build(Object inputValue) {
        return new CompilingContext(inputValue, propertyAccessors, typeDefinitions, listAccessors);
    }

    public CompilingContextBuilder registerStringValueFormat(Class<?> clazz) {
        return registerStringValueFormat(clazz.getSimpleName(), clazz);
    }

    public CompilingContextBuilder registerStringValueFormat(String name, Class<?> clazz) {
        return registerStringValueFormat(name, s -> {
            try {
                return clazz.getConstructor(String.class).newInstance(s);
            } catch (Exception e) {
                throw new IllegalStateException(String.format("Failed to wrap [%s] to %s. Type Wrapper should have a constructor %s(String)", s, name, name));
            }
        });
    }

    public CompilingContextBuilder registerStringValueFormat(String name, Function<String, ?> mapper) {
        typeDefinitions.put(name, o -> requiredType(o instanceof String, () -> mapper.apply((String) o)));
        return this;
    }

    public CompilingContextBuilder registerNumberValueFormat(Class<?> clazz) {
        return registerNumberValueFormat(clazz.getSimpleName(), clazz);
    }

    public CompilingContextBuilder registerNumberValueFormat(String name, Class<?> clazz) {
        return registerNumberValueFormat(name, n -> {
            try {
                return clazz.getConstructor(Number.class).newInstance(n);
            } catch (Exception e) {
                if (e.getCause() instanceof IllegalTypeException)
                    throw (IllegalTypeException) e.getCause();
                throw new IllegalStateException(String.format("Failed to wrap [%s] to %s. Type Wrapper should have a constructor %s(Number)", n, name, name));
            }
        });
    }

    public CompilingContextBuilder registerNumberValueFormat(String name, Function<Number, ?> mapper) {
        typeDefinitions.put(name, o -> requiredType(o instanceof Number, () -> mapper.apply((Number) o)));
        return this;
    }

    public CompilingContextBuilder registerSchema(Class<?> clazz) {
        return registerSchema(clazz.getSimpleName(), clazz);
    }

    public CompilingContextBuilder registerSchema(String name, Class<?> clazz) {
        return registerSchema(name, bw -> BeanUtil.findPropertyNames(clazz).containsAll(bw.getPropertyNames()));
    }

    @SuppressWarnings("unchecked")
    public <T> CompilingContextBuilder registerSchema(String name, Predicate<BeanWrapper> predicate) {
        typeDefinitions.put(name, o -> requiredType(o != null &&
                predicate.test(new BeanWrapper(o, propertyAccessors)), () -> o));
        return this;
    }

    public <T> CompilingContextBuilder registerPropertyAccessor(Class<T> type, PropertyAccessor<T> propertyAccessor) {
        propertyAccessors.put(type, propertyAccessor);
        return this;
    }

    public <T> CompilingContextBuilder registerListAccessor(Class<T> type, ListAccessor<T> listAccessor) {
        listAccessors.put(type, listAccessor);
        return this;
    }
}
