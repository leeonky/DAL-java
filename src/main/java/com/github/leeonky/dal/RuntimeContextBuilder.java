package com.github.leeonky.dal;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Formatters;
import com.github.leeonky.dal.token.IllegalTypeException;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.dal.util.TypeData;
import com.github.leeonky.dal.util.WrappedObject;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

public class RuntimeContextBuilder {
    private final TypeData<PropertyAccessor> propertyAccessors = new TypeData<>();
    private final TypeData<ListAccessor> listAccessors = new TypeData<>();
    private final Map<String, Constructor> constructors = new LinkedHashMap<>();
    private final Set<Class<?>> schemas = new HashSet<>();

    public RuntimeContextBuilder() {
        registerValueFormat(new Formatters.String());
        registerValueFormat(new Formatters.URL());
        registerValueFormat(new Formatters.Instant());
        registerValueFormat(new Formatters.LocalDate());
        registerValueFormat(new Formatters.LocalDateTime());
        registerValueFormat(new Formatters.Enum<>());

        registerValueFormat(new Formatters.Number());
        registerValueFormat(new Formatters.PositiveInteger());
        registerValueFormat(new Formatters.Integer());
        registerValueFormat(new Formatters.PositiveNumber());
        registerValueFormat(new Formatters.ZeroNumber());

        registerValueFormat(new Formatters.Boolean());

        registerSchema("List", WrappedObject::isList);
    }

    public RuntimeContext build(Object inputValue) {
        return new RuntimeContext(inputValue, propertyAccessors, constructors, listAccessors, schemas);
    }

    public RuntimeContextBuilder registerValueFormat(Formatter formatter) {
        return registerValueFormat(formatter.getFormatterName(), formatter);
    }

    @SuppressWarnings("unchecked")
    public RuntimeContextBuilder registerValueFormat(String name, Formatter formatter) {
        constructors.put(name, (o, context) -> formatter.transform(o));
        return this;
    }

    public RuntimeContextBuilder registerSchema(Class<?> clazz) {
        return registerSchema(clazz, NameStrategy.SIMPLE_NAME);
    }

    public RuntimeContextBuilder registerSchema(String name, Class<?> clazz) {
        schemas.add(clazz);
        return registerSchema(name, (bw) -> bw.createSchemaVerifier().verify(clazz, null, ""));
    }

    public RuntimeContextBuilder registerSchema(String name, Function<WrappedObject, Boolean> predicate) {
        constructors.put(name, (o, context) -> {
            if (o != null && predicate.apply(context.wrap(o)))
                return o;
            throw new IllegalTypeException();
        });
        return this;
    }

    public <T> RuntimeContextBuilder registerPropertyAccessor(Class<T> type, PropertyAccessor<T> propertyAccessor) {
        propertyAccessors.put(type, propertyAccessor);
        return this;
    }

    public <T> RuntimeContextBuilder registerListAccessor(Class<T> type, ListAccessor<T> listAccessor) {
        listAccessors.put(type, listAccessor);
        return this;
    }

    public RuntimeContextBuilder registerSchema(Class<?> clazz, NameStrategy nameStrategy) {
        return registerSchema(nameStrategy.toName(clazz), clazz);
    }
}
