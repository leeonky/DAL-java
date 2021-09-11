package com.github.leeonky.dal;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Formatters;
import com.github.leeonky.dal.token.IllegalTypeException;
import com.github.leeonky.dal.util.DataObject;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.dal.util.TypeData;
import com.github.leeonky.util.BeanClass;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;

public class RuntimeContextBuilder {
    private final TypeData<PropertyAccessor> propertyAccessors = new TypeData<>();
    private final TypeData<ListAccessor> listAccessors = new TypeData<>();
    private final Map<String, ConstructorViaSchema> constructors = new LinkedHashMap<>();
    private final Map<String, BeanClass<?>> schemas = new HashMap<>();

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

        registerSchema("List", DataObject::isList);
    }

    public RuntimeContext build(Object inputValue) {
        return new RuntimeContext(inputValue, propertyAccessors, constructors, listAccessors, schemas);
    }

    public RuntimeContextBuilder registerValueFormat(Formatter formatter) {
        return registerValueFormat(formatter.getFormatterName(), formatter);
    }

    @SuppressWarnings("unchecked")
    public RuntimeContextBuilder registerValueFormat(String name, Formatter formatter) {
        constructors.put(name, o -> formatter.transform(o.getInstance()));
        return this;
    }

    public RuntimeContextBuilder registerSchema(Class<?> schema) {
        return registerSchema(NameStrategy.SIMPLE_NAME, schema);
    }

    public RuntimeContextBuilder registerSchema(String name, Class<?> schema) {
        schemas.put(name, BeanClass.create(schema));
        return registerSchema(name, dataObject -> dataObject.createSchemaVerifier()
                .verify(schema, null, ""));
    }

    public RuntimeContextBuilder registerSchema(String name, Function<DataObject, Boolean> predicate) {
        constructors.put(name, (o) -> {
            if (predicate.apply(o))
                return o.getInstance();
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

    public RuntimeContextBuilder registerSchema(NameStrategy nameStrategy, Class<?> schema) {
        return registerSchema(nameStrategy.toName(schema), schema);
    }
}
