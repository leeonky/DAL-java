package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.type.SubType;

import java.util.function.Supplier;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.util.Optional.ofNullable;

public class Actual {
    private final String property;
    private final Data actual;

    public Actual(String property, Data actual) {
        this.property = property;
        this.actual = actual;
    }

    public static Actual actual(Data data) {
        return new Actual("", data);
    }

    public String getProperty() {
        return property;
    }

    @Deprecated
    public Data getActual() {
        return actual;
    }

    public Actual sub(Object property) {
        return new Actual(this.property + "." + property, actual.getValue(property));
    }

    public boolean isNull() {
        return actual.isNull();
    }

    public Actual sub(int index) {
        return new Actual(property + "[" + index + "]", actual.getValue(index));
    }

    private final static Compiler compiler = new Compiler();

    @SuppressWarnings("unchecked")
    public Class<Object> polymorphicSchemaType(Class<?> schemaType) {
        return ofNullable(schemaType.getAnnotation(SubType.class)).map(subType -> {
            Object subTypeProperty = actual.getValue(compiler.toChainNodes(subType.property())).getInstance();
            return (Class<Object>) Stream.of(subType.types()).filter(t -> t.value().equals(subTypeProperty))
                    .map(SubType.Type::type).findFirst().orElseThrow(() -> new IllegalStateException(
                            format("Cannot guess sub type through property type value[%s]", subTypeProperty)));
        }).orElse((Class<Object>) schemaType);
    }

    Supplier<IllegalStateException> invalidGenericType() {
        return () -> new IllegalStateException(format("%s should specify generic type", getProperty()));
    }
}
