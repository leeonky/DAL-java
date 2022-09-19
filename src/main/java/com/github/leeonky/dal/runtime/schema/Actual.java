package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.type.SubType;
import com.github.leeonky.util.BeanClass;

import java.util.stream.Stream;

import static com.github.leeonky.util.BeanClass.getClassName;
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

    public IllegalStateException invalidFieldGenericType() {
        return new IllegalStateException(format("%s should specify generic type", property));
    }

    public boolean convertAble(BeanClass<?> type, String inspect) {
        try {
            if (isNull())
                return Verification.errorLog("Can not convert null field `%s` to %s, " +
                        "use @AllowNull to verify nullable field", property, inspect);
            actual.convert(type.getType());
            return true;
        } catch (Exception ignore) {
            return Verification.errorLog("Can not convert field `%s` (%s: %s) to %s", property,
                    getClassName(actual.getInstance()), actual.getInstance(), inspect);
        }
    }

    public boolean verifyValue(Value<Object> value, BeanClass<?> type) {
        return value.verify(value.convertAs(actual, type))
                || Verification.errorLog(value.errorMessage(property, actual.getInstance()));
    }
}
