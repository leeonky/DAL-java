package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.SchemaAssertionFailure;
import com.github.leeonky.dal.type.Schema;
import com.github.leeonky.dal.type.SubType;
import com.github.leeonky.util.BeanClass;

import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.github.leeonky.util.BeanClass.getClassName;
import static java.lang.String.format;
import static java.util.Optional.ofNullable;
import static java.util.stream.IntStream.range;

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

    public Actual sub(Object property) {
        return new Actual(this.property + "." + property, actual.getValue(property));
    }

    public boolean isNull() {
        return actual.isNull();
    }

    public Actual sub(Integer index) {
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

    public IllegalStateException invalidGenericType() {
        return new IllegalStateException(format("%s should specify generic type", property));
    }

    public boolean convertAble(BeanClass<?> type, String inspect) {
        try {
//            TODO missing test error message
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

    public Stream<Object> fieldNames() {
        return actual.getFieldNames().stream();
    }

    public Stream<Integer> indexStream() {
        return range(0, actual.getListSize()).boxed();
    }

    public boolean verifyFormatter(Formatter<Object, Object> formatter) {
        return formatter.isValid(actual.getInstance())
                || Verification.errorLog("Expecting field `%s` to be in `%s`, but was [%s]", property,
                formatter.getFormatterName(), actual.getInstance());
    }

    boolean verifySize(Function<Actual, Stream<?>> actualStream, int expectSize) {
        return actualStream.apply(this).count() == expectSize
                || Verification.errorLog("Expecting field `%s` to be size [%d], but was size [%d]",
                property, expectSize, actualStream.apply(this).count());
    }

    boolean verifyType(Type<Object> expect) {
        return expect.verify(actual.getInstance()) ||
                Verification.errorLog(expect.errorMessage(property, actual.getInstance()));
    }

    boolean inInstanceOf(BeanClass<?> type) {
        return type.getType().isInstance(actual.getInstance()) ||
                Verification.errorLog(String.format("Expecting field `%s` to be %s, but was [%s]", property,
                        format("type [%s]", type.getName()), getClassName(actual.getInstance())));
    }

    boolean equals2(Object expect) {
        return Objects.equals(expect, actual.getInstance()) ||
                Verification.errorLog(format("Expecting field `%s` to be %s[%s], but was %s[%s]", property,
                        getClassName(expect), expect, getClassName(actual.getInstance()), actual.getInstance()));
    }

    void verifySchema(Schema expect1) {
        try {
            expect1.verify(actual);
        } catch (SchemaAssertionFailure schemaAssertionFailure) {
            Verification.errorLog(schemaAssertionFailure.getMessage());
        }
    }
}
