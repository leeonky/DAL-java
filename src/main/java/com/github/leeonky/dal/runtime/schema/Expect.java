package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.SubType;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyReader;

import java.util.Map;
import java.util.stream.Stream;

import static java.lang.String.format;

public class Expect {
    private final BeanClass<Object> type;
    private final Object expect;

    public Expect(BeanClass<Object> type, Object expect) {
        this.type = type;
        this.expect = expect;
    }

    public static Expect schemaExpect(BeanClass<?> type, Object expect, Data actual) {
        BeanClass<?> polymorphicSchemaType = getPolymorphicSchemaType(type.getType(), actual);
        return new Expect((BeanClass<Object>) polymorphicSchemaType, expect == null ? polymorphicSchemaType.newInstance() : expect) {
            @Override
            public boolean isSchema() {
                return true;
            }

            @Override
            public boolean isPartial() {
                return type.getType().getAnnotation(Partial.class) != null;
            }
        };
    }

    private static final Compiler compiler = new Compiler();

    public static BeanClass<?> getPolymorphicSchemaType(Class<?> schemaType, Data actual) {
        Class<?> type = schemaType;
        SubType subType = schemaType.getAnnotation(SubType.class);
        if (subType != null) {
            Object subTypeProperty = actual.getValue(compiler.toChainNodes(subType.property())).getInstance();
            type = Stream.of(subType.types()).filter(t -> t.value().equals(subTypeProperty)).map(SubType.Type::type)
                    .findFirst().orElseThrow(() -> new IllegalStateException(
                            format("Cannot guess sub type through property type value[%s]", subTypeProperty)));
        }
        return BeanClass.create(type);
    }

    public BeanClass<Object> getType() {
        return type;
    }

    public Object getExpect() {
        return expect;
    }

    public boolean isSchema() {
        return false;
    }

    public boolean isPartial() {
        return false;
    }

    public boolean isFormatter() {
        return Formatter.class.isAssignableFrom(((BeanClass<?>) getType()).getType());
    }

    public boolean isCollection() {
        return getType().isCollection();
    }

    public boolean isMap() {
        return Map.class.isAssignableFrom(((BeanClass<?>) getType()).getType());
    }

    public boolean isSchemaValue() {
        return Value.class.isAssignableFrom(((BeanClass<?>) getType()).getType());
    }

    public boolean isSchemaType() {
        return Type.class.isAssignableFrom(((BeanClass<?>) getType()).getType());
    }

    @SuppressWarnings("unchecked")
    public Expect subExpect(PropertyReader<Object> propertyReader, DALRuntimeContext context, Data actual) {
        return create((BeanClass<Object>) propertyReader.getType(),
                expect == null ? null : propertyReader.getValue(expect), context, actual);
    }

    private static Expect create(BeanClass<Object> type, Object expect, DALRuntimeContext context, Data actual) {
        return context.isSchemaRegistered(type.getType()) ? schemaExpect(type, expect, actual) : new Expect(type, expect);
    }

    @SuppressWarnings("unchecked")
    Expect subExpect(Object key, DALRuntimeContext context, final Expect expect, String property, Data subActual) {
        return create((BeanClass<Object>) expect.getType().getTypeArguments(1).orElseThrow(() ->
                        Verification.illegalStateException(property)),
                expect.getExpect() == null ? null : ((Map<?, Object>) expect.getExpect()).get(key), context, subActual);
    }
}
