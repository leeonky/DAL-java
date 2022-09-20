package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.Schema;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyReader;

import java.util.Map;
import java.util.Optional;
import java.util.function.BiPredicate;
import java.util.stream.Stream;

import static com.github.leeonky.util.BeanClass.newInstance;
import static com.github.leeonky.util.CollectionHelper.toStream;
import static java.lang.String.format;
import static java.lang.String.valueOf;

public class Expect {
    private final BeanClass<Object> type;
    final Object expect;

    public Expect(BeanClass<Object> type, Object expect) {
        this.type = type;
        this.expect = expect;
    }

    public static Expect schemaExpect(Class<?> type, Object expect,
                                      @Deprecated
                                      Actual subActual) {
        Class<Object> realSchemaType = subActual.polymorphicSchemaType(type);
        return new Expect(BeanClass.create(realSchemaType), expect == null ? newInstance(realSchemaType) : expect) {
            @Override
            public boolean isPartial() {
                return type.getAnnotation(Partial.class) != null;
            }
        };
    }

    public boolean isSchema() {
        return Schema.class.isAssignableFrom(type.getType());
    }

    protected boolean isPartial() {
        return false;
    }

    public boolean isFormatter() {
        return Formatter.class.isAssignableFrom(((BeanClass<?>) type).getType());
    }

    public boolean isCollection() {
        return type.isCollection();
    }

    public boolean isMap() {
        return Map.class.isAssignableFrom(((BeanClass<?>) type).getType());
    }

    public boolean isSchemaValue() {
        return Value.class.isAssignableFrom(((BeanClass<?>) type).getType());
    }

    public boolean isSchemaType() {
        return Type.class.isAssignableFrom(((BeanClass<?>) type).getType());
    }

    @SuppressWarnings("unchecked")
    public Expect sub(PropertyReader<Object> propertyReader, DALRuntimeContext context, Actual actual) {
        return create((BeanClass<Object>) propertyReader.getType(),
                expect == null ? null : propertyReader.getValue(expect), context, actual);
    }

    private static Expect create(BeanClass<Object> type, Object expect, DALRuntimeContext context, Actual subActual) {
        return context.isSchemaRegistered(type.getType()) ? schemaExpect(type.getType(), expect, subActual) : new Expect(type, expect);
    }

    @SuppressWarnings("unchecked")
    public Expect sub(BeanClass<Object> type, Object key, DALRuntimeContext context, Actual subActual) {
        return create(type, expect == null ? null : ((Map<?, Object>) expect).get(key), context, subActual);
    }

    public Expect sub(DALRuntimeContext context, int index, Actual subActual) {
        return sub(type.getPropertyReader(valueOf(index)), context, subActual);
    }

    public Optional<Schema> getSchema() {
        return BeanClass.cast(expect, Schema.class);
    }

    @SuppressWarnings("unchecked")
    public Formatter<Object, Object> extractFormatter() {
        if (expect == null)
            return (Formatter<Object, Object>) type.getTypeArguments(0).map(t -> type.newInstance((Object) t.getType()))
                    .orElseGet(type::newInstance);
        return (Formatter<Object, Object>) expect;
    }

    Optional<BeanClass<?>> getGenericType(int typePosition) {
        return type.getTypeArguments(typePosition);
    }

    String inspectExpectType() {
        return format("type [%s]", type.getName());
    }

    String inspectFullType() {
        return String.format("%s[%s]", type.getSimpleName(), type.getName());
    }

    public boolean verifyValue(BiPredicate<Value<Object>, BeanClass<?>> predicate) {
        return predicate.test((Value<Object>) expect, getGenericType(0).orElse(null));
    }

    @SuppressWarnings("unchecked")
    public int mapKeysSize() {
        return ((Map<?, Object>) expect).size();
    }

    int collectionSize() {
        return (int) toStream(expect).count();
    }

    Type<Object> extractType() {
        return (Type<Object>) expect;
    }

    boolean isInstanceOf(Actual actual) {
        return actual.inInstanceOf(type);
    }

    boolean isInstanceType(Actual actual) {
        return actual.inInstanceOf(getGenericType(0).orElseThrow(actual::invalidGenericType));
    }

    boolean equals(Actual actual) {
        return actual.equals2(expect);
    }

    Stream<PropertyReader<Object>> prpertyReaders() {
        return type.getPropertyReaders().values().stream();
    }

    public boolean structure() {
        return expect == null;
    }

    public Expect asSchema(Actual actual) {
        return schemaExpect(type.getType(), expect, actual);
    }
}
