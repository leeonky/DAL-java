package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.Schema;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Classes;
import com.github.leeonky.util.PropertyAccessor;
import com.github.leeonky.util.PropertyReader;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiPredicate;
import java.util.stream.Stream;

import static com.github.leeonky.util.CollectionHelper.toStream;
import static java.lang.String.format;
import static java.lang.String.valueOf;
import static java.util.stream.Collectors.joining;

public class Expect {
    protected final BeanClass<Object> type;
    protected final Object expect;

    public Expect(BeanClass<Object> type, Object expect) {
        this.type = type;
        this.expect = expect;
    }

    public boolean isSchema() {
        return type.isInheritedFrom(Schema.class);
    }

    public boolean isFormatter() {
        return type.isInheritedFrom(Formatter.class);
    }

    public boolean isCollection() {
        return type.isCollection();
    }

    public boolean isMap() {
        return type.isInheritedFrom(Map.class);
    }

    public boolean isSchemaValue() {
        return type.isInheritedFrom(Value.class);
    }

    public boolean isSchemaType() {
        return type.isInheritedFrom(Type.class);
    }

    @SuppressWarnings("unchecked")
    public Expect sub(PropertyReader<Object> propertyReader) {
        return new Expect((BeanClass<Object>) propertyReader.getType(),
                expect == null ? null : propertyReader.getValue(expect));
    }

    @SuppressWarnings("unchecked")
    public Expect sub(BeanClass<Object> type, Object key) {
        return new Expect(type, expect == null ? null : ((Map<?, Object>) expect).get(key));
    }

    public Expect sub(int index) {
        return sub(type.getPropertyReader(valueOf(index)));
    }

    public Optional<Schema> getSchema() {
        return BeanClass.cast(expect, Schema.class);
    }

    @SuppressWarnings("unchecked")
    public Formatter<Object, Object> extractFormatter() {
        if (expect == null)
            return (Formatter<Object, Object>) type.getTypeArguments(0)
                    .map(t -> type.newInstance(t.getType())).orElseGet(type::newInstance);
        return (Formatter<Object, Object>) expect;
    }

    public Optional<BeanClass<?>> getGenericType(int typePosition) {
        return type.getTypeArguments(typePosition);
    }

    public String inspectExpectType() {
        return format("type [%s]", type.getName());
    }

    public String inspectFullType() {
        return String.format("%s[%s]", type.getSimpleName(), type.getName());
    }

    @SuppressWarnings("unchecked")
    public boolean verifyValue(BiPredicate<Value<Object>, BeanClass<?>> predicate) {
        return predicate.test((Value<Object>) expect, getGenericType(0).orElse(null));
    }

    @SuppressWarnings("unchecked")
    @Deprecated
    public int mapKeysSize() {
        return ((Map<?, Object>) expect).size();
    }

    public Stream<Expect> subElements() {
        AtomicInteger index = new AtomicInteger();
        return expect == null ? Stream.generate(() -> sub(index.getAndIncrement()))
                : toStream(expect).map(e -> sub(index.getAndIncrement()));
    }

    @SuppressWarnings("unchecked")
    public Type<Object> extractType() {
        return (Type<Object>) expect;
    }

    public boolean isInstanceOf(Actual actual) {
        return actual.inInstanceOf(type);
    }

    public boolean isInstanceType(Actual actual) {
        return actual.inInstanceOf(getGenericType(0).orElseThrow(actual::invalidGenericType));
    }

    public boolean equals(Actual actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        return actual.equalsExpect(expect, runtimeContext);
    }

    public Stream<PropertyReader<Object>> propertyReaders() {
        return type.getPropertyReaders().values().stream();
    }

    public boolean structure() {
        return expect == null;
    }

    public SchemaExpect asSchema(Actual actual) {
        return new SchemaExpect(actual.polymorphicSchemaType(type.getType()), expect);
    }

    static class SchemaExpect extends Expect {
        public SchemaExpect(Class<Object> schemaType, Object expect) {
            super(BeanClass.create(schemaType), expect == null ? Classes.newInstance(schemaType) : expect);
        }

        public boolean noMoreUnexpectedField(Set<String> actualFields) {
            if (type.getAnnotation(Partial.class) != null)
                return true;
            Set<String> expectFields = new LinkedHashSet<String>(actualFields) {{
                propertyReaders().map(PropertyAccessor::getName).forEach(this::remove);
            }};
            return expectFields.isEmpty() || Verification.errorLog("Unexpected field %s for schema %s",
                    expectFields.stream().collect(joining("`, `", "`", "`")), inspectFullType());
        }

        public boolean allMandatoryPropertyShouldBeExist(Set<String> actualFields) {
            return propertyReaders().filter(field -> field.getAnnotation(AllowNull.class) == null)
                    .allMatch(field -> actualFields.contains(field.getName())
                            || Verification.errorLog("Expected field `%s` to be in type %s, but does not exist", field.getName(),
                            inspectFullType()));
        }

        public boolean allPropertyValueShouldBeValid(RuntimeContextBuilder.DALRuntimeContext runtimeContext, Actual actual) {
            return propertyReaders().allMatch(propertyReader -> {
                Actual subActual = actual.sub(propertyReader.getName());
                return propertyReader.getAnnotation(AllowNull.class) != null && subActual.isNull()
                        || Verification.expect(sub(propertyReader)).verify(runtimeContext, subActual);
            });
        }

        public boolean verifySchemaInstance(Actual actual) {
            getSchema().ifPresent(actual::verifySchema);
            return true;
        }

        public boolean verify(RuntimeContextBuilder.DALRuntimeContext runtimeContext, Actual actual, Set<String> actualFields) {
            return noMoreUnexpectedField(actualFields)
                    && allMandatoryPropertyShouldBeExist(actualFields)
                    && allPropertyValueShouldBeValid(runtimeContext, actual)
                    && verifySchemaInstance(actual);
        }
    }
}
