package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.Schema;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyAccessor;
import com.github.leeonky.util.PropertyReader;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
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
//        TODO move to beanclass ***************
        return Schema.class.isAssignableFrom(type.getType());
    }

    public boolean isPartial() {
//        TODO move to beanclass *************** instance/method return optional
        return type.getType().getAnnotation(Partial.class) != null;
    }

    public boolean isFormatter() {
//        TODO move to beanclass ***************
        return Formatter.class.isAssignableFrom(type.getType());
    }

    public boolean isCollection() {
        return type.isCollection();
    }

    public boolean isMap() {
//        TODO move to beanclass ***************
        return Map.class.isAssignableFrom(type.getType());
    }

    public boolean isSchemaValue() {
//        TODO move to beanclass ***************
        return Value.class.isAssignableFrom(type.getType());
    }

    public boolean isSchemaType() {
//        TODO move to beanclass ***************
        return Type.class.isAssignableFrom(type.getType());
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
//        TODO move to beanclass ***************
        return BeanClass.cast(expect, Schema.class);
    }

    @SuppressWarnings("unchecked")
    public Formatter<Object, Object> extractFormatter() {
        if (expect == null)
            return (Formatter<Object, Object>) type.getTypeArguments(0)
                    .map(t -> type.newInstance((Object) t.getType())).orElseGet(type::newInstance);
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

    @SuppressWarnings("unchecked")
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
        return actual.equalsExpect(expect);
    }

    Stream<PropertyReader<Object>> prpertyReaders() {
        return type.getPropertyReaders().values().stream();
    }

    public boolean structure() {
        return expect == null;
    }

    public SchemaExpect asSchema(Actual actual) {
        Class<Object> schemaType = actual.polymorphicSchemaType(type.getType());
        return new SchemaExpect(schemaType, expect);
    }

    public static class SchemaExpect extends Expect {
        public SchemaExpect(Class<Object> schemaType, Object expect) {
            super(BeanClass.create(schemaType), expect == null ? BeanClass.newInstance(schemaType) : expect);
        }

        boolean noMoreUnexpectedField(Set<String> actualFields) {
            if (isPartial())
                return true;
            Set<String> expectFields = new LinkedHashSet<String>(actualFields) {{
                prpertyReaders().map(PropertyAccessor::getName).forEach(this::remove);
            }};
            return expectFields.isEmpty() || Verification.errorLog("Unexpected field %s for schema %s",
                    expectFields.stream().collect(joining("`, `", "`", "`")), inspectFullType());
        }

        boolean allMandatoryPropertyShouldBeExist(Set<String> actualFields) {
            return prpertyReaders().filter(field -> field.getAnnotation(AllowNull.class) == null)
                    .allMatch(field -> actualFields.contains(field.getName())
                            || Verification.errorLog("Expecting field `%s` to be in type %s, but does not exist", field.getName(),
                            inspectFullType()));
        }

        boolean allPropertyValueShouldBeValid(RuntimeContextBuilder.DALRuntimeContext runtimeContext, Actual actual) {
            return prpertyReaders().allMatch(propertyReader -> {
                Actual subActual = actual.sub(propertyReader.getName());
                //            TODO annotation optional **************
                return propertyReader.getAnnotation(AllowNull.class) != null && subActual.isNull()
                        || Verification.expect(sub(propertyReader)).verify(runtimeContext, subActual);
            });
        }

        boolean verifySchemaInstance(Actual actual) {
            getSchema().ifPresent(actual::verifySchema);
            return true;
        }

        boolean verify(RuntimeContextBuilder.DALRuntimeContext runtimeContext, Actual actual, Set<String> actualFields) {
            return noMoreUnexpectedField(actualFields)
                    && allMandatoryPropertyShouldBeExist(actualFields)
                    && allPropertyValueShouldBeValid(runtimeContext, actual)
                    && verifySchemaInstance(actual);
        }
    }
}
