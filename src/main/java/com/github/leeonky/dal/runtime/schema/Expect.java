package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.SchemaAssertionFailure;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.Schema;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyReader;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiPredicate;
import java.util.stream.Collectors;

import static com.github.leeonky.util.BeanClass.newInstance;
import static java.lang.String.format;
import static java.lang.String.valueOf;

public class Expect {
    private final BeanClass<Object> type;
    private final Object expect;

    public Expect(BeanClass<Object> type, Object expect) {
        this.type = type;
        this.expect = expect;
    }

    public static Expect schemaExpect(Class<?> type, Object expect, Actual subActual) {
        Class<Object> realSchemaType = subActual.polymorphicSchemaType(type);
        return new Expect(BeanClass.create(realSchemaType), expect == null ? newInstance(realSchemaType) : expect) {
            @Override
            public boolean isSchema() {
                return true;
            }

            @Override
            public boolean isPartial() {
                return type.getAnnotation(Partial.class) != null;
            }
        };
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

    protected boolean isPartial() {
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
    public Expect sub(PropertyReader<Object> propertyReader, DALRuntimeContext context, Actual actual) {
        return create((BeanClass<Object>) propertyReader.getType(),
                expect == null ? null : propertyReader.getValue(expect), context, actual);
    }

    private static Expect create(BeanClass<Object> type, Object expect, DALRuntimeContext context, Actual subActual) {
        return context.isSchemaRegistered(type.getType()) ? schemaExpect(type.getType(), expect, subActual) : new Expect(type, expect);
    }

    @SuppressWarnings("unchecked")
    Expect sub(Object key, DALRuntimeContext context, Expect expect, String property, Actual subActual) {
        return create((BeanClass<Object>) expect.getType().getTypeArguments(1).orElseThrow(() -> new IllegalStateException(format("%s should specify generic type", property))),
                expect.expect == null ? null : ((Map<?, Object>) expect.expect).get(key), context, subActual);
    }

    public boolean verifySchemaInstance(Data actual) {
        if (expect instanceof Schema) {
            try {
                ((Schema) expect).verify(actual);
            } catch (SchemaAssertionFailure schemaAssertionFailure) {
                Verification.errorLog(schemaAssertionFailure.getMessage());
            }
        }
        return true;
    }

    public boolean noMoreUnexpectedField(Set<String> actualFields) {
        if (isPartial())
            return true;
        Set<String> expectFields = new LinkedHashSet<String>(actualFields) {{
            removeAll(getType().getPropertyReaders().keySet());
        }};
        return expectFields.isEmpty() || Verification.errorLog("Unexpected field %s for schema %s[%s]",
                expectFields.stream().collect(Collectors.joining("`, `", "`", "`")),
                getType().getSimpleName(), getType().getName());
    }

    @SuppressWarnings("unchecked")
    public Formatter<Object, Object> extractFormatter() {
        if (expect == null)
            return (Formatter<Object, Object>) type.getTypeArguments(0).map(t -> type.newInstance((Object) t.getType()))
                    .orElseGet(type::newInstance);
        return (Formatter<Object, Object>) expect;
    }

    Expect sub(DALRuntimeContext context, int index, Actual subActual) {
        return sub(type.getPropertyReader(valueOf(index)), context, subActual);
    }

    Optional<BeanClass<?>> getGenericType(int typePosition) {
        return getType().getTypeArguments(typePosition);
    }

    String inspectExpectType() {
        return format("type [%s]", getType().getName());
    }

    public boolean verifyValue(BiPredicate<Value<Object>, BeanClass<?>> predicate) {
        return predicate.test((Value<Object>) getExpect(), getGenericType(0).orElse(null));
    }
}
