package com.github.leeonky.dal.runtime.verifier;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.BeanClass;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.verifier.SchemaVerifier.shouldBeSameSize;
import static com.github.leeonky.util.BeanClass.cast;
import static com.github.leeonky.util.BeanClass.getClassName;
import static com.github.leeonky.util.CollectionHelper.toStream;
import static java.lang.String.format;
import static java.util.stream.Collectors.toList;
import static java.util.stream.IntStream.range;

public class JavaValueSchema implements FieldSchema {
    final String subPrefix;
    final BeanClass<?> type;
    final Object expect;
    final Data actual;

    public JavaValueSchema(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
        this.subPrefix = subPrefix;
        this.type = type;
        this.expect = expect;
        this.actual = actual;
    }

    static FieldSchema createFieldSchema(String subPrefix, BeanClass<?> type, Object expect,
                                         DALRuntimeContext runtimeContext, Data actual) {
        if (Formatter.class.isAssignableFrom(type.getType()))
            return new FormatterSchema(subPrefix, type, expect, actual);
        else if (runtimeContext.isSchemaRegistered(type.getType()))
            return new RecursiveSchema(subPrefix, type, expect, actual);
        else if (type.isCollection()) {
            if (expect != null)
                return new CollectionElementSchema(subPrefix, type, expect, actual);
            return new CollectionSchema(subPrefix, type, new SubExpect(Collections.emptyMap(), range(0, actual.getListSize()).boxed().collect(toList()), "%s[%d]"), actual);
        } else if (Map.class.isAssignableFrom(type.getType()))
            return new MapSchema(subPrefix, type, expect, actual);
        else if (Value.class.isAssignableFrom(type.getType()))
            return new ValueSchema(subPrefix, type, expect, actual);
        else if (Type.class.isAssignableFrom(type.getType()))
            return new TypeSchema(subPrefix, type, expect, actual);
        else if (expect == null)
            return new JavaTypeSchema(subPrefix, type, expect, actual);
        return new JavaValueSchema(subPrefix, type, expect, actual);
    }

    @Override
    public boolean verify(DALRuntimeContext runtimeContext) {
        return Objects.equals(expect, actual.getInstance())
                || SchemaVerifier.errorLog("Expecting field `%s` to be %s[%s], but was %s[%s]", subPrefix,
                getClassName(expect), expect, getClassName(actual.getInstance()), actual.getInstance());
    }

    static class FormatterSchema extends JavaValueSchema {
        private final Formatter<Object, Object> formatter;

        @SuppressWarnings("unchecked")
        public FormatterSchema(String subPrefix, BeanClass<?> type, Object schemaProperty, Data value) {
            super(subPrefix, type, schemaProperty, value);
            formatter = cast(expect, Formatter.class).orElseGet(() -> createFormatter(type));
        }

        @Override
        public boolean verify(DALRuntimeContext runtimeContext) {
            return formatter.isValid(actual.getInstance())
                    || SchemaVerifier.errorLog("Expecting field `%s` to be in `%s`, but was [%s]", subPrefix,
                    formatter.getFormatterName(), actual.getInstance());
        }

        @SuppressWarnings("unchecked")
        private Formatter<Object, Object> createFormatter(BeanClass<?> type) {
            return (Formatter<Object, Object>) type.getTypeArguments(0)
                    .<Object>map(t -> type.newInstance((Object) t.getType())).orElseGet(type::newInstance);
        }
    }

    static class RecursiveSchema extends JavaValueSchema {
        public RecursiveSchema(String subPrefix, BeanClass<?> type, Object schemaProperty, Data value) {
            super(subPrefix, type, schemaProperty, value);
        }

        @Override
        public boolean verify(DALRuntimeContext runtimeContext) {
            return actual.createSchemaVerifier().verify(type.getType(), expect, subPrefix);
        }
    }

    static class CollectionSchema extends JavaValueSchema {
        protected final SubExpect subExpect;

        public CollectionSchema(String subPrefix, BeanClass<?> type, SubExpect subExpect, Data actual) {
            super(subPrefix, type, null, actual);
            this.subExpect = subExpect;
        }

        @Override
        public boolean verify(DALRuntimeContext runtimeContext) {
            return subExpect.getActualProperties().stream().allMatch(property ->
                    createFieldSchema(subExpect.subPrefix(subPrefix, property), subExpect.subType(type),
                            subExpect.getExpectValues().get(property), runtimeContext, actual.getValue(property)).verify(runtimeContext));
        }
    }

    private static Map<?, Object> toMap(Stream<?> stream) {
        return new LinkedHashMap<Integer, Object>() {{
            AtomicInteger index = new AtomicInteger(0);
            stream.forEach(e -> put(index.getAndIncrement(), e));
        }};
    }

    static class CollectionElementSchema extends CollectionSchema {

        public CollectionElementSchema(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
            super(subPrefix, type, new SubExpect(toMap(toStream(expect)), range(0, actual.getListSize()).boxed().collect(toList()), "%s[%d]"), actual);
        }

        @Override
        public boolean verify(DALRuntimeContext runtimeContext) {
            return shouldBeSameSize(subPrefix, subExpect.getActualProperties().size(), subExpect.getExpectValues().size()) && super.verify(runtimeContext);
        }
    }

    static class MapSchema extends JavaValueSchema {
        public MapSchema(String subPrefix, BeanClass<?> type, Object schemaProperty, Data value) {
            super(subPrefix, type, schemaProperty, value);
        }

        @Override
        public boolean verify(DALRuntimeContext runtimeContext) {
            String subPrefix = this.subPrefix;
            Map<?, Object> schemaProperty = (Map<?, Object>) expect;
            BeanClass<?> subGenericType = type.getTypeArguments(1).orElseThrow(() ->
                    new IllegalArgumentException(format("`%s` should be generic type", subPrefix)));
            if (schemaProperty == null)
                return actual.getFieldNames().stream()
                        .allMatch(key -> actual.getValue(key).createSchemaVerifier()
                                .verifySchemaInGenericType(subPrefix + "." + key, subGenericType, null));
            return shouldBeSameSize(subPrefix, actual.getFieldNames().size(), schemaProperty.values().size())
                    && actual.getFieldNames().stream()
                    .allMatch(key -> actual.getValue(key).createSchemaVerifier()
                            .verifySchemaInGenericType(subPrefix + "." + key, subGenericType, schemaProperty.get(key)));
        }
    }

    static class ValueSchema extends JavaValueSchema {
        public ValueSchema(String subPrefix, BeanClass<?> type, Object schemaProperty, Data value) {
            super(subPrefix, type, schemaProperty, value);
        }

        @Override
        public boolean verify(DALRuntimeContext runtimeContext) {
            BeanClass<?> type1 = type.getTypeArguments(0).orElse(null);
            if (expect != null) {
                String subPrefix = this.subPrefix;
                Value<Object> schemaProperty = (Value<Object>) expect;
                try {
                    return schemaProperty.verify(schemaProperty.convertAs(runtimeContext, actual.getInstance(), type1))
                            || SchemaVerifier.errorLog(schemaProperty.errorMessage(subPrefix, actual.getInstance()));
                } catch (IllegalFieldException ignore) {
                    throw SchemaVerifier.illegalStateException(subPrefix);
                }
            }
            if (type1 == null)
                throw SchemaVerifier.illegalStateException(subPrefix);
            String subPrefix = this.subPrefix;
            Class<?> rawType = type1.getType();
            try {
                if (actual.isNull())
                    return SchemaVerifier.errorLog("Can not convert null field `%s` to type [%s], use @AllowNull to verify nullable field",
                            subPrefix, rawType.getName());
                runtimeContext.getConverter().convert(rawType, actual.getInstance());
                return true;
            } catch (Exception ignore) {
                return SchemaVerifier.errorLog("Can not convert field `%s` (%s: %s) to type [%s]", subPrefix,
                        getClassName(actual.getInstance()), actual.getInstance(), rawType.getName());
            }
        }
    }

    static class TypeSchema extends JavaValueSchema {
        public TypeSchema(String subPrefix, BeanClass<?> type, Object property, Data value) {
            super(subPrefix, type, property, value);
        }

        @Override
        public boolean verify(DALRuntimeContext runtimeContext) {
            if (expect != null) {
                Type<Object> schemaProperty1 = (Type<Object>) expect;
                return schemaProperty1.verify(actual.getInstance())
                        || SchemaVerifier.errorLog(schemaProperty1.errorMessage(subPrefix, actual.getInstance()));
            }
            Class<?> rawType = type.getTypeArguments(0)
                    .orElseThrow(() -> SchemaVerifier.illegalStateException(subPrefix)).getType();
            return rawType.isInstance(actual.getInstance())
                    || SchemaVerifier.errorLog("Expecting field `%s` to be type [%s], but was [%s]", subPrefix,
                    rawType.getName(), getClassName(actual.getInstance()));
        }
    }

    private static class JavaTypeSchema extends JavaValueSchema {
        public JavaTypeSchema(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
            super(subPrefix, type, expect, actual);
        }

        @Override
        public boolean verify(DALRuntimeContext runtimeContext) {
            return type.getType().isInstance(actual.getInstance())
                    || SchemaVerifier.errorLog("Expecting field `%s` to be type [%s], but was [%s]", subPrefix,
                    type.getName(), getClassName(actual.getInstance()));
        }
    }
}
