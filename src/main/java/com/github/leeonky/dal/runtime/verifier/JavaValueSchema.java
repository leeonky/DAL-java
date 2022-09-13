package com.github.leeonky.dal.runtime.verifier;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.BeanClass;

import java.util.Map;
import java.util.Objects;

import static com.github.leeonky.dal.runtime.verifier.SchemaVerifier.errorLog;
import static com.github.leeonky.util.BeanClass.getClassName;

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
            return dalRuntimeContext -> actual.createSchemaVerifier().verify(type.getType(), expect, subPrefix);
        else if (type.isCollection()) {
            if (expect != null)
                return new CollectionSchema.CollectionElementSchema(subPrefix, type, expect, actual);
            return new CollectionSchema(subPrefix, type, expect, actual);
        } else if (Map.class.isAssignableFrom(type.getType())) {
            if (expect != null)
                return new MapSchema.MapElementSchema(subPrefix, type, expect, actual);
            return new MapSchema(subPrefix, type, expect, actual);
        } else if (Value.class.isAssignableFrom(type.getType()))
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
                || errorLog("Expecting field `%s` to be %s[%s], but was %s[%s]", subPrefix,
                getClassName(expect), expect, getClassName(actual.getInstance()), actual.getInstance());
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
                            || errorLog(schemaProperty.errorMessage(subPrefix, actual.getInstance()));
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
                    return errorLog("Can not convert null field `%s` to type [%s], use @AllowNull to verify nullable field",
                            subPrefix, rawType.getName());
                runtimeContext.getConverter().convert(rawType, actual.getInstance());
                return true;
            } catch (Exception ignore) {
                return errorLog("Can not convert field `%s` (%s: %s) to type [%s]", subPrefix,
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
                Type<Object> schemaProperty = (Type<Object>) expect;
                return schemaProperty.verify(actual.getInstance())
                        || errorLog(schemaProperty.errorMessage(subPrefix, actual.getInstance()));
            }
            Class<?> rawType = type.getTypeArguments(0)
                    .orElseThrow(() -> SchemaVerifier.illegalStateException(subPrefix)).getType();
            return rawType.isInstance(actual.getInstance())
                    || errorLog("Expecting field `%s` to be type [%s], but was [%s]", subPrefix,
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
                    || errorLog("Expecting field `%s` to be type [%s], but was [%s]", subPrefix,
                    type.getName(), getClassName(actual.getInstance()));
        }
    }
}
