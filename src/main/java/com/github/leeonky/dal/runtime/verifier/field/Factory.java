package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.util.BeanClass;

import java.util.Map;
import java.util.Objects;

import static com.github.leeonky.dal.runtime.verifier.SchemaVerifier.errorLog;
import static com.github.leeonky.dal.runtime.verifier.SchemaVerifier.illegalStateException;
import static com.github.leeonky.util.BeanClass.cast;
import static com.github.leeonky.util.BeanClass.getClassName;

public class Factory {
    public static FieldSchema createFieldSchema(String subPrefix, BeanClass<?> type, Object expect,
                                                RuntimeContextBuilder.DALRuntimeContext runtimeContext, Data actual) {
        if (Formatter.class.isAssignableFrom(type.getType()))
            return formatterSchema(subPrefix, type, expect, actual);
        else if (runtimeContext.isSchemaRegistered(type.getType()))
            return subSchema(subPrefix, type, expect, actual);
        else if (type.isCollection()) {
            if (expect == null)
                return new CollectionSchema(subPrefix, type, expect, actual);
            return new CollectionSchema.CollectionContentSchema(subPrefix, type, expect, actual);
        } else if (Map.class.isAssignableFrom(type.getType())) {
            if (expect == null)
                return new MapSchema(subPrefix, type, expect, actual);
            return new MapSchema.MapContentSchema(subPrefix, type, expect, actual);
        } else if (Value.class.isAssignableFrom(type.getType())) {
            if (expect == null)
                return valueSchema(subPrefix, type, actual);
            return valueContentSchema(subPrefix, type, (Value<Object>) expect, actual);
        } else if (Type.class.isAssignableFrom(type.getType())) {
            if (expect == null)
                return typeSchema(subPrefix, type, actual);
            return typeContentSchema(subPrefix, expect, actual);
        } else if (expect == null)
            return javaTypeSchema(subPrefix, type, actual);
        return javaValueSchema(subPrefix, expect, actual);
    }

    @SuppressWarnings("unchecked")
    private static FieldSchema formatterSchema(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
        return runtimeContext -> {
            Formatter<Object, Object> formatter = cast(expect, Formatter.class).orElseGet(() -> createFormatter(type));
            return formatter.isValid(actual.getInstance())
                    || errorLog("Expecting field `%s` to be in `%s`, but was [%s]", subPrefix,
                    formatter.getFormatterName(), actual.getInstance());
        };
    }

    @SuppressWarnings("unchecked")
    private static Formatter<Object, Object> createFormatter(BeanClass<?> type) {
        return (Formatter<Object, Object>) type.getTypeArguments(0)
                .<Object>map(t -> type.newInstance((Object) t.getType())).orElseGet(type::newInstance);
    }

    private static FieldSchema subSchema(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
        return dalRuntimeContext -> actual.createSchemaVerifier().verify(type.getType(), expect, subPrefix);
    }

    private static FieldSchema valueContentSchema(String subPrefix, BeanClass<?> type, Value<Object> expect, Data actual) {
        return runtimeContext -> {
            try {
                return expect.verify(expect.convertAs(runtimeContext, actual.getInstance(),
                        type.getTypeArguments(0).orElse(null)))
                        || errorLog(expect.errorMessage(subPrefix, actual.getInstance()));
            } catch (IllegalFieldException ignore) {
                throw illegalStateException(subPrefix);
            }
        };
    }

    private static FieldSchema valueSchema(String subPrefix, BeanClass<?> type, Data actual) {
        return runtimeContext -> {
            Class<?> rawType = type.getTypeArguments(0).orElseThrow(() -> illegalStateException(subPrefix)).getType();
            try {
                if (actual.isNull())
                    return errorLog("Can not convert null field `%s` to type [%s], " +
                            "use @AllowNull to verify nullable field", subPrefix, rawType.getName());
                actual.convert(rawType);
                return true;
            } catch (Exception ignore) {
                return errorLog("Can not convert field `%s` (%s: %s) to type [%s]", subPrefix,
                        getClassName(actual.getInstance()), actual.getInstance(), rawType.getName());
            }
        };
    }

    private static FieldSchema javaTypeSchema(String subPrefix, BeanClass<?> type, Data actual) {
        return runtimeContext -> type.getType().isInstance(actual.getInstance())
                || errorLog("Expecting field `%s` to be type [%s], but was [%s]", subPrefix,
                type.getName(), getClassName(actual.getInstance()));
    }

    private static FieldSchema javaValueSchema(String subPrefix, Object expect, Data actual) {
        return runtimeContext -> Objects.equals(expect, actual.getInstance())
                || errorLog("Expecting field `%s` to be %s[%s], but was %s[%s]", subPrefix,
                getClassName(expect), expect, getClassName(actual.getInstance()), actual.getInstance());
    }

    private static FieldSchema typeContentSchema(String subPrefix, Object expect, Data actual) {
        return runtimeContext -> ((Type<Object>) expect).verify(actual.getInstance())
                || errorLog(((Type<Object>) expect).errorMessage(subPrefix, actual.getInstance()));
    }

    private static FieldSchema typeSchema(String subPrefix, BeanClass<?> type, Data actual) {
        return runtimeContext -> {
            Class<?> rawType = type.getTypeArguments(0).orElseThrow(() -> illegalStateException(subPrefix)).getType();
            return rawType.isInstance(actual.getInstance())
                    || errorLog("Expecting field `%s` to be type [%s], but was [%s]", subPrefix,
                    rawType.getName(), getClassName(actual.getInstance()));
        };
    }
}
