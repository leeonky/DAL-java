package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.BeanClass;

import java.util.Map;

import static java.lang.String.format;

public class Factory {
    public static FieldSchema createFieldSchema(String subPrefix, BeanClass<?> expectType, Object expect,
                                                DALRuntimeContext runtimeContext, Data actual) {
        RootExpectation<Object> expectation = new RootExpectation<>(expectType, subPrefix, expect);
        return expect == null ? createSchema(subPrefix, expectType, expect, actual, expectation, runtimeContext)
                : createContentSchema(subPrefix, expectType, expect, actual, expectation, runtimeContext);
    }

    private static FieldSchema createSchema(String subPrefix, BeanClass<?> expectType, Object expect, Data actual, RootExpectation<Object> expectation, DALRuntimeContext runtimeContext) {
        if (Formatter.class.isAssignableFrom(expectType.getType()))
            return formatterSchema(actual, expectation);
        if (runtimeContext.isSchemaRegistered(expectType.getType()))
            return subSchema(actual, expectation);
        if (expectType.isCollection())
            return new CollectionSchema(subPrefix, expectType, expect, actual);
        if (Map.class.isAssignableFrom(expectType.getType()))
            return new MapSchema(subPrefix, expectType, expect, actual);
        if (Value.class.isAssignableFrom(expectation.getType().getType()))
            return valueSchema(actual, expectation);
        if (Type.class.isAssignableFrom(expectation.getType().getType()))
            return typeSchema(actual, expectation);
        return javaSchema(actual, expectation);
    }

    private static FieldSchema createContentSchema(String subPrefix, BeanClass<?> expectType, Object expect, Data actual, RootExpectation<Object> expectation, DALRuntimeContext runtimeContext) {
        if (Formatter.class.isAssignableFrom(expectType.getType()))
            return formatterContentSchema(actual, expectation);
        if (runtimeContext.isSchemaRegistered(expectType.getType()))
            return subContentSchema(actual, expectation);
        if (expectType.isCollection())
            return new CollectionSchema.CollectionContentSchema(subPrefix, expectType, expect, actual);
        if (Map.class.isAssignableFrom(expectType.getType()))
            return new MapSchema.MapContentSchema(subPrefix, expectType, expect, actual);
        if (Value.class.isAssignableFrom(expectation.getType().getType()))
            return valueContentSchema(actual, expectation);
        if (Type.class.isAssignableFrom(expectation.getType().getType()))
            return typeContentSchema(actual, expectation);
        return javaContentSchema(actual, expectation);
    }

    private static FieldSchema formatterSchema(Data actual, RootExpectation<Object> expectation) {
        return runtimeContext -> expectation.formatterExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema formatterContentSchema(Data actual, RootExpectation<Object> expectation) {
        return runtimeContext -> expectation.formatterContentExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema subSchema(Data actual, RootExpectation<Object> expectation) {
        return runtimeContext -> expectation.schemaExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema subContentSchema(Data actual, RootExpectation<Object> expectation) {
        return runtimeContext -> expectation.schemaContentExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema valueContentSchema(Data actual, RootExpectation<Object> expectation) {
        return runtimeContext -> expectation.valueContentExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema valueSchema(Data actual, RootExpectation<Object> expectation) {
        return runtimeContext -> expectation.valueExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema javaSchema(Data actual, RootExpectation<Object> expectation) {
        return runtimeContext -> expectation.structureExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema javaContentSchema(Data actual, RootExpectation<Object> objectRootExpectation) {
        return runtimeContext -> objectRootExpectation.contentExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema typeContentSchema(Data actual, RootExpectation<Object> objectRootExpectation) {
        return runtimeContext -> objectRootExpectation.typeContentExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema typeSchema(Data actual, RootExpectation<Object> objectRootExpectation) {
        return runtimeContext -> objectRootExpectation.typeExpectation().verify(actual, runtimeContext);
    }

    static IllegalStateException illegalStateException(String subPrefix) {
        return new IllegalStateException(format("%s should specify generic type", subPrefix));
    }
}
