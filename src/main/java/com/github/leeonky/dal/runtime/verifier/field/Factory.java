package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.BeanClass;

import java.util.Map;

import static java.lang.String.format;

@Deprecated
public class Factory {
    public static FieldSchema_BK createFieldSchema(String subPrefix, BeanClass<?> expectType, Object expect,
                                                   DALRuntimeContext runtimeContext, Data actual) {
        RootExpectation_BK<Object> expectation = new RootExpectation_BK<>(expectType, subPrefix, expect);
        return expect == null ? createSchema(subPrefix, expectType, expect, actual, expectation, runtimeContext)
                : createContentSchema(subPrefix, expectType, expect, actual, expectation, runtimeContext);
    }

    public static FieldSchema_BK createSchema(String subPrefix, BeanClass<?> expectType, Object expect, Data actual, RootExpectation_BK<Object> expectation, DALRuntimeContext runtimeContext) {
        if (Formatter.class.isAssignableFrom(expectType.getType()))
            return formatterSchema(actual, expectation);
        if (runtimeContext.isSchemaRegistered(expectType.getType()))
            return subSchema(actual, expectation);
        if (expectType.isCollection())
            return new CollectionSchemaBK(subPrefix, expectType, expect, actual);
        if (Map.class.isAssignableFrom(expectType.getType()))
            return new MapSchemaBK(subPrefix, expectType, expect, actual);
        if (Value.class.isAssignableFrom(expectation.getType().getType()))
            return valueSchema(actual, expectation);
        if (Type.class.isAssignableFrom(expectation.getType().getType()))
            return typeSchema(actual, expectation);
        return javaSchema(actual, expectation);
    }

    public static FieldSchema_BK createContentSchema(String subPrefix, BeanClass<?> expectType, Object expect, Data actual, RootExpectation_BK<Object> expectation, DALRuntimeContext runtimeContext) {
        if (Formatter.class.isAssignableFrom(expectType.getType()))
            return formatterContentSchema(actual, expectation);
        if (runtimeContext.isSchemaRegistered(expectType.getType()))
            return subContentSchema(actual, expectation);
        if (expectType.isCollection())
            return new CollectionSchemaBK.CollectionContentSchemaBK(subPrefix, expectType, expect, actual);
        if (Map.class.isAssignableFrom(expectType.getType()))
            return new MapSchemaBK.MapContentSchemaBK(subPrefix, expectType, expect, actual);
        if (Value.class.isAssignableFrom(expectation.getType().getType()))
            return valueContentSchema(actual, expectation);
        if (Type.class.isAssignableFrom(expectation.getType().getType()))
            return typeContentSchema(actual, expectation);
        return javaContentSchema(actual, expectation);
    }

    private static FieldSchema_BK formatterSchema(Data actual, RootExpectation_BK<Object> expectation) {
        return runtimeContext -> expectation.formatterExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema_BK formatterContentSchema(Data actual, RootExpectation_BK<Object> expectation) {
        return runtimeContext -> expectation.formatterContentExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema_BK subSchema(Data actual, RootExpectation_BK<Object> expectation) {
        return runtimeContext -> expectation.schemaExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema_BK subContentSchema(Data actual, RootExpectation_BK<Object> expectation) {
        return runtimeContext -> expectation.schemaContentExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema_BK valueContentSchema(Data actual, RootExpectation_BK<Object> expectation) {
        return runtimeContext -> expectation.valueContentExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema_BK valueSchema(Data actual, RootExpectation_BK<Object> expectation) {
        return runtimeContext -> expectation.valueExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema_BK javaSchema(Data actual, RootExpectation_BK<Object> expectation) {
        return runtimeContext -> expectation.structureExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema_BK javaContentSchema(Data actual, RootExpectation_BK<Object> objectRootExpectationBK) {
        return runtimeContext -> objectRootExpectationBK.contentExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema_BK typeContentSchema(Data actual, RootExpectation_BK<Object> objectRootExpectationBK) {
        return runtimeContext -> objectRootExpectationBK.typeContentExpectation().verify(actual, runtimeContext);
    }

    private static FieldSchema_BK typeSchema(Data actual, RootExpectation_BK<Object> objectRootExpectationBK) {
        return runtimeContext -> objectRootExpectationBK.typeExpectation().verify(actual, runtimeContext);
    }

    static IllegalStateException illegalStateException(String subPrefix) {
        return new IllegalStateException(format("%s should specify generic type", subPrefix));
    }
}
