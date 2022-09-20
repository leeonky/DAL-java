package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.dal.runtime.IllegalTypeException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.util.BeanClass;

import java.util.Set;

import static java.lang.String.format;
import static java.util.stream.Collectors.toSet;

public class Verification {
    final Expect expect;

    private Verification(Expect expect) {
        this.expect = expect;
    }

    public static Verification expect(Expect expect) {
        return new Verification(expect);
    }

    public static boolean errorLog(String format, Object... params) {
        throw new IllegalTypeException(format(format, params));
    }

    public boolean verify(DALRuntimeContext runtimeContext, Actual actual) {
        if (expect.isSchema()) {
            return schema(runtimeContext, actual);
        }
        if (expect.isFormatter()) {
            return formatter(runtimeContext, actual);
        }
        if (expect.isSchemaValue()) {
            if (expect.structure())
                return valueStructure(runtimeContext, actual);
            return valueContent(runtimeContext, actual);
        }
        if (expect.isMap()) {
            if (expect.structure())
                return mapStructure(runtimeContext, actual);
            return mapContent(runtimeContext, actual);
        }
        if (expect.isCollection()) {
            if (expect.structure())
                return collectionStructure(runtimeContext, actual);
            return collectionContent(runtimeContext, actual);
        }
        if (expect.isSchemaType()) {
            if (expect.structure())
                return typeStructure(runtimeContext, actual);
            return typeContent(runtimeContext, actual);
        }
        if (expect.structure())
            return structure(runtimeContext, actual);
        return content(runtimeContext, actual);
    }

    private boolean valueStructure(DALRuntimeContext runtimeContext, Actual actual) {
        return actual.convertAble(expect.getGenericType(0).orElseThrow(actual::invalidGenericType),
                expect.inspectExpectType());
    }

    private boolean valueContent(DALRuntimeContext runtimeContext, Actual actual) {
        try {
            return expect.verifyValue(actual::verifyValue);
        } catch (IllegalFieldException ignore) {
            throw actual.invalidGenericType();
        }
    }

    private boolean mapStructure(DALRuntimeContext context, Actual actual) {
        BeanClass<Object> type = (BeanClass<Object>) expect.getGenericType(1).orElseThrow(actual::invalidGenericType);
        return actual.fieldNames().allMatch(key -> {
            Actual subActual = actual.sub(key);
            return expect(expect.sub(type, key, context, subActual)).verify(context, subActual);
        });
    }

    private boolean mapContent(DALRuntimeContext context, Actual actual) {
        return actual.verifySize(Actual::fieldNames, expect.mapKeysSize()) && mapStructure(context, actual);
    }

    private boolean collectionStructure(DALRuntimeContext context, Actual actual) {
        return actual.indexStream().allMatch(index -> {
            Actual subActual = actual.sub(index);
            return expect(expect.sub(context, index, subActual)).verify(context, subActual);
        });
    }

    private boolean collectionContent(DALRuntimeContext context, Actual actual) {
        return actual.verifySize(Actual::indexStream, expect.collectionSize()) && collectionStructure(context, actual);
    }

    private boolean formatter(DALRuntimeContext runtimeContext, Actual actual) {
        return actual.verifyFormatter(expect.extractFormatter());
    }

    private boolean typeContent(DALRuntimeContext runtimeContext, Actual actual) {
        return actual.verifyType(expect.extractType());
    }

    private boolean typeStructure(DALRuntimeContext r, Actual actual) {
        return expect.isInstanceType(actual);
    }

    private boolean structure(DALRuntimeContext runtimeContext, Actual actual) {
        return expect.isInstanceOf(actual);
    }

    private boolean content(DALRuntimeContext runtimeContext, Actual actual) {
        return expect.equals(actual);
    }

    private boolean schema(DALRuntimeContext runtimeContext, Actual actual) {
        Set<String> actualFields = actual.fieldNames().filter(String.class::isInstance)
                .map(Object::toString).collect(toSet());
        return (expect.noMoreUnexpectedField(actualFields))
                && allMandatoryPropertyShouldBeExist(actualFields)
                && allPropertyValueShouldBeValid(runtimeContext, actual)
                && expect.verifySchemaInstance(actual::verifySchema);
    }

    private boolean allMandatoryPropertyShouldBeExist(Set<String> actualFields) {
        return expect.prpertyReaders().filter(field -> field.getAnnotation(AllowNull.class) == null)
                .allMatch(field -> actualFields.contains(field.getName())
                        || errorLog("Expecting field `%s` to be in type %s, but does not exist", field.getName(),
                        expect.inspectFullType()));
    }

    private boolean allPropertyValueShouldBeValid(DALRuntimeContext runtimeContext, Actual actual) {
        return expect.prpertyReaders().allMatch(propertyReader -> {
            Actual subActual = actual.sub(propertyReader.getName());
            return propertyReader.getAnnotation(AllowNull.class) != null && subActual.isNull()
                    || expect(expect.sub(propertyReader, runtimeContext, subActual)).verify(runtimeContext, subActual);
        });
    }
}
