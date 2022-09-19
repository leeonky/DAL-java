package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.dal.runtime.IllegalTypeException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.util.BeanClass;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static com.github.leeonky.util.BeanClass.getClassName;
import static com.github.leeonky.util.CollectionHelper.toStream;
import static java.lang.String.format;
import static java.util.stream.Collectors.toSet;
import static java.util.stream.IntStream.range;

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
            if (structure())
                return valueStructure(runtimeContext, actual);
            return valueContent(runtimeContext, actual);
        }
        if (expect.isMap()) {
            if (structure())
                return mapStructure(runtimeContext, actual);
            return mapContent(runtimeContext, actual);
        }
        if (expect.isCollection()) {
            if (structure())
                return collectionStructure(runtimeContext, actual);
            return collectionContent(runtimeContext, actual);
        }
        if (expect.isSchemaType()) {
            if (structure())
                return typeStructure(runtimeContext, actual);
            return typeContent(runtimeContext, actual);
        }
        if (structure())
            return structure(runtimeContext, actual);
        return content(runtimeContext, actual);
    }

    public boolean structure() {
        return expect.getExpect() == null;
    }

    private boolean valueStructure(DALRuntimeContext runtimeContext, Actual actual) {
        return actual.convertAble(expect.getGenericType(0).orElseThrow(actual::invalidFieldGenericType), expect.inspectExpectType());
    }

    private boolean valueContent(DALRuntimeContext runtimeContext, Actual actual) {
        try {
            return expect.verifyValue(actual::verifyValue);
        } catch (IllegalFieldException ignore) {
            throw actual.invalidFieldGenericType();
        }
    }

    private boolean mapStructure(DALRuntimeContext context, Actual actual) {
        return actual.getActual().getFieldNames().stream().allMatch(key ->
        {
            Actual subActual = actual.sub(key);
            return expect(expect.sub(key, context, expect, actual.getProperty(), subActual))
                    .verify(context, subActual);
        });
    }

    private boolean mapContent(DALRuntimeContext context, Actual actual) {
        return verifySize(actual.getActual().getFieldNames().size(), ((Map<?, Object>) expect.getExpect()).size(), actual) && mapStructure(context, actual);
    }

    private boolean collectionStructure(DALRuntimeContext context, Actual actual) {
        return range(0, actual.getActual().getListSize()).allMatch(index ->
        {
            Actual subActual = actual.sub(index);
            return expect(expect.sub(context, index, subActual)).verify(context, subActual);
        });
    }

    private boolean collectionContent(DALRuntimeContext context, Actual actual) {
        return verifySize(actual.getActual().getListSize(), (int) toStream(expect.getExpect()).count(), actual) && collectionStructure(context, actual);
    }

    private boolean formatter(DALRuntimeContext runtimeContext, Actual actual) {
        Formatter<Object, Object> formatter = expect.extractFormatter();
        return formatter.isValid(actual.getActual().getInstance())
                || errorLog("Expecting field `%s` to be in `%s`, but was [%s]", actual.getProperty(),
                formatter.getFormatterName(), actual.getActual().getInstance());
    }

    private boolean verifySize(int actualSize, int expectSize, Actual actual) {
        return actualSize == expectSize || errorLog("Expecting field `%s` to be size [%d], but was size [%d]",
                actual.getProperty(), expectSize, actualSize);
    }

    private boolean typeContent(DALRuntimeContext runtimeContext, Actual actual) {
        Type<Object> expect = (Type<Object>) this.expect.getExpect();
        return expect.verify(actual.getActual().getInstance()) ||
                errorLog(expect.errorMessage(actual.getProperty(), actual.getActual().getInstance()));
    }

    private boolean typeStructure(DALRuntimeContext r, Actual actual) {
        BeanClass<?> type = expect.getGenericType(0).orElseThrow(actual::invalidFieldGenericType);
        return type.getType().isInstance(actual.getActual().getInstance()) ||
                errorLog(format("Expecting field `%s` to be %s, but was [%s]", actual.getProperty(), expect.inspectExpectType(),
                        getClassName(actual.getActual().getInstance())));
    }

    private boolean structure(DALRuntimeContext runtimeContext, Actual actual) {
        return expect.getType().getType().isInstance(actual.getActual().getInstance())
                || errorLog(format("Expecting field `%s` to be %s, but was [%s]", actual.getProperty(), expect.inspectExpectType(),
                getClassName(actual.getActual().getInstance())));
    }

    private boolean content(DALRuntimeContext runtimeContext, Actual actual) {
        return Objects.equals(expect.getExpect(), actual.getActual().getInstance()) ||
                errorLog(format("Expecting field `%s` to be %s[%s], but was %s[%s]", actual.getProperty(),
                        getClassName(expect.getExpect()), expect.getExpect(), getClassName(actual.getActual().getInstance()), actual.getActual().getInstance()));
    }

    private boolean schema(DALRuntimeContext runtimeContext, Actual actual) {
        Set<String> actualFields = actual.getActual().getFieldNames().stream().filter(String.class::isInstance)
                .map(Object::toString).collect(toSet());
        return (expect.noMoreUnexpectedField(actualFields))
                && allMandatoryPropertyShouldBeExist(actualFields)
                && allPropertyValueShouldBeValid(runtimeContext, actual)
                && expect.verifySchemaInstance(actual.getActual());
    }

    private boolean allMandatoryPropertyShouldBeExist(Set<String> actualFields) {
        return expect.getType().getPropertyReaders().values().stream()
                .filter(field -> field.getAnnotation(AllowNull.class) == null)
                .allMatch(field -> actualFields.contains(field.getName())
                        || errorLog("Expecting field `%s` to be in type %s[%s], but does not exist", field.getName(),
                        expect.getType().getSimpleName(), expect.getType().getName()));
    }

    private boolean allPropertyValueShouldBeValid(DALRuntimeContext runtimeContext, Actual actual) {
        return expect.getType().getPropertyReaders().values().stream().allMatch(propertyReader -> {
            Actual subActual = actual.sub(propertyReader.getName());
            return propertyReader.getAnnotation(AllowNull.class) != null && subActual.isNull()
                    || expect(expect.sub(propertyReader, runtimeContext, subActual)).verify(runtimeContext, subActual);
        });
    }
}
