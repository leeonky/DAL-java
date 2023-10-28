package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.runtime.IllegalFieldException;
import com.github.leeonky.dal.runtime.IllegalTypeException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.TriplePredicate;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.Zipped;
import com.github.leeonky.util.function.IfFactory;

import static com.github.leeonky.util.Zipped.zip;
import static com.github.leeonky.util.function.When.when;
import static java.lang.String.format;
import static java.util.stream.Collectors.toSet;

public class Verification {
    private static final IfFactory.Factory<Expect, TriplePredicate<Verification, DALRuntimeContext, Actual>> VERIFICATIONS =
            when(Expect::isSchema).<TriplePredicate<Verification, DALRuntimeContext, Actual>>then(Verification::schema)
                    .when(Expect::isFormatter).then(Verification::formatter)
                    .when(Expect::isSchemaValue).then(combine(Verification::valueStructure, Verification::valueContent))
                    .when(Expect::isMap).then(combine(Verification::mapStructure, Verification::mapContent))
                    .when(Expect::isCollection).then(combine(Verification::collectionStructure, Verification::collectionContent))
                    .when(Expect::isSchemaType).then(combine(Verification::typeStructure, Verification::typeContent))
                    .orElse(combine(Verification::structure, Verification::content));
    private final Expect expect;

    private Verification(Expect expect) {
        this.expect = expect;
    }

    public static Verification expect(Expect expect) {
        return new Verification(expect);
    }

    public static boolean errorLog(String format, Object... params) {
        throw new IllegalTypeException(format(format, params));
    }

    private static TriplePredicate<Verification, DALRuntimeContext, Actual> combine(
            TriplePredicate<Verification, DALRuntimeContext, Actual> structure,
            TriplePredicate<Verification, DALRuntimeContext, Actual> content) {
        return ((verification, context, actual) -> verification.expect.structure()
                ? structure.test(verification, context, actual) : content.test(verification, context, actual));
    }

    public boolean verify(DALRuntimeContext runtimeContext, Actual actual) {
        return VERIFICATIONS.get(expect).test(this, runtimeContext, actual);
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

    @SuppressWarnings("unchecked")
    private boolean mapStructure(DALRuntimeContext context, Actual actual) {
        BeanClass<Object> type = (BeanClass<Object>) expect.getGenericType(1).orElseThrow(actual::invalidGenericType);
        return actual.fieldNames().allMatch(key -> expect(expect.sub(type, key)).verify(context, actual.sub(key)));
    }

    private boolean mapContent(DALRuntimeContext context, Actual actual) {
        return actual.verifySize(Actual::fieldNames, expect.mapKeysSize()) && mapStructure(context, actual);
    }

    private boolean collectionStructure(DALRuntimeContext context, Actual actual) {
        return zip(actual.subElements(), expect.subElements()).stream()
                .allMatch(e -> expect(e.right()).verify(context, e.left()));
    }

    private boolean collectionContent(DALRuntimeContext context, Actual actual) {
        Zipped<Actual, Expect> zipped = zip(actual.subElements(), expect.subElements());
        if (zipped.stream().allMatch(e -> expect(e.right()).verify(context, e.left()))) {
            if (zipped.hasLeft())
                return actual.lessExpectSize(zipped.index());
            if (zipped.hasRight())
                return actual.moreExpectSize(zipped.index());
            return true;
        }
        return false;
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
        return expect.equals(actual, runtimeContext);
    }

    private boolean schema(DALRuntimeContext runtimeContext, Actual actual) {
        return expect.asSchema(actual).verify(runtimeContext, actual,
                actual.fieldNames().filter(String.class::isInstance).map(Object::toString).collect(toSet()));
    }
}
