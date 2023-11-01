package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.CollectionHelper;
import com.github.leeonky.util.NumberType;

import java.util.Comparator;
import java.util.Objects;
import java.util.function.Supplier;

import static com.github.leeonky.util.Classes.getClassName;
import static java.lang.String.format;
import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.reverseOrder;

public class Calculator {
    private static final NumberType numberType = new NumberType();

    public static int compare(Object v1, Object v2, DALRuntimeContext context) {
        if (v1 == null || v2 == null)
            throw new IllegalOperationException(format("Can not compare [%s] and [%s]", v1, v2));
        if (v1 instanceof Number && v2 instanceof Number)
            return context.getNumberType().compare((Number) v1, (Number) v2);
        if (v1 instanceof String && v2 instanceof String)
            return ((String) v1).compareTo((String) v2);
        throw new IllegalOperationException(format("Can not compare [%s: %s] and [%s: %s]",
                getClassName(v1), v1, getClassName(v2), v2));
    }

    public static boolean equals(Data v1, Data v2) {
        if (v2.isNull())
            return v1.isNull();
        if (v2.isList())
            return CollectionHelper.equals(v1.instance(), v2.instance());
        return Objects.equals(v1.instance(), v2.instance());
    }

    public static Object plus(Object v1, Object v2, DALRuntimeContext context) {
        if (v1 instanceof Number && v2 instanceof Number)
            return context.getNumberType().plus((Number) v1, (Number) v2);
        if (v1 instanceof String)
            return v1.toString() + v2;
        if (v2 instanceof String)
            return v1 + v2.toString();
        throw new IllegalOperationException(format("Can not plus '%s' and '%s'", getClassName(v1), getClassName(v2)));
    }

    public static Object subtract(Object v1, Object v2, DALRuntimeContext context) {
        requireNumber(v1, v2);
        return context.getNumberType().subtract((Number) v1, (Number) v2);
    }

    public static Object multiply(Object v1, Object v2, DALRuntimeContext context) {
        requireNumber(v1, v2);
        return context.getNumberType().multiply((Number) v1, (Number) v2);
    }

    public static Object divide(Object v1, Object v2, DALRuntimeContext context) {
        requireNumber(v1, v2);
        return context.getNumberType().divide((Number) v1, (Number) v2);
    }

    private static void requireNumber(Object v1, Object v2) {
        if (!(v1 instanceof Number && v2 instanceof Number))
            throw new IllegalOperationException(format("Operands should be number but '%s' and '%s'",
                    getClassName(v1), getClassName(v2)));
    }

    public static Data and(Supplier<Data> s1, Supplier<Data> s2) {
        Data v1 = s1.get();
        return isTrue(v1) ? s2.get() : v1;
    }

    private static boolean isTrue(Data value) {
        if (value.instance() instanceof Boolean)
            return (boolean) value.instance();
        if (value.instance() instanceof Number)
            return numberType.compare(0, (Number) value.instance()) != 0;
        return !value.isNull();
    }

    public static Data or(Supplier<Data> s1, Supplier<Data> s2) {
        Data v1 = s1.get();
        return isTrue(v1) ? v1 : s2.get();
    }

    public static Object not(Object v) {
        requireBooleanType(v, "Operand");
        return !(boolean) v;
    }

    public static void requireBooleanType(Object v, final String operand) {
        if (!(v instanceof Boolean))
            throw new IllegalOperationException(operand + " should be boolean but '" + getClassName(v) + "'");
    }

    public static Data negate(Data data, DALRuntimeContext context) {
        Object value = data.instance();
        if (value instanceof Number)
            return context.wrap(context.getNumberType().negate((Number) value));
        if (data.isList())
            return sortList(data, reverseOrder());
        throw new IllegalOperationException(format("Operands should be number but '%s'", getClassName(value)));
    }

    @SuppressWarnings("unchecked")
    private static Data sortList(Data data, Comparator<?> comparator) {
        try {
            return data.list().sort(Comparator.comparing(Data::instance, (Comparator<Object>) comparator)).wrap();
        } catch (InfiniteCollectionException e) {
            throw new IllegalOperationException("Can not sort infinite collection");
        }
    }

    public static Data positive(Data data, DALRuntimeContext context) {
        Object value = data.instance();
        if (value instanceof Number)
            return context.wrap(value);
        if (data.isList())
            return sortList(data, naturalOrder());
        throw new IllegalOperationException(format("Operands should be List but '%s'", getClassName(value)));
    }

    public static boolean less(Object left, Object right, DALRuntimeContext context) {
        return compare(left, right, context) < 0;
    }

    public static boolean greaterOrEqual(Object left, Object right, DALRuntimeContext context) {
        return compare(left, right, context) >= 0;
    }

    public static boolean lessOrEqual(Object left, Object rgiht, DALRuntimeContext context) {
        return compare(left, rgiht, context) <= 0;
    }

    public static boolean greater(Object left, Object right, DALRuntimeContext context) {
        return compare(left, right, context) > 0;
    }

    public static boolean notEqual(Data left, Data right) {
        return !equals(left, right);
    }
}
