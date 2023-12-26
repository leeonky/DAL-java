package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.NumberType;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

import static com.github.leeonky.dal.runtime.ExpressionException.illegalOp2RuntimeException;
import static com.github.leeonky.dal.runtime.ExpressionException.illegalOperationRuntimeException;
import static com.github.leeonky.util.Classes.getClassName;
import static java.lang.String.format;
import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.reverseOrder;

public class Calculator {
    private static final NumberType numberType = new NumberType();

    private static int compare(Data v1, Data v2, DALRuntimeContext context) {
        Object instance1 = v1.instance();
        Object instance2 = v2.instance();
        if (instance1 == null || instance2 == null)
            throw illegalOperationRuntimeException(format("Can not compare [%s] and [%s]", instance1, instance2));
        if (instance1 instanceof Number && instance2 instanceof Number)
            return context.getNumberType().compare((Number) instance1, (Number) instance2);
        if (instance1 instanceof String && instance2 instanceof String)
            return ((String) instance1).compareTo((String) instance2);
        throw illegalOperationRuntimeException(format("Can not compare [%s: %s] and [%s: %s]",
                getClassName(instance1), instance1, getClassName(instance2), instance2));
    }

    public static boolean equals(Data v1, Data v2) {
        if (v1.instance() == v2.instance())
            return true;
        if (ExpressionException.opt2(v2::isNull))
            return ExpressionException.opt1(v1::isNull);
        if (v2.isList() && v1.isList())
            return collect(v2, "2").equals(collect(v1, "1"));
        return Objects.equals(v1.instance(), v2.instance());
    }

    private static List<Object> collect(Data v2, String index) {
        try {
            return v2.list().collect();
        } catch (InfiniteCollectionException ignore) {
            throw illegalOperationRuntimeException("Invalid operation, operand " + index + " is infinite collection");
        }
    }

    public static Data plus(Data v1, DALOperator opt, Data v2, DALRuntimeContext context) {
        return context.calculate(v1, opt, v2);
    }

    public static Data subtract(Data v1, DALOperator opt, Data v2, DALRuntimeContext context) {
        return context.calculate(v1, opt, v2);
    }

    public static Data multiply(Data v1, DALOperator opt, Data v2, DALRuntimeContext context) {
        return context.calculate(v1, opt, v2);
    }

    public static Data divide(Data v1, DALOperator opt, Data v2, DALRuntimeContext context) {
        return context.calculate(v1, opt, v2);
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
        if (!(v instanceof Boolean))
            throw illegalOperationRuntimeException("Operand" + " should be boolean but '" + getClassName(v) + "'");
        return !(boolean) v;
    }

    public static Data negate(Data data, DALRuntimeContext context) {
        Object value = data.instance();
        if (value instanceof Number)
            return context.wrap(context.getNumberType().negate((Number) value));
        if (data.isList())
            return sortList(data, reverseOrder());
        throw illegalOperationRuntimeException(format("Operand should be number or list but '%s'", getClassName(value)));
    }

    @SuppressWarnings("unchecked")
    private static Data sortList(Data data, Comparator<?> comparator) {
        try {
            return data.list().sort(Comparator.comparing(Data::instance, (Comparator<Object>) comparator)).wrap();
        } catch (InfiniteCollectionException e) {
            throw illegalOperationRuntimeException("Can not sort infinite collection");
        }
    }

    public static Data positive(Data data, DALRuntimeContext context) {
        Object value = data.instance();
        if (data.isList())
            return sortList(data, naturalOrder());
        throw illegalOp2RuntimeException(format("Operand should be list but '%s'", getClassName(value)));
    }

    public static Data less(Data left, DALOperator opt, Data right, DALRuntimeContext context) {
        return context.wrap(compare(left, right, context) < 0);
    }

    public static Data greaterOrEqual(Data left, DALOperator opt, Data right, DALRuntimeContext context) {
        return context.wrap(compare(left, right, context) >= 0);
    }

    public static Data lessOrEqual(Data left, DALOperator opt, Data right, DALRuntimeContext context) {
        return context.wrap(compare(left, right, context) <= 0);
    }

    public static Data greater(Data left, DALOperator opt, Data right, DALRuntimeContext context) {
        return context.wrap(compare(left, right, context) > 0);
    }

    public static boolean notEqual(Data left, Data right) {
        return !equals(left, right);
    }
}
