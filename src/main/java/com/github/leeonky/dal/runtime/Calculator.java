package com.github.leeonky.dal.runtime;

import java.math.BigDecimal;
import java.util.Objects;
import java.util.function.Supplier;

import static com.github.leeonky.util.BeanClass.getClassName;

public class Calculator {
    public static int compare(Object v1, Object v2) {
        if (v1 == null || v2 == null)
            throw new IllegalArgumentException(String.format("Can not compare [%s] and [%s]", v1, v2));
        if (v1 instanceof Number && v2 instanceof Number)
            return toBigDecimal(v1).compareTo(toBigDecimal(v2));
        if (v1 instanceof String && v2 instanceof String)
            return ((String) v1).compareTo((String) v2);
        throw new IllegalArgumentException(String.format("Can not compare [%s: %s] and [%s: %s]",
                getClassName(v1), v1, getClassName(v2), v2));
    }

    private static BigDecimal toBigDecimal(Object value) {
        return new BigDecimal(String.valueOf(value));
    }

    public static boolean equals(Object v1, Object v2) {
        if (v1 instanceof Number && v2 instanceof Number)
            return toBigDecimal(v1).compareTo(toBigDecimal(v2)) == 0;
        return objectEquals(v1, v2);
    }

    private static boolean objectEquals(Object v1, Object v2) {
        if (v1 != null && v2 != null) {
            if (!v1.getClass().equals(v2.getClass()))
                throw new IllegalArgumentException(String.format("Can not compare '%s' and '%s'",
                        getClassName(v1), getClassName(v2)));
        }
        return Objects.equals(v1, v2);
    }

    public static Object plus(Object v1, Object v2) {
        if (v1 instanceof Number && v2 instanceof Number) {
            if (!(v1 instanceof BigDecimal))
                v1 = new BigDecimal(v1.toString());
            if (!(v2 instanceof BigDecimal))
                v2 = new BigDecimal(v2.toString());
            return ((BigDecimal) v1).add((BigDecimal) v2);
        }

        if (v1 instanceof String)
            return v1.toString() + v2;
        if (v2 instanceof String)
            return v1 + v2.toString();
        throw new IllegalArgumentException(String.format("Can not plus '%s' and '%s'", getClassName(v1), getClassName(v2)));
    }

    public static Object subtract(Object v1, Object v2) {
        requireNumber(v1, v2);
        return new BigDecimal(v1.toString()).subtract(new BigDecimal(v2.toString()));
    }

    public static Object multiply(Object v1, Object v2) {
        requireNumber(v1, v2);
        return new BigDecimal(v1.toString()).multiply(new BigDecimal(v2.toString()));
    }

    public static Object divide(Object v1, Object v2) {
        requireNumber(v1, v2);
        return new BigDecimal(v1.toString()).divide(new BigDecimal(v2.toString()));
    }

    private static void requireNumber(Object v1, Object v2) {
        if (!(v1 instanceof Number && v2 instanceof Number))
            throw new IllegalArgumentException(String.format("Operands should be number but '%s' and '%s'", getClassName(v1), getClassName(v2)));
    }

    public static Object and(Supplier<Object> s1, Supplier<Object> s2) {
        Object v1 = s1.get();
        requireBooleanType(v1, "Operand 1");
        if ((boolean) v1) {
            Object v2 = s2.get();
            requireBooleanType(v2, "Operand 2");
            return v2;
        }
        return false;
    }

    public static Object or(Supplier<Object> s1, Supplier<Object> s2) {
        Object v1 = s1.get();
        requireBooleanType(v1, "Operand 1");
        if (!(boolean) v1) {
            Object v2 = s2.get();
            requireBooleanType(v2, "Operand 2");
            return v2;
        }
        return true;
    }

    public static Object not(Object v) {
        requireBooleanType(v, "Operand");
        return !(boolean) v;
    }

    public static void requireBooleanType(Object v, final String operand) {
        if (!(v instanceof Boolean))
            throw new IllegalArgumentException(operand + " should be boolean but '" + getClassName(v) + "'");
    }

    public static Object negate(Object v) {
        if (v instanceof Number)
            return new BigDecimal(v.toString()).negate();
        throw new IllegalArgumentException(String.format("Operands should be number but '%s'", getClassName(v)));
    }
}
