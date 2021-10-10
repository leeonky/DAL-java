package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.Converter;
import com.github.leeonky.util.NumberUtil;

import java.util.Objects;
import java.util.function.Supplier;

import static com.github.leeonky.util.BeanClass.getClassName;

public class Calculator {
    public static int compare(Object v1, Object v2, Converter converter) {
        if (v1 == null || v2 == null)
            throw new IllegalArgumentException(String.format("Can not compare [%s] and [%s]", v1, v2));
        if (v1 instanceof Number && v2 instanceof Number)
            return NumberUtil.compare((Number) v1, (Number) v2, converter);
        if (v1 instanceof String && v2 instanceof String)
            return ((String) v1).compareTo((String) v2);
        throw new IllegalArgumentException(String.format("Can not compare [%s: %s] and [%s: %s]",
                getClassName(v1), v1, getClassName(v2), v2));
    }

    public static boolean equals(Object v1, Object v2) {
        return Objects.equals(v1, v2);
    }

    public static Object plus(Object v1, Object v2, Converter converter) {
        if (v1 instanceof Number && v2 instanceof Number)
            return NumberUtil.plus((Number) v1, (Number) v2, converter);
        if (v1 instanceof String)
            return v1.toString() + v2;
        if (v2 instanceof String)
            return v1 + v2.toString();
//        TODO message reformat type+value
        throw new IllegalArgumentException(String.format("Can not plus '%s' and '%s'", getClassName(v1), getClassName(v2)));
    }

    public static Object subtract(Object v1, Object v2, Converter converter) {
        requireNumber(v1, v2);
        return NumberUtil.subtract((Number) v1, (Number) v2, converter);
    }

    public static Object multiply(Object v1, Object v2, Converter converter) {
        requireNumber(v1, v2);
        return NumberUtil.multiply((Number) v1, (Number) v2, converter);
    }

    public static Object divide(Object v1, Object v2, Converter converter) {
        requireNumber(v1, v2);
        return NumberUtil.divide((Number) v1, (Number) v2, converter);
    }

    private static void requireNumber(Object v1, Object v2) {
        if (!(v1 instanceof Number && v2 instanceof Number))
            throw new IllegalArgumentException(String.format("Operands should be number but '%s' and '%s'",
                    getClassName(v1), getClassName(v2)));
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
            return NumberUtil.negate((Number) v);
        throw new IllegalArgumentException(String.format("Operands should be number but '%s'", getClassName(v)));
    }
}
