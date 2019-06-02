package com.github.leeonky.dal;

import java.math.BigDecimal;
import java.util.Objects;

public class Calculator {
    public static int compare(Object v1, Object v2) {
        if (v1 == null || v2 == null)
            throw new IllegalArgumentException(String.format("Can not compare <%s> and <%s>", v1, v2));
        if (v1 instanceof Number && v2 instanceof Number)
            return toBigDecimal(v1).compareTo(toBigDecimal(v2));
        if (v1 instanceof String && v2 instanceof String)
            return ((String) v1).compareTo((String) v2);
        throw new IllegalArgumentException(String.format("Can not compare <%s: %s> and <%s: %s>",
                v1.getClass().getName(), v1, v2.getClass().getName(), v2));
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
                throw new IllegalArgumentException(String.format("Can not compare %s and %s",
                        v1.getClass().getName(), v2.getClass().getName()));
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
        throw new IllegalArgumentException(String.format("Can not plus %s and %s", DALCompiler.getClassName(v1), DALCompiler.getClassName(v2)));
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
            throw new IllegalArgumentException(String.format("Operands should be number but %s and %s", DALCompiler.getClassName(v1), DALCompiler.getClassName(v2)));
    }

    public static Object and(Object v1, Object v2) {
        requireBooleanType(v1, "Operand 1");
        requireBooleanType(v2, "Operand 2");
        return Boolean.logicalAnd((boolean) v1, (boolean) v2);
    }

    public static Object or(Object v1, Object v2) {
        requireBooleanType(v1, "Operand 1");
        requireBooleanType(v2, "Operand 2");
        return Boolean.logicalOr((boolean) v1, (boolean) v2);
    }

    public static Object not(Object v) {
        requireBooleanType(v, "Operand");
        return !(boolean) v;
    }

    private static void requireBooleanType(Object v, final String operand) {
        if (!(v instanceof Boolean))
            throw new IllegalArgumentException(operand + " should be boolean but " + DALCompiler.getClassName(v));
    }
}
