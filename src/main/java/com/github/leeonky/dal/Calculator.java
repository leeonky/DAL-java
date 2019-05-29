package com.github.leeonky.dal;

import java.math.BigDecimal;
import java.util.Objects;

public class Calculator {
    public static int compare(Object v1, Object v2) {
        if (v1 == null || v2 == null)
            throw new IllegalStateException(String.format("Can not compare <%s> and <%s>", v1, v2));
        if (v1 instanceof Number && v2 instanceof Number)
            return toBigDecimal(v1).compareTo(toBigDecimal(v2));
        if (v1 instanceof String && v2 instanceof String)
            return ((String) v1).compareTo((String) v2);
        throw new IllegalStateException(String.format("Can not compare <%s: %s> and <%s: %s>", v1.getClass().getName(), v1, v2.getClass().getName(), v2));
    }

    private static BigDecimal toBigDecimal(Object value) {
        return new BigDecimal(String.valueOf(value));
    }

    public static boolean equals(Object v1, Object v2) {
        return (v1 instanceof Number && v2 instanceof Number && toBigDecimal(v1).equals(toBigDecimal(v2)))
                || Objects.equals(v1, v2);
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
        throw new IllegalStateException(String.format("Can not plus %s and %s", getClass(v1), getClass(v2)));
    }

    private static Class<?> getClass(Object obj) {
        return obj == null ? null : obj.getClass();
    }
}
