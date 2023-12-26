package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.DALExpression;

import java.util.function.Function;
import java.util.function.Supplier;

public abstract class ExpressionException extends java.lang.RuntimeException {
    public static <T> T opt1(Supplier<T> supplier) {
        try {
            return supplier.get();
        } catch (Exception e) {
            throw exception(expression -> new RuntimeException(e.getMessage(), expression.left().getOperandPosition(), e));
        }
    }

    public static <T> T opt2(Supplier<T> supplier) {
        try {
            return supplier.get();
        } catch (Exception e) {
            throw exception(expression -> new RuntimeException(e.getMessage(), expression.right().getOperandPosition(), e));
        }
    }

    public java.lang.RuntimeException rethrow(DALExpression expression) {
        return thrower(expression);
    }

    abstract protected java.lang.RuntimeException thrower(DALExpression expression);

    public static ExpressionException exception(Function<DALExpression, java.lang.RuntimeException> thrower) {
        return new ExpressionException() {
            @Override
            protected java.lang.RuntimeException thrower(DALExpression expression) {
                return thrower.apply(expression);
            }
        };
    }

    public static ExpressionException illegalOperationRuntimeException(String message) {
        return exception(expression -> new RuntimeException(message, expression.operator().getPosition()));
    }

    public static ExpressionException illegalOp2RuntimeException(String message) {
        return exception(expression -> new RuntimeException(message, expression.right().getOperandPosition()));
    }

    public static ExpressionException illegalOp1RuntimeException(String message) {
        return exception(expression -> new RuntimeException(message, expression.left().getOperandPosition()));
    }
}
