package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.ExecutableNode;
import com.github.leeonky.dal.ast.node.SchemaComposeNode;
import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.Notation;
import com.github.leeonky.util.function.TriFunction;

import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

public class Factory {
    public static DALOperator logical(Notation<?, ?, ?, ?, ?> notation, ExpressionContextObject.SupplierSupplierData logical) {
        return new Operator(Precedence.LOGICAL, notation, ExpressionContextObject.adapt(logical), true);
    }

    public static DALOperator plusSub(Notation<?, ?, ?, ?, ?> notation, ExpressionContextObject.ObjectObjectContextObject plusSub) {
        return new Operator(Precedence.PLUS_SUB, notation, ExpressionContextObject.adapt(plusSub), false);
    }

    public static DALOperator mulDiv(Notation<?, ?, ?, ?, ?> notation, ExpressionContextObject.ObjectObjectContextObject mulDiv) {
        return new Operator(Precedence.MUL_DIV, notation, ExpressionContextObject.adapt(mulDiv), false);
    }

    public static DALOperator comparator(Notation<?, ?, ?, ?, ?> notation, ExpressionContextObject operation) {
        return new Operator(Precedence.COMPARISON, notation, operation, true);
    }

    public static DALOperator unary(Notation<?, ?, ?, ?, ?> notation, ExpressionContextObject unary) {
        return new Operator(Precedence.UNARY_OPERATION, notation, unary, true) {
            @Override
            public String inspect(String node1, String node2) {
                return notation.getLabel() + node2;
            }
        };
    }

    public static DALOperator parentheses() {
        return new DALOperator(Precedence.PARENTHESES, "", false) {
            @Override
            public Data calculateData(DALExpression expression, DALRuntimeContext context) {
                return expression.right().evaluateData(context);
            }

            @Override
            public String inspect(String node1, String node2) {
                return String.format("(%s)", node2);
            }
        };
    }

    public static DALOperator executable(Notation<?, ?, ?, ?, ?> notation) {
        return new DALOperator(Precedence.PROPERTY, notation.getLabel(), false) {
            @Override
            public Data calculateData(DALExpression expression, DALRuntimeContext context) {
                return ((ExecutableNode) expression.right()).getValue(expression.left(), context);
            }

            @Override
            public String inspect(String node1, String node2) {
                return String.format("%s%s%s", node1, label, node2);
            }
        };
    }

    public static DALOperator is() {
        return new DALOperator(Precedence.COMPARISON, Notations.Operators.IS.getLabel(), true) {
            @Override
            public Data calculateData(DALExpression expression, DALRuntimeContext context) {
                return ((SchemaComposeNode) expression.right()).verify(expression.left(), context);
            }
        };
    }

    public static DALOperator which() {
        return new DALOperator(Precedence.WHICH, Notations.Operators.WHICH.getLabel(), true) {
            @Override
            public Object calculate(DALExpression expression, DALRuntimeContext context) {
                try {
                    return expression.left().evaluateData(context).newBlockScope(() -> expression.right().evaluate(context));
                } catch (IllegalStateException e) {
                    throw new RuntimeException(e.getMessage(), getPosition());
                }
            }
        };
    }

    public static DALOperator remark() {
        return new DALOperator(Precedence.REMARK, "", true) {

            @Override
            public Data calculateData(DALExpression expression, DALRuntimeContext context) {
                Data leftValue = expression.left().evaluateData(context);
                Data rightValue = expression.right().evaluateData(context);
                if (Objects.equals(leftValue.getInstance(), rightValue.getInstance()))
                    return leftValue;
                throw new RuntimeException(String.format("Incorrect const remark, const value was %s\nbut remark %s was %s",
                        leftValue.dumpAll(), expression.right().inspect(), rightValue.dumpAll()), expression.right().getPositionBegin());
            }

            @Override
            public String inspect(String node1, String node2) {
                return node1 + " " + node2;
            }
        };
    }

    public interface ExpressionContextObject extends BiFunction<DALExpression, DALRuntimeContext, Object> {
        static ExpressionContextObject adapt(SupplierSupplierData operation) {
            return (expression, context) -> operation.apply(() -> expression.left().evaluateData(context),
                    () -> expression.right().evaluateData(context)).getInstance();
        }

        static ExpressionContextObject adapt(ObjectObjectContextObject operation) {
            return (expression, context) -> operation.apply(expression.left().evaluate(context), expression.right().evaluate(context), context);
        }

        static ExpressionContextObject adapt(DataContextObject operation) {
            return (expression, context) -> operation.apply(expression.right().evaluateData(context), context);
        }

        static ExpressionContextObject adapt(ObjectObject operation) {
            return (expression, context) -> operation.apply(expression.right().evaluate(context));
        }

        static ExpressionContextObject adapt(DataDataObject operation) {
            return (expression, context) -> operation.apply(expression.left().evaluateData(context), expression.right().evaluateData(context));
        }

        interface SupplierSupplierData extends BiFunction<Supplier<Data>, Supplier<Data>, Data> {
        }

        interface ObjectObjectContextObject extends TriFunction<Object, Object, DALRuntimeContext, Object> {
        }

        interface DataContextObject extends BiFunction<Data, DALRuntimeContext, Object> {
        }

        interface ObjectObject extends Function<Object, Object> {
        }

        interface DataDataObject extends BiFunction<Data, Data, Object> {
        }
    }

    static class Operator extends DALOperator {
        private final ExpressionContextObject operation;

        public Operator(int precedence, Notation<?, ?, ?, ?, ?> notation, ExpressionContextObject operation, boolean needInspect) {
            super(precedence, notation.getLabel(), needInspect);
            this.operation = operation;
        }

        @Override
        public Object calculate(DALExpression expression, DALRuntimeContext context) {
            return operation.apply(expression, context);
        }
    }
}
