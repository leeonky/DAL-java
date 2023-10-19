package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.ast.node.DALNode;
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
    public static DALOperator logical(Notation<?, ?, ?> notation, NodeNodeContextObject.SupplierSupplierData logical) {
        return new Operator(Precedence.LOGICAL, notation, NodeNodeContextObject.adapt(logical), true);
    }

    public static DALOperator plusSub(Notation<?, ?, ?> notation, NodeNodeContextObject.ObjectObjectContextObject plusSub) {
        return new Operator(Precedence.PLUS_SUB, notation, NodeNodeContextObject.adapt(plusSub), false);
    }

    public static DALOperator mulDiv(Notation<?, ?, ?> notation, NodeNodeContextObject.ObjectObjectContextObject mulDiv) {
        return new Operator(Precedence.MUL_DIV, notation, NodeNodeContextObject.adapt(mulDiv), false);
    }

    public static DALOperator comparator(Notation<?, ?, ?> notation, NodeNodeContextObject operation) {
        return new Operator(Precedence.COMPARISON, notation, operation, true);
    }

    public static DALOperator unary(Notation<?, ?, ?> notation, NodeNodeContextObject unary) {
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
            public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
                return right.evaluateData(context);
            }

            @Override
            public String inspect(String node1, String node2) {
                return String.format("(%s)", node2);
            }
        };
    }

    public static DALOperator executable(Notation<?, ?, ?> notation) {
        return new DALOperator(Precedence.PROPERTY, notation.getLabel(), false) {
            @Override
            public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
                return ((ExecutableNode) right).getValue(left, context);
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
            public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
                return ((SchemaComposeNode) right).verify(left, context);
            }
        };
    }

    public static DALOperator which() {
        return new DALOperator(Precedence.WHICH, Notations.Operators.WHICH.getLabel(), true) {
            @Override
            public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
                try {
                    return left.evaluateData(context).newBlockScope(() -> right.evaluate(context));
                } catch (IllegalStateException e) {
                    throw new RuntimeException(e.getMessage(), getPosition());
                }
            }
        };
    }

    public static DALOperator remark() {
        return new DALOperator(Precedence.REMARK, "", true) {

            @Override
            public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
                Data leftValue = left.evaluateData(context);
                Data rightValue = right.evaluateData(context);
                if (Objects.equals(leftValue.getInstance(), rightValue.getInstance()))
                    return leftValue;
                throw new RuntimeException(String.format("Incorrect const remark, const value was %s\nbut remark %s was %s",
                        leftValue.dumpAll(), right.inspect(), rightValue.dumpAll()), right.getPositionBegin());
            }

            @Override
            public String inspect(String node1, String node2) {
                return node1 + " " + node2;
            }
        };
    }

    public interface NodeNodeContextObject extends TriFunction<DALNode, DALNode, DALRuntimeContext, Object> {
        static NodeNodeContextObject adapt(SupplierSupplierData operation) {
            return (left, right, context) -> operation.apply(() -> left.evaluateData(context),
                    () -> right.evaluateData(context)).getInstance();
        }

        static NodeNodeContextObject adapt(ObjectObjectContextObject operation) {
            return (left, right, context) -> operation.apply(left.evaluate(context), right.evaluate(context), context);
        }

        static NodeNodeContextObject adapt(DataContextObject operation) {
            return (left, right, context) -> operation.apply(right.evaluateData(context), context);
        }

        static NodeNodeContextObject adapt(ObjectObject operation) {
            return (left, right, context) -> operation.apply(right.evaluate(context));
        }

        static NodeNodeContextObject adapt(DataDataObject operation) {
            return (left, right, context) -> operation.apply(left.evaluateData(context), right.evaluateData(context));
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
        private final NodeNodeContextObject operation;

        public Operator(int precedence, Notation<?, ?, ?> notation, NodeNodeContextObject operation, boolean needInspect) {
            super(precedence, notation.getLabel(), needInspect);
            this.operation = operation;
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return operation.apply(left, right, context);
        }
    }
}
