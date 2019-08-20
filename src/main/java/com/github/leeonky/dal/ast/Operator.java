package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.util.Calculator;
import com.github.leeonky.dal.util.ListAccessor;

import java.lang.reflect.Array;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

public abstract class Operator {
    private static final int PRECEDENCE_LOGIC_COMBINATION_OPT = 200;
    private static final int PRECEDENCE_LOGIC_COMPARE_OPT = 210;
    private static final int PRECEDENCE_PLUS_SUB_OPT = 300;
    private static final int PRECEDENCE_MUL_DIV = 400;
    private static final int PRECEDENCE_UNARY_OPERATION = 500;
    private static final int PRECEDENCE_INDEX = 501;
    private final int precedence;
    private int position;

    protected Operator(int precedence) {
        this.precedence = precedence;
    }

    public boolean isPrecedentThan(Operator operator) {
        return precedence > operator.precedence;
    }

    protected Object calculate(Object v1, Object v2) {
        return null;
    }

    public Object calculate(Node node1, Node node2, CompilingContext context) {
        return calculate(node1.evaluate(context), node2.evaluate(context));
    }

    @Override
    public boolean equals(Object obj) {
        return getClass().isInstance(obj);
    }

    public int getPosition() {
        return position;
    }

    public void setPosition(int position) {
        this.position = position;
    }

    public static class Equal extends Operator {
        public Equal() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.equals(v1, v2);
        }
    }

    public static class Less extends Operator {
        public Less() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) < 0;
        }
    }

    public static class GreaterOrEqual extends Operator {
        public GreaterOrEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) >= 0;
        }
    }

    public static class LessOrEqual extends Operator {
        public LessOrEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) <= 0;
        }
    }

    public static class NotEqual extends Operator {
        public NotEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return !Calculator.equals(v1, v2);
        }
    }

    public static class Plus extends Operator {
        public Plus() {
            super(PRECEDENCE_PLUS_SUB_OPT);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.plus(v1, v2);
        }
    }

    public static class Greater extends Operator {
        public Greater() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) > 0;
        }
    }

    public static class Subtraction extends Operator {
        public Subtraction() {
            super(PRECEDENCE_PLUS_SUB_OPT);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.subtract(v1, v2);
        }
    }

    public static class Multiplication extends Operator {
        public Multiplication() {
            super(PRECEDENCE_MUL_DIV);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.multiply(v1, v2);
        }
    }

    public static class Division extends Operator {
        public Division() {
            super(PRECEDENCE_MUL_DIV);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.divide(v1, v2);
        }
    }

    public static class And extends Operator {
        public And() {
            super(PRECEDENCE_LOGIC_COMBINATION_OPT);
        }

        @Override
        public Object calculate(Node node1, Node node2, CompilingContext context) {
            return Calculator.and(() -> node1.evaluate(context), () -> node2.evaluate(context));
        }
    }

    public static class Or extends Operator {
        public Or() {
            super(PRECEDENCE_LOGIC_COMBINATION_OPT);
        }

        @Override
        public Object calculate(Node node1, Node node2, CompilingContext context) {
            return Calculator.or(() -> node1.evaluate(context), () -> node2.evaluate(context));
        }
    }

    public static class Not extends Operator {
        public Not() {
            super(PRECEDENCE_UNARY_OPERATION);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.not(v2);
        }
    }

    public static class Minus extends Operator {

        public Minus() {
            super(PRECEDENCE_UNARY_OPERATION);
        }

        @Override
        protected Object calculate(Object v1, Object v2) {
            return Calculator.negate(v2);
        }
    }

    public static class Index extends Operator {

        public Index() {
            super(PRECEDENCE_UNARY_OPERATION);
        }

        @Override
        @SuppressWarnings("unchecked")
        public Object calculate(Node node1, Node node2, CompilingContext context) {
            Object v1 = node1.evaluate(context);
            Object v2 = node2.evaluate(context);
            Optional<ListAccessor> optionalArrayType = context.searchListAccessor(v1);
            int index = (int) v2;
            if (optionalArrayType.isPresent())
                return optionalArrayType.get().get(v1, index);
            if (v1 instanceof List)
                return ((List) v1).get(index);
            else if (v1 instanceof Iterable) {
                Iterator iterator = ((Iterable) v1).iterator();
                for (int i = 0; iterator.hasNext(); i++) {
                    Object object = iterator.next();
                    if (Objects.equals(i, v2))
                        return object;
                }
                throw new ArrayIndexOutOfBoundsException();
            }
            return Array.get(v1, index);
        }
    }
}
