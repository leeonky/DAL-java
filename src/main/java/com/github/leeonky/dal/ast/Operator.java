package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.Calculator;

public abstract class Operator {
    private static final int PRECEDENCE_LOGIC_COMBINATION_OPT = 200;
    private static final int PRECEDENCE_LOGIC_COMPARE_OPT = 210;
    private static final int PRECEDENCE_PLUS_SUB_OPT = 300;
    private static final int PRECEDENCE_MUL_DIV = 400;
    private static final int PRECEDENCE_LOGIC_NOT_OPT = 500;
    private final int precedence;
    private int position;

    protected Operator(int precedence) {
        this.precedence = precedence;
    }

    public boolean isPrecedentThan(Operator operator) {
        return precedence > operator.precedence;
    }

    public abstract Object calculate(Object v1, Object v2);

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
        public Object calculate(Object v1, Object v2) {
            return Calculator.equals(v1, v2);
        }
    }

    public static class Less extends Operator {
        public Less() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) < 0;
        }
    }

    public static class GreaterOrEqual extends Operator {
        public GreaterOrEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) >= 0;
        }
    }

    public static class LessOrEqual extends Operator {
        public LessOrEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) <= 0;
        }
    }

    public static class NotEqual extends Operator {
        public NotEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return !Calculator.equals(v1, v2);
        }
    }

    public static class Plus extends Operator {
        public Plus() {
            super(PRECEDENCE_PLUS_SUB_OPT);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.plus(v1, v2);
        }
    }

    public static class Greater extends Operator {
        public Greater() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) > 0;
        }
    }

    public static class Subtraction extends Operator {
        public Subtraction() {
            super(PRECEDENCE_PLUS_SUB_OPT);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.subtract(v1, v2);
        }
    }

    public static class Multiplication extends Operator {
        public Multiplication() {
            super(PRECEDENCE_MUL_DIV);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.multiply(v1, v2);
        }
    }

    public static class Division extends Operator {
        public Division() {
            super(PRECEDENCE_MUL_DIV);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.divide(v1, v2);
        }
    }

    public static class And extends Operator {
        public And() {
            super(PRECEDENCE_LOGIC_COMBINATION_OPT);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.and(v1, v2);
        }
    }

    public static class Or extends Operator {
        public Or() {
            super(PRECEDENCE_LOGIC_COMBINATION_OPT);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.or(v1, v2);
        }
    }

    public static class Not extends Operator {
        public Not() {
            super(PRECEDENCE_LOGIC_NOT_OPT);
        }

        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.not(v2);
        }
    }
}
