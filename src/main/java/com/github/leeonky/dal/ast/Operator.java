package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.util.Calculator;

public abstract class Operator {
    private static final int PRECEDENCE_LOGIC_COMBINATION_OPT = 200;
    private static final int PRECEDENCE_LOGIC_COMPARE_OPT = 210;
    private static final int PRECEDENCE_PLUS_SUB_OPT = 300;
    private static final int PRECEDENCE_MUL_DIV = 400;
    private static final int PRECEDENCE_UNARY_OPERATION = 500;
    private static final int PRECEDENCE_INDEX = 501;
    private final int precedence;
    private final String inspect;
    private final boolean needInspect;
    private int position;

    protected Operator(int precedence, String inspect, boolean needInspect) {
        this.precedence = precedence;
        this.inspect = inspect;
        this.needInspect = needInspect;
    }

    public boolean isNeedInspect() {
        return needInspect;
    }

    public boolean isPrecedentThan(Operator operator) {
        return precedence > operator.precedence;
    }

    public abstract Object calculate(Node node1, Node node2, RuntimeContext context);

    @Override
    public boolean equals(Object obj) {
        return getClass().isInstance(obj);
    }

    public int getPosition() {
        return position;
    }

    public Operator setPosition(int position) {
        this.position = position;
        return this;
    }

    public String inspect(Node node1, Node node2) {
        if (node1 instanceof InputNode)
            return String.format("%s %s", inspect, node2.inspect());
        return String.format("%s %s %s", node1.inspect(), inspect, node2.inspect());
    }

    public static class Equal extends Operator {
        public Equal() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "=", true);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return node2.judge(node1, this, context);
        }
    }

    public static class Less extends Operator {
        public Less() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "<", true);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context)) < 0;
        }
    }

    public static class GreaterOrEqual extends Operator {
        public GreaterOrEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, ">=", true);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context)) >= 0;
        }
    }

    public static class LessOrEqual extends Operator {
        public LessOrEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "<=", true);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context)) <= 0;
        }
    }

    public static class NotEqual extends Operator {
        public NotEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "!=", true);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return !Calculator.equals(node1.evaluate(context), node2.evaluate(context));
        }
    }

    public static class Plus extends Operator {
        public Plus() {
            super(PRECEDENCE_PLUS_SUB_OPT, "+", false);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.plus(node1.evaluate(context), node2.evaluate(context));
        }
    }

    public static class Greater extends Operator {
        public Greater() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, ">", true);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context)) > 0;
        }
    }

    public static class Subtraction extends Operator {
        public Subtraction() {
            super(PRECEDENCE_PLUS_SUB_OPT, "-", false);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.subtract(node1.evaluate(context), node2.evaluate(context));
        }
    }

    public static class Multiplication extends Operator {
        public Multiplication() {
            super(PRECEDENCE_MUL_DIV, "*", false);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.multiply(node1.evaluate(context), node2.evaluate(context));
        }
    }

    public static class Division extends Operator {
        public Division() {
            super(PRECEDENCE_MUL_DIV, "/", false);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.divide(node1.evaluate(context), node2.evaluate(context));
        }
    }

    public static class And extends Operator {
        public And(String inspect) {
            super(PRECEDENCE_LOGIC_COMBINATION_OPT, inspect, true);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.and(() -> node1.evaluate(context), () -> node2.evaluate(context));
        }
    }

    public static class Or extends Operator {
        public Or(String inspect) {
            super(PRECEDENCE_LOGIC_COMBINATION_OPT, inspect, true);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.or(() -> node1.evaluate(context), () -> node2.evaluate(context));
        }
    }

    public static class Not extends Operator {
        public Not() {
            super(PRECEDENCE_UNARY_OPERATION, "!", true);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.not(node2.evaluate(context));
        }

        @Override
        public String inspect(Node node1, Node node2) {
            return "!" + node2.inspect();
        }
    }

    public static class Minus extends Operator {

        public Minus() {
            super(PRECEDENCE_UNARY_OPERATION, "-", false);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return Calculator.negate(node2.evaluate(context));
        }

        @Override
        public String inspect(Node node1, Node node2) {
            return "-" + node2.inspect();
        }
    }

    public static class Matcher extends Operator {

        public Matcher() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, Constants.Operators.MATCH, true);
        }

        @Override
        public Object calculate(Node node1, Node node2, RuntimeContext context) {
            return node2.judge(node1, this, context);
        }
    }
}
