package com.github.leeonky.interpreter;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.compiler.Constants;
import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public abstract class Operator<N extends Node<N, C>, C extends RuntimeContext<C>> {
    private static final int PRECEDENCE_LOGIC_COMBINATION_OPT = 200;
    private static final int PRECEDENCE_LOGIC_COMPARE_OPT = 210;
    private static final int PRECEDENCE_PLUS_SUB_OPT = 300;
    private static final int PRECEDENCE_MUL_DIV = 400;
    private static final int PRECEDENCE_UNARY_OPERATION = 500;
    private static final int PRECEDENCE_INDEX = 501;
    private final int precedence;
    protected final String inspect;
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

    public boolean isPrecedentThan(Operator<N, C> operator) {
        return precedence > operator.precedence;
    }

    public abstract Object calculate(N node1, N node2, C context);

    @Override
//    TODO tobe removed
    public boolean equals(Object obj) {
        return getClass().isInstance(obj);
    }

    public int getPosition() {
        return position;
    }

    public Operator<N, C> setPosition(int position) {
        this.position = position;
        return this;
    }

    public String inspect(String node1, String node2) {
        if (node1 == null || node1.isEmpty())
            return String.format("%s %s", inspect, node2);
        return String.format("%s %s %s", node1, inspect, node2);
    }

    public static class Equal extends Operator<DALNode, DALRuntimeContext> {
        public Equal() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "=", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return node2.judge(node1, this, context);
        }

        @Override
        public String inspect(String node1, String node2) {
            return String.format("%s%s %s", node1, inspect, node2);
        }
    }

    public static class Less extends Operator<DALNode, DALRuntimeContext> {
        public Less() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "<", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context), context.getConverter()) < 0;
        }
    }

    public static class GreaterOrEqual extends Operator<DALNode, DALRuntimeContext> {
        public GreaterOrEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, ">=", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context), context.getConverter()) >= 0;
        }
    }

    public static class LessOrEqual extends Operator<DALNode, DALRuntimeContext> {
        public LessOrEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "<=", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context), context.getConverter()) <= 0;
        }
    }

    public static class NotEqual extends Operator<DALNode, DALRuntimeContext> {
        public NotEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "!=", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return !Calculator.equals(node1.evaluateDataObject(context), node2.evaluateDataObject(context));
        }
    }

    public static class Plus extends Operator<DALNode, DALRuntimeContext> {
        public Plus() {
            super(PRECEDENCE_PLUS_SUB_OPT, "+", false);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.plus(node1.evaluate(context), node2.evaluate(context), context.getConverter());
        }
    }

    public static class Greater extends Operator<DALNode, DALRuntimeContext> {
        public Greater() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, ">", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context), context.getConverter()) > 0;
        }
    }

    public static class Subtraction extends Operator<DALNode, DALRuntimeContext> {
        public Subtraction() {
            super(PRECEDENCE_PLUS_SUB_OPT, "-", false);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.subtract(node1.evaluate(context), node2.evaluate(context), context.getConverter());
        }
    }

    public static class Multiplication extends Operator<DALNode, DALRuntimeContext> {
        public Multiplication() {
            super(PRECEDENCE_MUL_DIV, "*", false);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.multiply(node1.evaluate(context), node2.evaluate(context), context.getConverter());
        }
    }

    public static class Division extends Operator<DALNode, DALRuntimeContext> {
        public Division() {
            super(PRECEDENCE_MUL_DIV, "/", false);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.divide(node1.evaluate(context), node2.evaluate(context), context.getConverter());
        }
    }

    public static class And extends Operator<DALNode, DALRuntimeContext> {
        public And(String inspect) {
            super(PRECEDENCE_LOGIC_COMBINATION_OPT, inspect, true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.and(() -> node1.evaluate(context), () -> node2.evaluate(context));
        }
    }

    public static class Or extends Operator<DALNode, DALRuntimeContext> {
        public Or(String inspect) {
            super(PRECEDENCE_LOGIC_COMBINATION_OPT, inspect, true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.or(() -> node1.evaluate(context), () -> node2.evaluate(context));
        }
    }

    public static class Not extends Operator<DALNode, DALRuntimeContext> {
        public Not() {
            super(PRECEDENCE_UNARY_OPERATION, "!", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.not(node2.evaluate(context));
        }

        @Override
        public String inspect(String node1, String node2) {
            return "!" + node2;
        }
    }

    public static class Minus extends Operator<DALNode, DALRuntimeContext> {

        public Minus() {
            super(PRECEDENCE_UNARY_OPERATION, "-", false);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.negate(node2.evaluate(context));
        }

        @Override
        public String inspect(String node1, String node2) {
            return "-" + node2;
        }
    }

    public static class Matcher extends Operator<DALNode, DALRuntimeContext> {

        public Matcher() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, Constants.Operators.MATCH, true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return node2.judge(node1, this, context);
        }

        @Override
        public String inspect(String node1, String node2) {
            return String.format("%s%s %s", node1, inspect, node2);
        }
    }
}
