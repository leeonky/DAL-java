package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Operator;

public abstract class DALOperator extends Operator<DALRuntimeContext, DALNode, DALOperator> {
    private static final int PRECEDENCE_LOGIC_COMBINATION_OPT = 200;
    private static final int PRECEDENCE_LOGIC_COMPARE_OPT = 210;
    private static final int PRECEDENCE_PLUS_SUB_OPT = 300;
    private static final int PRECEDENCE_MUL_DIV = 400;
    private static final int PRECEDENCE_UNARY_OPERATION = 500;
    private static final int PRECEDENCE_PROPERTY = 501;
    private static final int PRECEDENCE_PARENTHESES = Integer.MAX_VALUE;
    private final boolean needInspect;

    protected DALOperator(int precedence, String label, boolean needInspect) {
        super(precedence, label);
        this.needInspect = needInspect;
    }

    public Data calculateData(DALNode node1, DALNode node2, DALRuntimeContext context) {
        return context.wrap(calculate(node1, node2, context));
    }

    @Override
    public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
        return calculateData(node1, node2, context).getInstance();
    }

    public static And operatorAnd() {
        return new And(Notations.Operators.AND.getLabel());
    }

    public static Or operatorOr() {
        return new Or(Notations.Operators.OR.getLabel());
    }

    public static And keywordAnd() {
        return new And(Notations.Keywords.AND.getLabel());
    }

    public static And commaAnd() {
        return new And(",");
    }

    public static Or keywordOr() {
        return new Or(Notations.Keywords.OR.getLabel());
    }

    public boolean isNeedInspect() {
        return needInspect;
    }

    @Override
//    TODO tobe removed
    public boolean equals(Object obj) {
        return getClass().isInstance(obj);
    }

    public String inspect(String node1, String node2) {
        if (node1 == null || node1.isEmpty())
            return String.format("%s %s", label, node2);
        return String.format("%s %s %s", node1, label, node2);
    }

    public static class Equal extends DALOperator {
        public Equal() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "=", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return node2.judge(node1, this, context);
        }

        @Override
        public String inspect(String node1, String node2) {
            return String.format("%s%s %s", node1, label, node2);
        }
    }

    public static class Less extends DALOperator {
        public Less() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "<", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context), context.getConverter()) < 0;
        }
    }

    public static class GreaterOrEqual extends DALOperator {
        public GreaterOrEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, ">=", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context), context.getConverter()) >= 0;
        }
    }

    public static class LessOrEqual extends DALOperator {
        public LessOrEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "<=", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context), context.getConverter()) <= 0;
        }
    }

    public static class NotEqual extends DALOperator {
        public NotEqual() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, "!=", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return !Calculator.equals(node1.evaluateDataObject(context), node2.evaluateDataObject(context));
        }
    }

    public static class Plus extends DALOperator {
        public Plus() {
            super(PRECEDENCE_PLUS_SUB_OPT, "+", false);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.plus(node1.evaluate(context), node2.evaluate(context), context.getConverter());
        }
    }

    public static class Greater extends DALOperator {
        public Greater() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, ">", true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.compare(node1.evaluate(context), node2.evaluate(context), context.getConverter()) > 0;
        }
    }

    public static class Subtraction extends DALOperator {
        public Subtraction() {
            super(PRECEDENCE_PLUS_SUB_OPT, "-", false);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.subtract(node1.evaluate(context), node2.evaluate(context), context.getConverter());
        }
    }

    public static class Multiplication extends DALOperator {
        public Multiplication() {
            super(PRECEDENCE_MUL_DIV, "*", false);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.multiply(node1.evaluate(context), node2.evaluate(context), context.getConverter());
        }
    }

    public static class Division extends DALOperator {
        public Division() {
            super(PRECEDENCE_MUL_DIV, "/", false);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.divide(node1.evaluate(context), node2.evaluate(context), context.getConverter());
        }
    }

    public static class And extends DALOperator {
        public And(String inspect) {
            super(PRECEDENCE_LOGIC_COMBINATION_OPT, inspect, true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.and(() -> node1.evaluate(context), () -> node2.evaluate(context));
        }
    }

    public static class Or extends DALOperator {
        public Or(String inspect) {
            super(PRECEDENCE_LOGIC_COMBINATION_OPT, inspect, true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return Calculator.or(() -> node1.evaluate(context), () -> node2.evaluate(context));
        }
    }

    public static class Not extends DALOperator {
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

    public static class Minus extends DALOperator {

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

    public static class Matcher extends DALOperator {

        public Matcher() {
            super(PRECEDENCE_LOGIC_COMPARE_OPT, Notations.Operators.MATCHER.getLabel(), true);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return node2.judge(node1, this, context);
        }

        @Override
        public String inspect(String node1, String node2) {
            return String.format("%s%s %s", node1, label, node2);
        }
    }

    public static class Parentheses extends DALOperator {
        public Parentheses() {
            super(PRECEDENCE_PARENTHESES, "", false);
        }

        @Override
        public Data calculateData(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return node2.evaluateDataObject(context);
        }

        @Override
        public String inspect(String node1, String node2) {
            return String.format("(%s)", node2);
        }
    }

    public static class Property extends DALOperator {
        public Property(int precedence, String label, boolean needInspect) {
            super(precedence, label, needInspect);
        }

        @Override
        public Data calculateData(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return ((SymbolNode) node2).getPropertyValue(node1, context);
        }

        @Override
        public Object calculate(DALNode node1, DALNode node2, DALRuntimeContext context) {
            return calculateData(node1, node2, context).getInstance();
        }
    }

    public static class PropertyDot extends Property {

        public PropertyDot() {
            super(PRECEDENCE_PROPERTY, ".", false);
        }

        @Override
        public String inspect(String node1, String node2) {
            return String.format("%s%s%s", node1, label, node2);
        }
    }

    public static class PropertyImplicit extends Property {

        public PropertyImplicit() {
            super(PRECEDENCE_PROPERTY, "", false);
        }

        @Override
        public String inspect(String node1, String node2) {
            return String.format("%s%s", node1, node2);
        }
    }
}
