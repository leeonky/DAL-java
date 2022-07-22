package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.Operator;

public abstract class DALOperator extends Operator<DALRuntimeContext, DALNode, DALOperator> {
    private static final int PRECEDENCE_WHICH = 100;
    private static final int PRECEDENCE_LOGICAL = 200;
    private static final int PRECEDENCE_COMPARISON = 210;
    private static final int PRECEDENCE_PLUS_SUB = 300;
    private static final int PRECEDENCE_MUL_DIV = 400;
    private static final int PRECEDENCE_UNARY_OPERATION = 500;
    private static final int PRECEDENCE_PROPERTY = 501;
    private static final int PRECEDENCE_PARENTHESES = Integer.MAX_VALUE;
    private final boolean needInspect;

    protected DALOperator(int precedence, String label, boolean needInspect) {
        super(precedence, label);
        this.needInspect = needInspect;
    }

    public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
        return context.wrap(calculate(left, right, context));
    }

    @Override
    public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
        return calculateData(left, right, context).getInstance();
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

    public String inspect(String node1, String node2) {
        if (node1 == null || node1.isEmpty())
            return String.format("%s %s", label, node2);
        return String.format("%s %s %s", node1, label, node2);
    }

    public static class Equal extends DALOperator {
        public Equal() {
            super(PRECEDENCE_COMPARISON, "=", true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return left.verifyBy(right, this, context);
        }

        @Override
        public String inspect(String node1, String node2) {
            return String.format("%s%s %s", node1, label, node2);
        }
    }

    public static class Less extends DALOperator {
        public Less() {
            super(PRECEDENCE_COMPARISON, "<", true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.compare(left.evaluate(context), right.evaluate(context), context) < 0;
        }
    }

    public static class GreaterOrEqual extends DALOperator {
        public GreaterOrEqual() {
            super(PRECEDENCE_COMPARISON, ">=", true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.compare(left.evaluate(context), right.evaluate(context), context) >= 0;
        }
    }

    public static class LessOrEqual extends DALOperator {
        public LessOrEqual() {
            super(PRECEDENCE_COMPARISON, "<=", true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.compare(left.evaluate(context), right.evaluate(context), context) <= 0;
        }
    }

    public static class NotEqual extends DALOperator {
        public NotEqual() {
            super(PRECEDENCE_COMPARISON, "!=", true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return !Calculator.equals(left.evaluateData(context), right.evaluateData(context));
        }
    }

    public static class Plus extends DALOperator {
        public Plus() {
            super(PRECEDENCE_PLUS_SUB, "+", false);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.plus(left.evaluate(context), right.evaluate(context), context);
        }
    }

    public static class Greater extends DALOperator {
        public Greater() {
            super(PRECEDENCE_COMPARISON, ">", true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.compare(left.evaluate(context), right.evaluate(context), context) > 0;
        }
    }

    public static class Subtraction extends DALOperator {
        public Subtraction() {
            super(PRECEDENCE_PLUS_SUB, "-", false);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.subtract(left.evaluate(context), right.evaluate(context), context);
        }
    }

    public static class Multiplication extends DALOperator {
        public Multiplication() {
            super(PRECEDENCE_MUL_DIV, "*", false);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.multiply(left.evaluate(context), right.evaluate(context), context);
        }
    }

    public static class Division extends DALOperator {
        public Division() {
            super(PRECEDENCE_MUL_DIV, "/", false);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.divide(left.evaluate(context), right.evaluate(context), context);
        }
    }

    public static class And extends DALOperator {
        public And(String inspect) {
            super(PRECEDENCE_LOGICAL, inspect, true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.and(() -> left.evaluate(context), () -> right.evaluate(context));
        }
    }

    public static class Or extends DALOperator {
        public Or(String inspect) {
            super(PRECEDENCE_LOGICAL, inspect, true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.or(() -> left.evaluate(context), () -> right.evaluate(context));
        }
    }

    public static class Not extends DALOperator {
        public Not() {
            super(PRECEDENCE_UNARY_OPERATION, "!", true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.not(right.evaluate(context));
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
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.negate(right.evaluateData(context), context);
        }

        @Override
        public String inspect(String node1, String node2) {
            return "-" + node2;
        }
    }

    public static class Positive extends DALOperator {
        public Positive() {
            super(PRECEDENCE_UNARY_OPERATION, "+", false);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return Calculator.positive(right.evaluateData(context), context);
        }

        @Override
        public String inspect(String node1, String node2) {
            return "+" + node2;
        }
    }

    public static class Matcher extends DALOperator {
        public Matcher() {
            super(PRECEDENCE_COMPARISON, Notations.Operators.MATCHER.getLabel(), true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return left.verifyBy(right, this, context);
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
        public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
            return right.evaluateData(context);
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
        public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
            Data data = left.evaluateData(context);
            if (data.isNull())
                throw new RuntimeException("Instance is null", right.getOperandPosition());
            return ((ExecutableNode) right).getValue(data, context);
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

    public static class PropertySlash extends Property {
        public PropertySlash() {
            super(PRECEDENCE_PROPERTY, "/", false);
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

    public static class PropertyMeta extends Property {
        public PropertyMeta() {
            super(PRECEDENCE_PROPERTY, "::", false);

        }

        @Override
        public String inspect(String node1, String node2) {
            return String.format("%s%s%s", node1, label, node2);
        }

        @Override
        public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
            return context.metaProperty(left, right);
        }
    }

    public static class Which extends DALOperator {
        public Which() {
            super(PRECEDENCE_WHICH, Notations.Operators.WHICH.getLabel(), true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            try {
                return left.evaluateData(context).newBlockScope(() -> right.evaluate(context));
            } catch (IllegalStateException e) {
                throw new RuntimeException(e.getMessage(), getPosition());
            }
        }
    }

    public static class Is extends DALOperator {
        public Is() {
            super(PRECEDENCE_COMPARISON, Notations.Operators.IS.getLabel(), true);
        }

        @Override
        public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
            return ((SchemaComposeNode) right).verify(left, context);
        }
    }
}
