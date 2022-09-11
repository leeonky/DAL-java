package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.interpreter.Operator;

public abstract class DALOperator extends Operator<DALRuntimeContext, DALNode, DALOperator> {
    static final int PRECEDENCE_WHICH = 100;
    static final int PRECEDENCE_LOGICAL = 200;
    static final int PRECEDENCE_COMPARISON = 210;
    static final int PRECEDENCE_PLUS_SUB = 300;
    static final int PRECEDENCE_MUL_DIV = 400;
    static final int PRECEDENCE_UNARY_OPERATION = 500;
    static final int PRECEDENCE_PROPERTY = 501;
    static final int PRECEDENCE_PARENTHESES = Integer.MAX_VALUE;
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

    public static class NotEqual extends DALOperator {
        public NotEqual() {
            super(PRECEDENCE_COMPARISON, "!=", true);
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return !Calculator.equals(left.evaluateData(context), right.evaluateData(context));
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

    public static class Property extends DALOperator {
        public Property(int precedence, String label, boolean needInspect) {
            super(precedence, label, needInspect);
        }

        @Override
        public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
            return ((ExecutableNode) right).getValue(left, context);
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
