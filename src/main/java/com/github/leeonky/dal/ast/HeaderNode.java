package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.OperatorFactory;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Comparator;

public class HeaderNode extends Node {
    private final SequenceNode sequence;
    private final Node property;
    private final Operator operator;

    public HeaderNode(SequenceNode sequence, Node property, Operator operator) {
        this.sequence = sequence;
        this.property = property;
        this.operator = operator;
    }

    @Override
    public String inspect() {
        return sequence.inspect() + operator.inspect(property.inspect(), "").trim();
    }

    public Node getProperty() {
        return property;
    }

    public Operator getOperator() {
        return operator;
    }

    public OperatorFactory defaultHeaderOperator() {
        return tokenParser -> operator;
    }

    public SequenceNode getSequence() {
        return sequence;
    }

    public Comparator<Object> getListComparator(RuntimeContextBuilder.RuntimeContext context) {
        return sequence.getComparator(o -> context.newThisScope(context.wrap(o), () -> property.evaluate(context)));
    }

    public static Comparator<HeaderNode> bySequence() {
        return Comparator.<HeaderNode>comparingInt(node -> node.sequence.getValue()).reversed();
    }
}
