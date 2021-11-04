package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.OperatorMatcher;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Comparator;
import java.util.Optional;

public class HeaderNode extends Node {
    private final SequenceNode sequence;
    private final Node property;
    private final Optional<Operator> operator;

    public HeaderNode(SequenceNode sequence, Node property, Optional<Operator> operator) {
        this.sequence = sequence;
        this.property = property;
        this.operator = operator;
    }

    @Override
    public String inspect() {
        String inspect = property.inspect();
        return sequence.inspect() + operator.map(o -> o.inspect(inspect, "").trim()).orElse(inspect);
    }

    public Node getProperty() {
        return property;
    }

    public Optional<Operator> getOperator() {
        return operator;
    }

    public OperatorMatcher headerOperator() {
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
