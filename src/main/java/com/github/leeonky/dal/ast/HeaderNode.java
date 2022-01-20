package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Operator;
import com.github.leeonky.interpreter.OperatorMatcher;

import java.util.Comparator;
import java.util.Optional;

public class HeaderNode extends DALNode {
    private final SequenceNode sequence;
    private final DALNode property;
    private final Optional<Operator<DALNode, DALRuntimeContext>> operator;

    public HeaderNode(SequenceNode sequence, DALNode property, Optional<Operator<DALNode, DALRuntimeContext>> operator) {
        this.sequence = sequence;
        this.property = property;
        this.operator = operator;
    }

    @Override
    public String inspect() {
        String inspect = property.inspect();
        return sequence.inspect() + operator.map(o -> o.inspect(inspect, "").trim()).orElse(inspect);
    }

    public DALNode getProperty() {
        return property;
    }

    public OperatorMatcher<DALNode, DALRuntimeContext> headerOperator() {
        return tokenParser -> operator;
    }

    public Comparator<Object> getListComparator(DALRuntimeContext context) {
        return sequence.getComparator(o -> context.newThisScope(context.wrap(o), () -> property.evaluate(context)));
    }

    public static Comparator<HeaderNode> bySequence() {
        return Comparator.comparing(headerNode -> headerNode.sequence, SequenceNode.comparator().reversed());
    }
}
