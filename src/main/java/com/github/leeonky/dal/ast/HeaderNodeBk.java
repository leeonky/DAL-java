package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.OperatorParser;

import java.util.Comparator;
import java.util.Optional;

@Deprecated
public class HeaderNodeBk extends DALNode {
    private final SortSequenceNode sequence;
    private final DALNode property;
    private final Optional<DALOperator> operator;

    public HeaderNodeBk(SortSequenceNode sequence, DALNode property, Optional<DALOperator> operator) {
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

    public OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> headerOperator() {
        return procedure -> operator;
    }

    public Comparator<Object> getListComparator(DALRuntimeContext context) {
        return sequence.getComparator(o -> context.newBlockScope(context.wrap(o), () -> property.evaluate(context)));
    }

    public static Comparator<HeaderNodeBk> bySequence() {
        return Comparator.comparing(headerNode -> headerNode.sequence, SortSequenceNode.comparator().reversed());
    }
}
