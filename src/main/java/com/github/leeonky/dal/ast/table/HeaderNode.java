package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.SortSequenceNode;
import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.OperatorParser;

import java.util.Comparator;
import java.util.Optional;

public class HeaderNode extends DALNode {
    private final SortSequenceNode sequence;
    private final DALNode property;
    private final Optional<DALOperator> operator;

    public HeaderNode(SortSequenceNode sequence, DALNode property, Optional<DALOperator> operator) {
        this.sequence = sequence;
        this.property = property;
        this.operator = operator;
    }

    @Override
    public String inspect() {
        String property = this.property.inspect();
        return sequence.inspect() + operator.map(operator -> operator.inspect(property, "").trim()).orElse(property);
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

    public static Comparator<HeaderNode> bySequence() {
        return Comparator.comparing(headerNode -> headerNode.sequence, SortSequenceNode.comparator().reversed());
    }
}
