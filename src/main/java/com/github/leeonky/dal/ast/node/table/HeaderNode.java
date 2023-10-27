package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.node.SortGroupNode;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.OperatorParser;

import java.util.Comparator;
import java.util.Optional;

public class HeaderNode extends DALNode {
    private final SortGroupNode sort;
    private final DALNode property;
    private final Optional<DALOperator> operator;

    public HeaderNode(SortGroupNode sort, DALNode property, Optional<DALOperator> operator) {
        this.sort = sort;
        this.property = property;
        this.operator = operator;
    }

    @Override
    public String inspect() {
        String property = this.property.inspect();
        return sort.inspect() + operator.map(operator -> operator.inspect(property, "").trim()).orElse(property);
    }

    public DALNode property() {
        return property;
    }

    public OperatorParser<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression> operator() {
        return procedure -> operator;
    }

    public Comparator<Object> comparator(DALRuntimeContext context) {
        return sort.comparator(o -> context.wrap(o).newBlockScope(() -> property.evaluate(context)));
    }

    public static Comparator<HeaderNode> bySequence() {
        return Comparator.comparing(headerNode -> headerNode.sort, SortGroupNode.comparator().reversed());
    }
}
