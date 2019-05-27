package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.ast.opt.Operator;

import java.util.Objects;

public class Expression implements Node {
    private final Node node1, node2;

    @Deprecated
    private final Operator operatorDp;

    private final com.github.leeonky.dal.ast.Operator operator;

    @Deprecated
    public Expression(Node node1, Node node2, Operator operatorDp) {
        this.node1 = node1;
        this.node2 = node2;
        this.operatorDp = operatorDp;
        operator = null;
    }

    public Expression(Node node1, Node node2, com.github.leeonky.dal.ast.Operator operator) {
        this.node1 = node1;
        this.node2 = node2;
        operatorDp = null;
        this.operator = operator;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        return operatorDp.calculate(context, node1, node2);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof Expression
                && Objects.equals(node1, ((Expression) obj).node1)
                && Objects.equals(node2, ((Expression) obj).node2)
                && Objects.equals(operator, ((Expression) obj).operator)
                && Objects.equals(operator, ((Expression) obj).operator);
    }
}
