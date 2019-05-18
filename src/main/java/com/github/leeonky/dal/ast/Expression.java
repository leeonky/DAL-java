package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.ast.opt.Operator;

public class Expression implements Node {
    private final Node node1, node2;
    private final Operator operator;

    public Expression(Node node1, Node node2, Operator operator) {
        this.node1 = node1;
        this.node2 = node2;
        this.operator = operator;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        return operator.calculate(context, node1, node2);
    }
}
