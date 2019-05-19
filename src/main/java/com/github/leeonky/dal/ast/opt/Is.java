package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.Comparer;
import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.ast.Node;

public class Is extends Operator {
    public Is() {
        super("is", true);
    }

    @Override
    public Object calculate(CompilingContext context, Node node1, Node node2) {
        return Comparer.equals(node1.evaluate(context), node2.evaluate(context));
    }
}
