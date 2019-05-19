package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.Comparer;
import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.ast.Node;

public class Equal extends Operator {

    public Equal() {
        super("=");
    }

    @Override
    public Object calculate(CompilingContext context, Node node1, Node node2) {
        return Comparer.equals(node1.evaluate(context), node2.evaluate(context));
    }
}
