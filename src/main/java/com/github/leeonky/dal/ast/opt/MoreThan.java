package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.Comparer;
import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.ast.Node;

public class MoreThan extends SymbolOperator {
    public MoreThan() {
        super(">");
    }

    @Override
    public Object calculate(CompilingContext context, Node node1, Node node2) {
        return Comparer.compare(node1.evaluate(context), node2.evaluate(context)) > 0;
    }
}
