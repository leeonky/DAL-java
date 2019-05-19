package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.ast.Node;

import java.math.BigDecimal;

public class SmallThan extends SymbolOperator {
    public SmallThan() {
        super("<");
    }

    @Override
    public Object calculate(CompilingContext context, Node node1, Node node2) {
        Object value1 = node1.evaluate(context);
        Object value2 = node2.evaluate(context);

        return new BigDecimal(value1.toString()).compareTo(new BigDecimal(value2.toString())) < 0;
    }
}
