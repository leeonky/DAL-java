package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.WildcardNode;
import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.Operators;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class Match extends DALOperator {
    public Match() {
        super(Precedence.VERIFICATION, Notations.Operators.MATCHER.getLabel(), true, Operators.MATCH);
    }

    @Override
    public Data calculateData(DALExpression expression, RuntimeContextBuilder.DALRuntimeContext context) {
        if (expression.right() instanceof WildcardNode)
            return context.wrap(true);
        return context.calculate(expression.left().evaluateData(context),
                expression.operator(), expression.right().evaluateData(context));
    }

    @Override
    public String inspect(String node1, String node2) {
        return String.format("%s%s %s", node1, label, node2);
    }
}
