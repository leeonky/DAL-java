package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.InterpreterException;

import java.util.ArrayList;
import java.util.List;

import static java.util.stream.Collectors.toList;

public class GroupExpression extends DALNode {
    private final GroupNode groupNode;
    private List<DALNode> expressions = new ArrayList<>();
    private final List<String> inspects = new ArrayList<>();

    public GroupExpression(List<DALNode> group) {
        groupNode = new GroupNode(group);
        expressions.addAll(group);
        inspects.add(groupNode.inspect());
    }

    public DALNode appendClauseChain(Clause<DALRuntimeContext, DALNode> clause) {
        expressions = expressions.stream().map(clause::expression).collect(toList());
        inspects.add(clause.expression(InputNode.INPUT_NODE).inspect());
        return this;
    }

    @Override
    public Object evaluate(DALRuntimeContext context) {
        return new ArrayList<Object>() {{
            for (int i = 0; i < expressions.size(); i++)
                add(evaluateExpression(context, expressions.get(i), i));
        }};
    }

    private Object evaluateExpression(DALRuntimeContext context, DALNode expression, int index) {
        try {
            return expression.evaluate(context);
        } catch (InterpreterException e) {
            throw e.multiPosition(groupNode.elements.get(index).getOperandPosition(),
                    InterpreterException.Position.Type.CHAR);
        }
    }

    @Override
    public String inspect() {
        return String.join("", inspects);
    }
}
