package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.InterpreterException;

import java.util.ArrayList;
import java.util.List;

import static java.util.stream.Collectors.toList;

public class GroupExpression extends DALNode {
    private final GroupNode groupNode;
    private List<DALNode> expressions = new ArrayList<>();
    private String inspect;

    public GroupExpression(List<DALNode> group) {
        groupNode = new GroupNode(group);
        expressions.addAll(group);
        inspect = groupNode.inspect();
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
        return inspect;
    }

    public DALNode append(DALOperator operator, DALNode right, DALProcedure dalProcedure) {
        Clause<DALRuntimeContext, DALNode> clause = n -> dalProcedure.createExpression(n, operator, right);
        expressions = expressions.stream().map(clause::expression).collect(toList());
        inspect = operator.inspect(inspect, right.inspect());
        return this;
    }

    public DALNode insert(DALNode left, DALOperator operator, DALProcedure dalProcedure) {
        expressions = expressions.stream().map(e -> dalProcedure.createExpression(left, operator, e)).collect(toList());
        inspect = operator.inspect(left.inspect(), inspect);
        return this;
    }
}
