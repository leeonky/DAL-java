package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.InterpreterException;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toList;

public class GroupExpression extends DALNode {
    private final List<DALNode> group = new ArrayList<>();
    private final List<DALNode> expressions = new ArrayList<>();
    private final String inspect;

    public GroupExpression(List<DALNode> group) {
        this.group.addAll(group);
        expressions.addAll(group);
        inspect = this.group.stream().map(DALNode::inspect).collect(Collectors.joining(", ", "<<", ">>"));
    }

    public GroupExpression(List<DALNode> group, List<DALNode> expressions, String inspect) {
        this.group.addAll(group);
        this.expressions.addAll(expressions);
        this.inspect = inspect;
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
            throw e.multiPosition(group.get(index).getOperandPosition(),
                    InterpreterException.Position.Type.CHAR);
        }
    }

    @Override
    public String inspect() {
        return inspect;
    }

    public DALNode append(DALOperator operator, DALNode right, DALProcedure dalProcedure) {
        return new GroupExpression(group, expressions.stream()
                .map(e -> dalProcedure.createExpression(e, operator, right)).collect(toList()),
                operator.inspect(inspect, right.inspect()));
    }

    public DALNode insert(DALNode left, DALOperator operator, DALProcedure dalProcedure) {
        return new GroupExpression(group, expressions.stream()
                .map(e -> dalProcedure.createExpression(left, operator, e)).collect(toList()),
                operator.inspect(left.inspect(), inspect));
    }
}
