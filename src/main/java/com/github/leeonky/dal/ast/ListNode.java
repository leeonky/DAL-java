package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ListNode extends Node {
    private final List<Expression> expressions = new ArrayList<>();

    @Override
    public String inspect() {
        return String.format("[%s]", expressions.stream().map(Expression::getRightOperand)
                .map(Node::inspect).collect(Collectors.joining(" ")));
    }


    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        return judgeAll(actualNode, context);
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        return judgeAll(actualNode, context);
    }

    private boolean judgeAll(Node actualNode, RuntimeContext context) {
        Object actual = actualNode.evaluate(context);
        return context.wrap(actual).getListSize() == expressions.size() && judgeAll(actual, context);
    }

    private boolean judgeAll(Object input, RuntimeContext context) {
        if (input != null)
            try {
                //TODO process sub schema
                context.wrappedValueStack.push(input);
                return expressions.stream().allMatch(expression -> (boolean) expression.evaluate(context));
            } finally {
                context.wrappedValueStack.pop();
            }
        return false;
    }

    public void addJudgements(Expression expression) {
        expressions.add(expression);
    }
}
