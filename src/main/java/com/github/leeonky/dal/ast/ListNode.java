package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.AssertionFailure.assertListSize;
import static java.lang.String.format;

public class ListNode extends Node {
    private final List<Expression> expressions = new ArrayList<>();

    @Override
    public String inspect() {
        return format("[%s]", expressions.stream().map(Expression::getRightOperand)
                .map(Node::inspect).collect(Collectors.joining(" ")));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        return judgeAll(actualNode, context, operator);
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        return judgeAll(actualNode, context, operator);
    }

    private boolean judgeAll(Node actualNode, RuntimeContext context, Operator operator) {
        Object actual = actualNode.evaluate(context);
        assertListSize(expressions.size(), context.wrap(actual).getListSize(), operator.getPosition());
        return judgeAll(actual, context);
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
