package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.AssertionFailure.assertListSize;
import static java.lang.String.format;
import static java.util.stream.StreamSupport.stream;

public class ListNode extends Node {
    private final List<Expression> expressions = new ArrayList<>();

    public ListNode(List<Expression> expressions) {
        this.expressions.addAll(expressions);
    }

    public ListNode() {
        this(Collections.emptyList());
    }

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
        Object[] list = stream(context.wrap(actualNode.evaluate(context)).getList().spliterator(), false).toArray();
        assertListSize(expressions.size(), list.length, operator.getPosition());
        return judgeAll(list, context);
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
}
