package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.AssertionFailure;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.util.DataObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.AssertionFailure.assertListSize;
import static java.lang.String.format;

public class ListNode extends Node {
    private final List<Expression> expressions = new ArrayList<>();

    public ListNode(List<Expression> expressions) {
        this.expressions.addAll(expressions);
    }

    public ListNode() {
        this(Collections.emptyList());
    }

    public List<Expression> getExpressions() {
        return expressions;
    }

    @Override
    public String inspect() {
        return format("[%s]", expressions.stream().map(Expression::getRightOperand)
                .map(Node::inspect).collect(Collectors.joining(" ")));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        DataObject dataObject = actualNode.evaluateDataObject(context);
        if (dataObject.isNull())
            throw new AssertionFailure(String.format("expected [null] equal to [%s] but was not", inspect()),
                    getPositionBegin());
        return judgeAll(context, dataObject);
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        DataObject dataObject = actualNode.evaluateDataObject(context);
        if (dataObject.isNull())
            throw new AssertionFailure(String.format("expected [null] matches [%s] but was not", inspect()),
                    getPositionBegin());
        return judgeAll(context, dataObject);
    }

    private boolean judgeAll(RuntimeContext context, DataObject dataObject) {
        assertListSize(expressions.size(), dataObject.getListSize(), getPositionBegin());
        return context.newThisScope(dataObject,
                () -> expressions.stream().allMatch(expression -> (boolean) expression.evaluate(context)));
    }
}
