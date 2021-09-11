package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.AssertionFailure;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.SchemaType;
import com.github.leeonky.dal.util.DataObject;

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
        Object actual = dataObject.getInstance();
        if (actual == null)
            throw new AssertionFailure(String.format("expected [null] equal to [%s] but was not", inspect()),
                    getPositionBegin());
        return judgeAll(context, actual, dataObject.schemaType);
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        DataObject dataObject = actualNode.evaluateDataObject(context);
        Object actual = dataObject.getInstance();
        if (actual == null)
            throw new AssertionFailure(String.format("expected [null] matches [%s] but was not", inspect()),
                    getPositionBegin());
        return judgeAll(context, actual, dataObject.schemaType);
    }

    private boolean judgeAll(RuntimeContext context, Object actual, SchemaType schemaType) {
        Object[] list = stream(context.wrap(actual).asList().spliterator(), false)
                .map(DataObject::getInstance).toArray();
        assertListSize(expressions.size(), list.length, getPositionBegin());
        return judgeAll(list, context, schemaType);
    }

    private boolean judgeAll(Object input, RuntimeContext context, SchemaType schemaType) {
        try {
            //TODO process sub schema
            context.wrappedValueStack.push(input);
            context.schemaTypesStack.push(schemaType);
            return expressions.stream().allMatch(expression -> (boolean) expression.evaluate(context));
        } finally {
            context.schemaTypesStack.pop();
            context.wrappedValueStack.pop();
        }
    }
}
