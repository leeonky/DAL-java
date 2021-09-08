package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.AssertionFailure;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.util.DataObject;

import java.util.*;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.AssertionFailure.assertUnexpectedFields;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class ObjectNode extends Node {

    private final List<Expression> expressions = new ArrayList<>();

    public ObjectNode(List<Expression> expressions) {
        this.expressions.addAll(expressions);
    }

    public ObjectNode() {
        this(Collections.emptyList());
    }

    @Override
    public String inspect() {
        return format("{%s}", expressions.stream().map(Expression::inspect).collect(joining(" ")));
    }

    //TODO refactor
    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        Object actual = actualNode.evaluate(context);
        if (actual == null)
            throw new AssertionFailure(String.format("expected [null] equal to [%s] but was not", inspect()),
                    getPositionBegin());
        DataObject data = context.wrap(actual);
        Set<String> dataFields = new LinkedHashSet<>(data.getPropertyReaderNames());
        //TODO property alias
        dataFields.removeAll(expressions.stream().map(expression -> ((PropertyNode) expression.getLeftOperand()).getRootName())
                .collect(Collectors.toSet()));
        assertUnexpectedFields(dataFields, operator.getPosition());
        return judgeAll(actual, context);
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        Object actual = actualNode.evaluate(context);
        if (actual == null)
            throw new AssertionFailure(String.format("expected [null] matches [%s] but was not", inspect()),
                    getPositionBegin());
        return judgeAll(actual, context);
    }

    private boolean judgeAll(Object actual, RuntimeContext context) {
        try {
            //TODO process sub schema
            context.wrappedValueStack.push(actual);
            return expressions.stream().allMatch(expression -> (boolean) expression.evaluate(context));
        } finally {
            context.wrappedValueStack.pop();
        }
    }
}
