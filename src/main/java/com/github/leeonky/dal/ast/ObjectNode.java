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

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        DataObject dataObject = actualNode.evaluateDataObject(context);
        if (dataObject.isNull())
            throw new AssertionFailure(format("expected [null] equal to [%s] but was not", inspect()), getPositionBegin());
        assertUnexpectedFields(collectUnexpectedFields(dataObject), operator.getPosition());
        return judgeAll(context, dataObject);
    }

    private Set<String> collectUnexpectedFields(DataObject dataObject) {
        Set<String> dataFields = new LinkedHashSet<>(dataObject.getFieldNames());
        dataFields.removeAll(expressions.stream().map(expression ->
                dataObject.filedNameFromAlias(((PropertyNode) expression.getLeftOperand()).getRootName()))
                .collect(Collectors.toSet()));
        return dataFields;
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        DataObject dataObject = actualNode.evaluateDataObject(context);
        if (dataObject.isNull())
            throw new AssertionFailure(format("expected [null] matches [%s] but was not", inspect()),
                    getPositionBegin());
        return judgeAll(context, dataObject);
    }

    private boolean judgeAll(RuntimeContext context, DataObject dataObject) {
        return context.newThisScope(dataObject,
                () -> expressions.stream().allMatch(expression -> (boolean) expression.evaluate(context)));
    }
}
