package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.DataObject;
import com.github.leeonky.dal.runtime.RuntimeContext;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.ast.AssertionFailure.assertUnexpectedFields;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class ObjectNode extends Node {

    // TODO New type JudgementExpression
    private final List<Expression> expressions = new ArrayList<>();

    public ObjectNode(List<Expression> expressions) {
        this.expressions.addAll(expressions);
    }

    public List<Expression> getExpressions() {
        return expressions;
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
            throw new AssertionFailure("the input value is null", getPositionBegin());
        if (dataObject.isList()) {
            AtomicInteger integer = new AtomicInteger(0);
            dataObject.getListObjects().forEach(element -> assertUnexpectedFields(collectUnexpectedFields(element),
                    actualNode.inspect() + format("[%d]", integer.getAndIncrement()), operator.getPosition()));
        } else
            assertUnexpectedFields(collectUnexpectedFields(dataObject), operator.getPosition());
        return judgeAll(context, dataObject);
    }

    private Set<String> collectUnexpectedFields(DataObject dataObject) {
        Set<String> dataFields = new LinkedHashSet<>(dataObject.getFieldNames());
        dataFields.removeAll(expressions.stream().map(expression ->
                dataObject.firstFieldFromAlias(((PropertyNode) expression.getLeftOperand()).getRootName()))
                .collect(Collectors.toSet()));
        return dataFields;
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        DataObject dataObject = actualNode.evaluateDataObject(context);
        if (dataObject.isNull())
            throw new AssertionFailure("the input value is null", getPositionBegin());
        return judgeAll(context, dataObject);
    }

    private boolean judgeAll(RuntimeContext context, DataObject dataObject) {
        return context.newThisScope(dataObject,
                () -> expressions.stream().allMatch(expression -> (boolean) expression.evaluate(context)));
    }
}
