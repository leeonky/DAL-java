package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.DataObject;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.ast.AssertionFailure.assertUnexpectedFields;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class ObjectNode extends Node {
    private final List<Node> expressions = new ArrayList<>();

    public ObjectNode(List<Node> expressions) {
        this.expressions.addAll(expressions);
    }

    public List<Node> getExpressions() {
        return expressions;
    }

    public ObjectNode() {
        this(Collections.emptyList());
    }

    @Override
    public String inspect() {
        return format("{%s}", expressions.stream().map(Node::inspect).collect(joining(" ")));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContextBuilder.RuntimeContext context) {
        DataObject dataObject = actualNode.evaluateDataObject(context);
        checkNull(dataObject);
        if (dataObject.isList()) {
            AtomicInteger integer = new AtomicInteger(0);
            dataObject.getListObjects().forEach(element -> assertUnexpectedFields(collectUnexpectedFields(element),
                    actualNode.inspect() + format("[%d]", integer.getAndIncrement()), operator.getPosition()));
        } else
            assertUnexpectedFields(collectUnexpectedFields(dataObject), operator.getPosition());
        return judgeAll(context, dataObject);
    }

    private void checkNull(DataObject dataObject) {
        if (dataObject.isNull())
            throw new AssertionFailure("The input value is null", getPositionBegin());
    }

    private Set<String> collectUnexpectedFields(DataObject dataObject) {
        return new LinkedHashSet<String>(dataObject.getFieldNames()) {{
            removeAll(expressions.stream().map(expression ->
                    dataObject.firstFieldFromAlias(expression.getRootName()))
                    .collect(Collectors.toSet()));
        }};
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContextBuilder.RuntimeContext context) {
        DataObject dataObject = actualNode.evaluateDataObject(context);
        checkNull(dataObject);
        return judgeAll(context, dataObject);
    }

    private boolean judgeAll(RuntimeContextBuilder.RuntimeContext context, DataObject dataObject) {
        return context.newThisScope(dataObject, () -> {
            expressions.forEach(expression -> expression.evaluate(context));
            return true;
        });
    }
}
