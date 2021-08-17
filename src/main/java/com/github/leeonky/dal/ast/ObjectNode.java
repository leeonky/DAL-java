package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.AssertionFailure;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.util.DataObject;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class ObjectNode extends Node {

    private final List<Expression> expressions = new ArrayList<>();

    @Override
    public String inspect() {
        return format("{%s}", expressions.stream().map(Expression::inspect).collect(joining(" ")));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        Object actual = actualNode.evaluate(context);
        DataObject data = context.wrap(actual);
        Set<String> dataFields = new LinkedHashSet<>(data.getPropertyReaderNames());
        //TODO property chain
        //TODO property alias
        dataFields.removeAll(expressions.stream().map(expression -> expression.getPropertyNode1().getRootName()).collect(Collectors.toSet()));
        if (!dataFields.isEmpty()) {
            String unexpectedFields = dataFields.stream().map(s -> format("`%s`", s)).collect(joining(", "));
            throw new AssertionFailure("unexpected fields " + unexpectedFields, operator.getPosition());
        }
        return judgeAll(actual, context);
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        return judgeAll(actualNode.evaluate(context), context);
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
