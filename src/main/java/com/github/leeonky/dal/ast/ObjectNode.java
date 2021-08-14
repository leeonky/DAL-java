package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.util.DataObject;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class ObjectNode extends Node {

    private final List<Expression> expressions = new ArrayList<>();

    @Override
    public String inspect() {
        return String.format("{%s}", expressions.stream().map(Expression::inspect).collect(Collectors.joining(" ")));
    }

    @Override
    public boolean judge(Operator.Equal operator, Object input, RuntimeContext context) {
        DataObject data = context.wrap(input);
        Set<String> dataFields = new LinkedHashSet<>(data.getPropertyReaderNames());
        dataFields.removeAll(expressions.stream().map(expression -> expression.getPropertyNode1().getName()).collect(Collectors.toSet()));
        if (!dataFields.isEmpty()) {
            System.err.printf("Warning: unexpected fields: %s\n", dataFields);
            return false;
        }
        return expressions.stream().allMatch(expression -> (boolean) expression.evaluate(context));
    }

    @Override
    public boolean judge(Operator.Matcher operator, Object input, RuntimeContext context) {
        return input != null;
    }

    public void addJudgements(Expression expression) {
        expressions.add(expression);
    }
}
