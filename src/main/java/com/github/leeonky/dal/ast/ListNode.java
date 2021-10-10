package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.DataObject;
import com.github.leeonky.dal.runtime.RuntimeContext;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.ast.AssertionFailure.assertListSize;
import static java.lang.String.format;

public class ListNode extends Node {
    // TODO New type JudgementExpression
    private final List<Expression> expressions;
    private final boolean incomplete;

    public ListNode(List<Expression> expressions) {
        long count = expressions.stream().filter(Objects::isNull).count();
        incomplete = count == 1;
        this.expressions = expressions.stream().filter(Objects::nonNull).collect(Collectors.toList());

    }

    public ListNode() {
        this(Collections.emptyList());
    }

    public List<Expression> getExpressions() {
        return expressions;
    }

    @Override
    public String inspect() {
        return format("[%s%s]", expressions.stream().map(Expression::getRightOperand)
                .map(Node::inspect).collect(Collectors.joining(" ")), incomplete ? " ..." : "");
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        return judgeAll(context, actualNode.evaluateDataObject(context));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        return judgeAll(context, actualNode.evaluateDataObject(context));
    }

    private boolean judgeAll(RuntimeContext context, DataObject dataObject) {
        if (!dataObject.isList())
            throw new RuntimeException(format("cannot compare%sand list", dataObject.inspect()), getPositionBegin());
        if (!incomplete)
            assertListSize(expressions.size(), dataObject.getListSize(), getPositionBegin());
        return context.newThisScope(dataObject, () -> expressions.stream()
                .allMatch(expression -> (boolean) expression.evaluate(context)));
    }
}
