package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.ExpressionClause;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.List;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.joining;

public class TableNode extends Node {
    private final List<HeaderNode> headers;
    private final List<List<Node>> rowsCells;

    public TableNode(List<HeaderNode> headers, List<List<Node>> rowsCells) {
        this.headers = headers;
        this.rowsCells = rowsCells;
    }

    @Override
    public String inspect() {
        return headers.stream().map(HeaderNode::inspect).collect(joining(" | ", "| ", " |")) +
                rowsCells.stream().map(cells -> cells.stream().map(Node::inspectClause).collect(joining(" | ", "\n| ", " |")))
                        .collect(joining());
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContextBuilder.RuntimeContext context) {
        return toListNode(operator).judge(actualNode, operator, context);
    }

    private ListNode toListNode(Operator operator) {
        return new ListNode(rowsCells.stream().<ExpressionClause>map(cells -> input ->
                new Expression(input, operator, new ObjectNode(cells))).collect(Collectors.toList()));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContextBuilder.RuntimeContext context) {
        return toListNode(operator).judge(actualNode, operator, context);
    }
}
