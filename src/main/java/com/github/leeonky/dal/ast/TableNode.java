package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.ExpressionClause;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.ArrayList;
import java.util.List;

import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class TableNode extends Node {
    private final List<HeaderNode> headers;
    private final List<List<Node>> rows;

    public TableNode(List<HeaderNode> headers, List<List<Node>> rows) {
        this.headers = headers;
        this.rows = rows;
    }

    public List<HeaderNode> getHeaders() {
        return headers;
    }

    public List<List<Node>> getRows() {
        return rows;
    }

    @Override
    public String inspect() {
        return new ArrayList<List<String>>() {{
            add(headers.stream().map(HeaderNode::inspect).collect(toList()));
            addAll(rows.stream().map(cells -> cells.stream().map(Node::inspectClause)
                    .collect(toList())).collect(toList()));
        }}.stream().map(cells -> cells.stream().collect(joining(" | ", "| ", " |"))).collect(joining("\n"));
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContextBuilder.RuntimeContext context) {
        return toListNode(operator).judgeAll(context, actualNode.evaluateDataObject(context));
    }

    private ListNode toListNode(Operator operator) {
        return new ListNode(rows.stream().<ExpressionClause>map(cells -> input ->
                new Expression(input, operator, cellsToOneOperandNode(cells))).collect(toList()), true);
    }

    private Node cellsToOneOperandNode(List<Node> cells) {
        if (cells.isEmpty())
            return new WildcardNode("***");
        return new ObjectNode(cells);
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContextBuilder.RuntimeContext context) {
        return toListNode(operator).judge(actualNode, operator, context);
    }
}
