package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.ExpressionClause;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static com.github.leeonky.dal.ast.HeaderNode.bySequence;
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
        return judgeRows(actualNode, operator, context);
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContextBuilder.RuntimeContext context) {
        return judgeRows(actualNode, operator, context);
    }

    private boolean judgeRows(Node actualNode, Operator operator, RuntimeContextBuilder.RuntimeContext context) {
        return new ListNode(rows.stream().<ExpressionClause>map(cells ->
                input -> isEllipsis(cells) ? cells.get(0) : new Expression(input, operator,
                        isRowWildcard(cells) ? cells.get(0) : new ObjectNode(cells))).collect(toList()), true)
                .judgeAll(context, actualNode.evaluateDataObject(context).setListComparator(collectComparator(context)));
    }

    private boolean isEllipsis(List<Node> cells) {
        return cells.size() == 1 && cells.get(0) instanceof ListEllipsisNode;
    }

    private boolean isRowWildcard(List<Node> cells) {
        return cells.size() == 1 && cells.get(0) instanceof WildcardNode;
    }

    private Comparator<Object> collectComparator(RuntimeContextBuilder.RuntimeContext context) {
        return headers.stream().sorted(bySequence())
                .map(headerNode -> headerNode.getListComparator(context))
                .reduce(Comparator::thenComparing)
                .orElse(SequenceNode.NOP_COMPARATOR);
    }
}
