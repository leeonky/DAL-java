package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static com.github.leeonky.dal.ast.HeaderNode.bySequence;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class TableNode extends Node {
    private final List<HeaderNode> headers;
    private final List<RowNode> rows;

    public TableNode(List<HeaderNode> headers, List<RowNode> row) {
        this.headers = new ArrayList<>(headers);
        rows = new ArrayList<>(row);
    }

    public List<HeaderNode> getHeaders() {
        return headers;
    }

    public List<RowNode> getRows() {
        return rows;
    }

    @Override
    public String inspect() {
        return String.join("\n", new ArrayList<String>() {{
            add(headers.stream().map(HeaderNode::inspect).collect(joining(" | ", "| ", " |")));
            rows.stream().map(RowNode::inspect).forEach(this::add);
        }});
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
        return new ListNode(rows.stream().map(rowNode -> rowNode.toExpressionClause(operator)).collect(toList()), true)
                .judgeAll(context, actualNode.evaluateDataObject(context).setListComparator(collectComparator(context)));
    }

    private Comparator<Object> collectComparator(RuntimeContextBuilder.RuntimeContext context) {
        return headers.stream().sorted(bySequence())
                .map(headerNode -> headerNode.getListComparator(context))
                .reduce(Comparator::thenComparing)
                .orElse(SequenceNode.NOP_COMPARATOR);
    }
}
