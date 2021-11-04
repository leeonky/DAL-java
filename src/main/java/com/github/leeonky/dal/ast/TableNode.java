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
    private final List<RowNode> rows;

    public TableNode(List<HeaderNode> headers, List<RowNode> row) {
        this.headers = new ArrayList<>(headers);
        rows = new ArrayList<>(row);
    }

    public List<HeaderNode> getHeaders() {
        return headers;
    }

    //    TODO move to row node
    public List<List<Node>> getRows() {
        return rows.stream().map(rows2 -> rows2.nodes).collect(toList());
    }

    //    TODO move to row node
    public List<ExpressionClause> getRowSchemas() {
        return rows.stream().map(rows2 -> rows2.expressionClause.orElse(null)).collect(toList());
    }

    //    TODO move to row node
    public List<Operator> getRowOperators() {
        return rows.stream().map(rows2 -> rows2.operator.orElse(null)).collect(toList());
    }

    @Override
    public String inspect() {
//        TODO to be refactor
        return new ArrayList<List<String>>() {{
            add(headers.stream().map(HeaderNode::inspect).collect(toList()));
            addAll(rows.stream().map(row -> row.nodes.stream().map(Node::inspectClause).collect(toList())
            ).collect(toList()));
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
