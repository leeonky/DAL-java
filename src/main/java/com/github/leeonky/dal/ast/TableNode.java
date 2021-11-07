package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static com.github.leeonky.dal.ast.HeaderNode.bySequence;
import static com.github.leeonky.dal.runtime.FunctionUtil.transpose;
import static com.github.leeonky.dal.runtime.FunctionUtil.zip;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class TableNode extends Node {
    private final List<HeaderNode> headers;
    private final List<RowNode> rows;
    private final Type type;

    public TableNode(List<HeaderNode> headers, List<RowNode> row) {
        this(headers, row, Type.NORMAL);
    }

    public TableNode(List<HeaderNode> headers, List<RowNode> row, Type type) {
        this.headers = new ArrayList<>(headers);
        rows = new ArrayList<>(row);
        this.type = type;
    }

    public List<HeaderNode> getHeaders() {
        return headers;
    }

    public List<RowNode> getRows() {
        return rows;
    }

    //        TODO refactor
    @Override
    public String inspect() {
        return type.inspect(headers, rows);
    }

    private String getString(List<HeaderNode> headers, List<RowNode> rows) {
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
        try {
            return new ListNode(rows.stream().map(rowNode -> rowNode.toExpressionClause(operator)).collect(toList()), true)
                    .judgeAll(context, actualNode.evaluateDataObject(context).setListComparator(collectComparator(context)));
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw type.toDalException(elementAssertionFailure, this);
        }
    }

    private Comparator<Object> collectComparator(RuntimeContextBuilder.RuntimeContext context) {
        return headers.stream().sorted(bySequence())
                .map(headerNode -> headerNode.getListComparator(context))
                .reduce(Comparator::thenComparing)
                .orElse(SequenceNode.NOP_COMPARATOR);
    }

    public enum Type {
        NORMAL {
            @Override
            protected DalException toDalException(ElementAssertionFailure elementAssertionFailure, TableNode tableNode) {
                return elementAssertionFailure.linePositionException();
            }

            @Override
            protected String inspect(List<HeaderNode> headers, List<RowNode> rows) {
                return String.join("\n", new ArrayList<String>() {{
                    add(headers.stream().map(HeaderNode::inspect).collect(joining(" | ", "| ", " |")));
                    rows.stream().map(RowNode::inspect).forEach(this::add);
                }});
            }
        }, TRANSPOSED {
            @Override
            protected DalException toDalException(ElementAssertionFailure elementAssertionFailure, TableNode tableNode) {
                return elementAssertionFailure.columnPositionException(tableNode);
            }

            @Override
            protected String inspect(List<HeaderNode> headers, List<RowNode> rows) {
                if (!rows.isEmpty()) {
                    return ">>" + zip(headers.stream().map(HeaderNode::inspect).collect(toList()).stream(),
                            transpose(rows.stream().map(RowNode::inspectCells).collect(toList())).stream(),
                            (h, cells) -> new ArrayList<String>() {{
                                add(h);
                                addAll(cells);
                            }}).map(l -> l.stream().collect(joining(" | ", "| ", " |"))).collect(joining("\n"));
                }
                return ">>" + headers.stream().map(HeaderNode::inspect).collect(joining(" |\n| ", "| ", " |"));
            }
        };

        protected abstract DalException toDalException(ElementAssertionFailure elementAssertionFailure, TableNode tableNode);

        protected abstract String inspect(List<HeaderNode> headers, List<RowNode> rows);
    }
}
