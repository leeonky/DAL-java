package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.node.SortGroupNode;
import com.github.leeonky.dal.ast.node.TableNode;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import static com.github.leeonky.dal.ast.node.InputNode.INPUT_NODE;
import static java.util.stream.Collectors.toList;

public class TableHeadRow extends DALNode {
    private final List<HeaderNode> headers;

    public TableHeadRow(List<DALNode> headers) {
        this.headers = headers.stream().map(HeaderNode.class::cast).collect(toList());
    }

    @Override
    public String inspect() {
        return TableNode.printLine(headers) + "\n";
    }

    public Comparator<Data> collectComparator(RuntimeContextBuilder.DALRuntimeContext context) {
        return headers.stream().sorted(HeaderNode.bySequence()).map(headerNode -> headerNode.comparator(context))
                .reduce(Comparator::thenComparing).orElse(SortGroupNode.NOP_COMPARATOR);
    }

    public HeaderNode getHeader(int index) {
        if (index >= headers.size())
            return new HeaderNode(SortGroupNode.NO_SEQUENCE, INPUT_NODE, Optional.empty());
        return headers.get(index);
    }

    public void checkDataCellSize(TableRowNode rowNode) {
        rowNode.checkSize(headers.size());
    }

    public TableHeadRow merge(TableHeadRow tableHeadRow) {
        return new TableHeadRow(new ArrayList<DALNode>() {{
            addAll(headers);
            addAll(tableHeadRow.headers);
        }});
    }
}