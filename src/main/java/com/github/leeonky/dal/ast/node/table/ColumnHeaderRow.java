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
import static com.github.leeonky.dal.ast.node.SortGroupNode.NOP_COMPARATOR;
import static java.util.stream.Collectors.toList;

public class ColumnHeaderRow extends DALNode {
    private final List<ColumnHeader> headers;

    public ColumnHeaderRow(List<DALNode> headers) {
        this.headers = headers.stream().map(ColumnHeader.class::cast).collect(toList());
    }

    @Override
    public String inspect() {
        return TableNode.printLine(headers) + "\n";
    }

    public Comparator<Data> collectComparator(RuntimeContextBuilder.DALRuntimeContext context) {
        return headers.stream().sorted(ColumnHeader.bySequence()).map(headerNode -> headerNode.comparator(context))
                .reduce((comparator, other) -> comparator == NOP_COMPARATOR ? other : comparator.thenComparing(other))
                .orElse(NOP_COMPARATOR);
    }

    public ColumnHeader getHeader(int index) {
        if (index >= headers.size())
            return new ColumnHeader(SortGroupNode.NO_SEQUENCE, INPUT_NODE, Optional.empty());
        return headers.get(index);
    }

    public void checkDataCellSize(Row rowNode) {
        rowNode.checkSize(headers.size());
    }

    public ColumnHeaderRow merge(ColumnHeaderRow columnHeaderRow) {
        return new ColumnHeaderRow(new ArrayList<DALNode>() {{
            addAll(headers);
            addAll(columnHeaderRow.headers);
        }});
    }
}