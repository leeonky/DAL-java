package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.SortGroupNode;
import com.github.leeonky.dal.ast.TableNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import static com.github.leeonky.dal.ast.table.HeaderNode.bySequence;
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

    public Comparator<Object> collectComparator(RuntimeContextBuilder.DALRuntimeContext context) {
        return headers.stream().sorted(bySequence()).map(headerNode -> headerNode.comparator(context))
                .reduce(Comparator::thenComparing).orElse(SortGroupNode.NOP_COMPARATOR);
    }

    public HeaderNode getHeader(int index) {
        if (index >= headers.size())
            return new HeaderNode(SortGroupNode.NO_SEQUENCE, InputNode.INSTANCE, Optional.empty());
        return headers.get(index);
    }

    public void checkDataCellSize(TableRowNode rowNode) {
        if (!rowNode.specialRow() && rowNode.getCells().size() != headers.size())
            throw new SyntaxException("Different cell size",
                    rowNode.getCells().get(rowNode.getCells().size() - 1).getOperandPosition());
    }
}