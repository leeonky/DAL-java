package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.node.TableNode;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.interpreter.InterpreterException.Position.Type.ROW;
import static java.util.Optional.empty;

public class TransposedRowHeaderRow extends DALNode {
    private final List<RowHeader> rowHeaders;

    public TransposedRowHeaderRow(List<DALNode> rowHeaders) {
        this.rowHeaders = rowHeaders.stream().map(RowHeader.class::cast).collect(Collectors.toList());
        if (!rowHeaders.isEmpty())
            setPositionBegin(rowHeaders.get(rowHeaders.size() - 1).getPositionBegin());
    }

    @Override
    public String inspect() {
        return "| >> " + TableNode.printLine(rowHeaders) + "\n";
    }

    public RowHeader getRowHeader(int i) {
        if (i >= rowHeaders.size())
            return new RowHeader(empty(), empty(), empty());
        return rowHeaders.get(i);
    }

    public void checkSize(TransposedRow row) {
        if (row.cellCount() != rowHeaders.size())
            throw new SyntaxException("Different cell size", getPositionBegin(), ROW);
    }
}
