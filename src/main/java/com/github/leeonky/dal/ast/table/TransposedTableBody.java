package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.github.leeonky.interpreter.InterpreterException.Position.Type.CHAR;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

public class TransposedTableBody extends DALNode {
    private final List<TransposedRowNode> rows;

    public TransposedTableBody(List<DALNode> rows) {
        this.rows = rows.stream().map(TransposedRowNode.class::cast).collect(toList());
    }

    public TransposedTableBody checkTable(TransposedTableHead tableHead) {
        checkCellSize(tableHead);
        replaceEmptyCell();
        transpose(tableHead).checkPrefix(CHAR);
        return this;
    }

    private void replaceEmptyCell() {
        rows.stream().skip(1).forEach(row -> row.replaceEmptyCell(rows.get(0)));
    }

    private void checkCellSize(TransposedTableHead tableHead) {
        rows.forEach(tableHead::checkSize);
        Map<Integer, List<TransposedRowNode>> rowsByCount = rows.stream().collect(groupingBy(TransposedRowNode::cellCount));
        if (rowsByCount.size() > 1)
            throw new SyntaxException("Different cell size", new ArrayList<>(rowsByCount.values()).get(1).get(0)
                    .getPositionBegin(), InterpreterException.Position.Type.LINE);
    }

    public TableBody transpose(TransposedTableHead tableHead) {
        return new TableBody(rows.stream().map(rowNode -> rowNode.transpose(tableHead))
                .reduce(this::merge).orElse(Collections.emptyList()));
    }

    private List<RowNode> merge(List<RowNode> rows1, List<RowNode> rows2) {
        return new ArrayList<RowNode>() {{
            for (int i = 0; i < rows1.size(); i++)
                add(rows1.get(i).merge(rows2.get(i)));
        }};
    }

    public TableHead transposeHead() {
        return new TableHead(rows.stream().map(TransposedRowNode::getHeader).collect(toList()));
    }

    @Override
    public String inspect() {
        return rows.stream().map(TransposedRowNode::inspect).collect(Collectors.joining("\n"));
    }
}