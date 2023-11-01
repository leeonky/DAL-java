package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.DALNode;
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

public class TransposedBody extends DALNode {
    private final List<TransposedRow> rows;

    public TransposedBody(List<DALNode> rows) {
        this.rows = rows.stream().map(TransposedRow.class::cast).collect(toList());
    }

    public TransposedBody checkFormat(TransposedRowHeaderRow tableHead) {
        checkCellSize(tableHead);
        replaceEmptyCell();
        return this;
    }

    private void replaceEmptyCell() {
        TransposedRow firstRow = rows.get(0);
        rows.stream().skip(1).forEach(row -> row.replaceEmptyCell(firstRow));
    }

    private void checkCellSize(TransposedRowHeaderRow tableHead) {
        rows.forEach(tableHead::checkSize);
        Map<Integer, List<TransposedRow>> rowsByCount = rows.stream().collect(groupingBy(TransposedRow::cellCount));
        if (rowsByCount.size() > 1)
            throw new SyntaxException("Different cell size", new ArrayList<>(rowsByCount.values()).get(1).get(0)
                    .getPositionBegin(), InterpreterException.Position.Type.ROW);
    }

    public Body transpose(TransposedRowHeaderRow tableHead) {
        return new Body(rows.stream().map(rowNode -> rowNode.transpose(tableHead))
                .reduce(this::merge).orElse(Collections.emptyList()), CHAR);
    }

    private List<Row> merge(List<Row> rows1, List<Row> rows2) {
        return new ArrayList<Row>() {{
            for (int i = 0; i < rows1.size(); i++)
                add(rows1.get(i).merge(rows2.get(i)));
        }};
    }

    public ColumnHeaderRow transposeHead() {
        return new ColumnHeaderRow(rows.stream().map(TransposedRow::getHeader).collect(toList()));
    }

    @Override
    public String inspect() {
        return rows.stream().map(TransposedRow::inspect).collect(Collectors.joining("\n"));
    }
}