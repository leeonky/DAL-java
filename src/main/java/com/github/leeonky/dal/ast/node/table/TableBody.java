package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.interpreter.InterpreterException.Position.Type.ROW;
import static java.util.stream.Collectors.toList;

public class TableBody extends DALNode {
    private static final RowType EMPTY_TABLE_ROW_TYPE = new EmptyTableRowType();
    private final List<TableRowNode> rows;
    private final RowType rowType;

    public TableBody(List<? extends DALNode> rows) {
        this(rows, ROW);
    }

    public TableBody(List<? extends DALNode> rows, InterpreterException.Position.Type type) {
        this.rows = rows.stream().map(TableRowNode.class::cast).collect(toList());
        rowType = resolveRowType(type);
    }

    public RowType resolveRowType(InterpreterException.Position.Type type) {
        return rows.stream().reduce(EMPTY_TABLE_ROW_TYPE, (last, rowNode) -> {
            try {
                return rowNode.mergeRowTypeBy(last);
            } catch (IllegalArgumentException ignored) {
                throw new SyntaxException("Row index should be consistent", rowNode.getPositionBegin(), type)
                        .multiPosition(rows.get(0).getPositionBegin(), type);
            }
        }, RowType::merge);
    }

    @Override
    public String inspect() {
        return rows.stream().map(TableRowNode::inspect).collect(Collectors.joining("\n"));
    }

    public DALNode convertToVerificationNode(Data actual, DALOperator operator, Comparator<Data> comparator) {
        return rowType.constructVerificationNode(actual, rows.stream().map(rowNode ->
                rowNode.constructVerificationClause(operator, rowType)), comparator);
    }

    public TableRowNode dataRowSkipEllipsis(int indexSkipEllipsis) {
        return rows.stream().filter(TableRowNode::isData).collect(toList()).get(indexSkipEllipsis);
    }

    public TableBody checkFormat(TableHeadRow headRow) {
        rows.forEach(headRow::checkDataCellSize);
        return this;
    }
}