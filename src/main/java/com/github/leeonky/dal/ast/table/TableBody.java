package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.ast.table.RowKey.RowKeyType.EMPTY_TABLE_ROW_KEY;
import static com.github.leeonky.interpreter.FunctionUtil.notAllowParallelReduce;
import static com.github.leeonky.interpreter.InterpreterException.Position.Type.LINE;
import static java.util.stream.Collectors.toList;

public class TableBody extends DALNode {
    private final List<RowNode> rows;
    private final RowKey.RowKeyType rowKeyType;

    public TableBody(List<? extends DALNode> rows) {
        this(rows, LINE);
    }

    public TableBody(List<? extends DALNode> rows, InterpreterException.Position.Type type) {
        this.rows = rows.stream().map(RowNode.class::cast).collect(toList());
        rowKeyType = resolveRowKeyType(type);
    }

    public RowKey.RowKeyType resolveRowKeyType(InterpreterException.Position.Type type) {
        return rows.stream().reduce(EMPTY_TABLE_ROW_KEY, (last, rowNode) -> {
            try {
                return rowNode.combineRowKey(last);
            } catch (IllegalArgumentException ignored) {
                throw new SyntaxException("Row index should be consistent", rowNode.getPositionBegin(), type)
                        .multiPosition(rows.get(0).getPositionBegin(), type);
            }
        }, notAllowParallelReduce());
    }

    @Override
    public String inspect() {
        return rows.stream().map(RowNode::inspect).collect(Collectors.joining());
    }

    public DALNode transformToListScope(DALOperator operator, Comparator<Object> comparator) {
        return rowKeyType.transformToVerificationNode(operator, rows, comparator);
    }

    public RowNode getDataRowByDataIndex(int row) {
        return rows.stream().filter(RowNode::isData).collect(toList()).get(row);
    }

    public TableBody checkTable(TableHead tableHead) {
        rows.forEach(tableHead::checkSize);
        return this;
    }
}