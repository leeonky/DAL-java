package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.ListScopeNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.table.RowKey.RowKeyType.EMPTY_TABLE_ROW_KEY;
import static com.github.leeonky.interpreter.FunctionUtil.notAllowParallelReduce;
import static com.github.leeonky.interpreter.InterpreterException.Position.Type.LINE;
import static java.util.stream.Collectors.toList;

public class TableBody extends DALNode {
    private final List<RowNode> rows;

    public TableBody(List<? extends DALNode> rows) {
        this.rows = rows.stream().map(RowNode.class::cast).collect(toList());
    }

    private boolean isHasRowIndex() {
        return (!rows.isEmpty()) && rows.get(0).hasIndex();
    }

    public TableBody checkPrefix(InterpreterException.Position.Type type) {
        RowKey.RowKeyType keyType = rows.stream().reduce(EMPTY_TABLE_ROW_KEY, (rowKeyType, rowNode) -> {
            RowKey.RowKeyType rowKeyType1 = rowNode.combineRowKey(rowKeyType);
            if (rowKeyType1 == null) {
                throw new SyntaxException("Row index should be consistent", rowNode.getPositionBegin(), type)
                        .multiPosition(rows.get(0).getPositionBegin(), type);
            }
            return rowKeyType1;
        }, notAllowParallelReduce());
        return this;
    }

    @Override
    public String inspect() {
        return rows.stream().map(RowNode::inspect).collect(Collectors.joining());
    }

    public ListScopeNode transformToListScope(DALOperator operator) {
        Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses = rows.stream().map(rowNode ->
                rowNode.verificationClause(operator));
        return isHasRowIndex() ? new ListScopeNode(rowClauses.map(rowNode -> rowNode.expression(null))
                .collect(toList()), true, ListScopeNode.Type.FIRST_N_ITEMS)
                : new ListScopeNode(rowClauses.collect(Collectors.toList()), true);
    }

    public RowNode getDataRowByDataIndex(int row) {
        return rows.stream().filter(RowNode::isData).collect(toList()).get(row);
    }

    public TableBody checkTable(TableHead tableHead) {
        rows.forEach(tableHead::checkSize);
        return checkPrefix(LINE);
    }
}