package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.ListScopeNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.interpreter.InterpreterException.Position.Type.LINE;
import static java.util.stream.Collectors.toList;

public class TableBody extends DALNode {
    private final List<RowNode> rows;

    public TableBody(List<DALNode> rows) {
        this.rows = rows.stream().map(RowNode.class::cast).collect(toList());
        checkRowIndex();
    }

    private boolean isHasRowIndex() {
        return (!rows.isEmpty()) && rows.get(0).hasIndex();
    }

    private void checkRowIndex() {
        rows.stream().skip(1).filter(rowNode -> rows.get(0).hasIndex() != rowNode.hasIndex()).findAny().ifPresent(row -> {
            throw new SyntaxException("Row index should be consistent", row.getPositionBegin(), LINE)
                    .multiPosition(rows.get(0).getPositionBegin(), LINE);
        });
    }

    @Override
    public String inspect() {
        return rows.stream().map(RowNode::inspect).collect(Collectors.joining());
    }

    public ListScopeNode transformToListScope(DALOperator operator) {
        Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses = rows.stream().map(rowNode ->
                rowNode.verificationClause(operator));
        return isHasRowIndex() ? new ListScopeNode(rowClauses.map(rowNode -> rowNode.makeExpression(null))
                .collect(toList()), true, ListScopeNode.Type.FIRST_N_ITEMS)
                : new ListScopeNode(rowClauses.collect(Collectors.toList()), true);
    }
}