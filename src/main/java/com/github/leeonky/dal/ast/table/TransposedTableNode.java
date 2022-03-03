package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class TransposedTableNode extends DALNode {
    private final TransposedTableHead tableHead;
    private final TransposedTableBody tableBody;

    public TransposedTableNode(DALNode transposeTableHead, DALNode transposedTableBody) {
        tableHead = (TransposedTableHead) transposeTableHead;
        tableBody = ((TransposedTableBody) transposedTableBody).checkTable(tableHead);
    }

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Equal operator, DALRuntimeContext context) {
        return verifyAsListAndReThrow(actualNode, operator, context);
    }

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Matcher operator, DALRuntimeContext context) {
        return verifyAsListAndReThrow(actualNode, operator, context);
    }

    private boolean verifyAsListAndReThrow(DALNode actualNode, DALOperator operator, DALRuntimeContext context) {
        try {
            return transpose().verifyAsList(actualNode, operator, context);
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.columnPositionException(this);
        }
    }

    public TableNode transpose() {
        return new TableNode(tableBody.transposeHead(), tableBody.transpose(tableHead));
    }

    @Override
    public String inspect() {
        return tableHead.inspect() + tableBody.inspect();
    }
}
