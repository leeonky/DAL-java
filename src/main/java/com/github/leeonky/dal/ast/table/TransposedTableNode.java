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
        try {
            return transpose().transformToVerificationNode(operator, context).verify(actualNode, operator, context);
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.columnPositionException(this);
        }
    }

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Matcher operator, DALRuntimeContext context) {
        try {
            return transpose().transformToVerificationNode(operator, context).verify(actualNode, operator, context);
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.columnPositionException(this);
        }
    }

    public TableNode transpose() {
        return (TableNode) new TableNode(tableBody.transposeHead(), tableBody.transpose(tableHead))
                .setPositionBegin(getPositionBegin());
    }

    @Override
    public String inspect() {
        return tableHead.inspect() + tableBody.inspect();
    }

    @Override
    public DALNode setPositionBegin(int positionBegin) {
        return super.setPositionBegin(positionBegin);
    }
}
