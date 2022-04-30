package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.table.TransposedTableBody;
import com.github.leeonky.dal.ast.table.TransposedTableHead;
import com.github.leeonky.dal.runtime.Data;
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
    protected boolean verify(Data actual, DALOperator.Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        try {
            return transpose().transformToVerificationNode(actual, operator, context).verify(actual, operator, context, actualNode);
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.columnPositionException(this);
        }
    }

    @Override
    protected boolean verify(Data actual, DALOperator.Equal operator, DALRuntimeContext context, DALNode actualNode) {
        try {
            return transpose().transformToVerificationNode(actual, operator, context).verify(actual, operator, context, actualNode);
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
