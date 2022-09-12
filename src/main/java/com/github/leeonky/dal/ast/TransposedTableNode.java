package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.ast.table.TransposedTableBody;
import com.github.leeonky.dal.ast.table.TransposedTableHead;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RowAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class TransposedTableNode extends DALNode {
    private final TransposedTableHead tableHead;
    private final TransposedTableBody tableBody;

    public TransposedTableNode(DALNode transposeTableHead, DALNode transposedTableBody) {
        tableHead = (TransposedTableHead) transposeTableHead;
        tableBody = ((TransposedTableBody) transposedTableBody).checkFormat(tableHead);
    }

    @Override
    protected boolean verify(Data actual, Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        try {
            return transpose().convertToVerificationNode(actual, operator, context)
                    .verify(actual, operator, context, actualNode);
        } catch (RowAssertionFailure rowAssertionFailure) {
            throw rowAssertionFailure.columnPositionException(this);
        }
    }

    @Override
    protected boolean verify(Data actual, Equal operator, DALRuntimeContext context, DALNode actualNode) {
        try {
            return transpose().convertToVerificationNode(actual, operator, context)
                    .verify(actual, operator, context, actualNode);
        } catch (RowAssertionFailure rowAssertionFailure) {
            throw rowAssertionFailure.columnPositionException(this);
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
}
