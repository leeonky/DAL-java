package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.node.table.TransposedBody;
import com.github.leeonky.dal.ast.node.table.TransposedRowHeaderRow;
import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Match;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RowAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class TransposedTableNode extends DALNode {
    private final TransposedRowHeaderRow tableHead;
    private final TransposedBody tableBody;

    public TransposedTableNode(DALNode transposeTableHead, DALNode transposedTableBody) {
        tableHead = (TransposedRowHeaderRow) transposeTableHead;
        tableBody = ((TransposedBody) transposedTableBody).checkFormat(tableHead);
    }

    @Override
    public Data verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        DelegateNode node = new DelegateNode(actualNode);
        Data actual = node.evaluateData(context);
        try {
            return transpose().convertToVerificationNode(actual, operator, context).verify(node, operator, context);
        } catch (RowAssertionFailure rowAssertionFailure) {
            throw rowAssertionFailure.columnPositionException(this);
        }
    }

    @Override
    public Data verify(DALNode actualNode, Match operator, DALRuntimeContext context) {
        DelegateNode node = new DelegateNode(actualNode);
        Data actual = node.evaluateData(context);
        try {
            return transpose().convertToVerificationNode(actual, operator, context).verify(node, operator, context);
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
