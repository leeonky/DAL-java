package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.node.table.TransposedTableBody;
import com.github.leeonky.dal.ast.node.table.TransposedTableHead;
import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
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
    public Data verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        Data actual = actualNode.evaluateData(context);
        try {
            return transpose().convertToVerificationNode(actual, operator, context).verify(actualNode, operator, context);
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
