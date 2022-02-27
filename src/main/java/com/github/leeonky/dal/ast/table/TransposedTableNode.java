package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.List;

public class TransposedTableNode extends DALNode {
    private final TransposedTableHead tableHead;
    private final TransposedTableBody tableBody;

    public TransposedTableNode(DALNode transposeTableHead, List<DALNode> rows) {
        tableHead = (TransposedTableHead) transposeTableHead;
        tableBody = new TransposedTableBody(rows).checkTable(tableHead);
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Equal operator, DALRuntimeContext context) {
        return judgeAsListAndReThrow(actualNode, operator, context);
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Matcher operator, DALRuntimeContext context) {
        return judgeAsListAndReThrow(actualNode, operator, context);
    }

    private boolean judgeAsListAndReThrow(DALNode actualNode, DALOperator operator, DALRuntimeContext context) {
        try {
            return transpose().judgeAsList(actualNode, operator, context);
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
