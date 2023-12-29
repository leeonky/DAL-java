package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.node.table.TransposedBody;
import com.github.leeonky.dal.ast.node.table.TransposedRowHeaderRow;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ExpectationFactory;
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
    protected ExpectationFactory toVerify(DALRuntimeContext context) {
        return (operator, actual) -> {
            ExpectationFactory.Expectation expectation = transpose().convertToVerificationNode(actual, operator, context)
                    .toVerify(context).create(operator, actual);

            return new ExpectationFactory.Expectation() {
                @Override
                public Data matches() {
                    try {
                        return expectation.matches();
                    } catch (RowAssertionFailure rowAssertionFailure) {
                        throw rowAssertionFailure.columnPositionException(TransposedTableNode.this);
                    }
                }

                @Override
                public Data equalTo() {
                    try {
                        return expectation.equalTo();
                    } catch (RowAssertionFailure rowAssertionFailure) {
                        throw rowAssertionFailure.columnPositionException(TransposedTableNode.this);
                    }
                }

                @Override
                public ExpectationFactory.Type type() {
                    return expectation.type();
                }
            };
        };
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
