package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.node.table.Body;
import com.github.leeonky.dal.ast.node.table.ColumnHeaderRow;
import com.github.leeonky.dal.ast.node.table.Row;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ExpectationFactory;
import com.github.leeonky.dal.runtime.RowAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.List;
import java.util.stream.Collectors;

public class TableNode extends DALNode {
    private final ColumnHeaderRow columnHeaderRow;
    private final Body body;

    public TableNode(ColumnHeaderRow columnHeaderRow, Body body) {
        this.columnHeaderRow = columnHeaderRow;
        this.body = body.checkFormat(this.columnHeaderRow);
        setPositionBegin(columnHeaderRow.getPositionBegin());
    }

    @Override
    protected ExpectationFactory toVerify(DALRuntimeContext context) {
        return (operator, actual) -> {
            ExpectationFactory.Expectation verificationExpectation = convertToVerificationNode(actual, operator,
                    context).toVerify(context).create(operator, actual);
            return new ExpectationFactory.Expectation() {
                @Override
                public Data matches() {
                    try {
                        return verificationExpectation.matches();
                    } catch (RowAssertionFailure rowAssertionFailure) {
                        throw rowAssertionFailure.linePositionException(TableNode.this);
                    }
                }

                @Override
                public Data equalTo() {
                    try {
                        return verificationExpectation.equalTo();
                    } catch (RowAssertionFailure rowAssertionFailure) {
                        throw rowAssertionFailure.linePositionException(TableNode.this);
                    }
                }

                @Override
                public ExpectationFactory.Type type() {
                    return verificationExpectation.type();
                }
            };
        };
    }

    public DALNode convertToVerificationNode(Data actual, DALOperator operator, DALRuntimeContext context) {
        return body.convertToVerificationNode(actual, operator, columnHeaderRow.collectComparator(context))
                .setPositionBegin(getPositionBegin());
    }

    @Override
    public String inspect() {
        return (columnHeaderRow.inspect() + body.inspect()).trim();
    }

    public static String printLine(List<? extends DALNode> nodes) {
        return nodes.stream().map(DALNode::inspect).collect(Collectors.joining(" | ", "| ", " |"));
    }

    public Row fetchDataRowSkipEllipsis(int indexSkipEllipsis) {
        return body.dataRowSkipEllipsis(indexSkipEllipsis);
    }
}
