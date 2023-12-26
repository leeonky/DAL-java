package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.node.table.Body;
import com.github.leeonky.dal.ast.node.table.ColumnHeaderRow;
import com.github.leeonky.dal.ast.node.table.Row;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.Expectation;
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
    public Data evaluateData(DALRuntimeContext context) {
        return context.wrap(new Expectation() {
            @Override
            public Data equalTo(DALOperator operator, Data actual) {
                try {
                    return ((Expectation) convertToVerificationNode(actual, operator, context)
                            .evaluateData(context).instance()).equalTo(operator, actual);
                } catch (RowAssertionFailure rowAssertionFailure) {
                    throw rowAssertionFailure.linePositionException(TableNode.this);
                }
            }

            @Override
            public Data matches(DALOperator operator, Data actual) {
                try {
                    return ((Expectation) convertToVerificationNode(actual, operator, context)
                            .evaluateData(context).instance()).matches(operator, actual);
                } catch (RowAssertionFailure rowAssertionFailure) {
                    throw rowAssertionFailure.linePositionException(TableNode.this);
                }
            }
        });
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
