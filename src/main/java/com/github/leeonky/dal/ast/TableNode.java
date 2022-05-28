package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.table.TableBody;
import com.github.leeonky.dal.ast.table.TableHeadRow;
import com.github.leeonky.dal.ast.table.TableRowNode;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RowAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.List;
import java.util.stream.Collectors;

public class TableNode extends DALNode {
    private final TableHeadRow headRow;
    private final TableBody tableBody;

    public TableNode(TableHeadRow headRow, TableBody tableBody) {
        this.headRow = headRow;
        this.tableBody = tableBody.checkFormat(this.headRow);
        setPositionBegin(headRow.getPositionBegin());
    }

    @Override
    protected boolean verify(Data actual, DALOperator.Equal operator, DALRuntimeContext context, DALNode actualNode) {
        try {
            return convertToVerificationNode(actual, operator, context).verify(actual, operator, context, actualNode);
        } catch (RowAssertionFailure rowAssertionFailure) {
            throw rowAssertionFailure.linePositionException(this);
        }
    }

    @Override
    protected boolean verify(Data actual, DALOperator.Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        try {
            return convertToVerificationNode(actual, operator, context).verify(actual, operator, context, actualNode);
        } catch (RowAssertionFailure rowAssertionFailure) {
            throw rowAssertionFailure.linePositionException(this);
        }
    }

    public DALNode convertToVerificationNode(Data actual, DALOperator operator, DALRuntimeContext context) {
        return tableBody.convertToVerificationNode(actual, operator, headRow.collectComparator(context))
                .setPositionBegin(getPositionBegin());
    }

    @Override
    public String inspect() {
        return headRow.inspect() + tableBody.inspect();
    }

    public static String printLine(List<? extends DALNode> nodes) {
        return nodes.stream().map(DALNode::inspect).collect(Collectors.joining(" | ", "| ", " |"));
    }

    public TableRowNode fetchDataRowSkipEllipsis(int indexSkipEllipsis) {
        return tableBody.dataRowSkipEllipsis(indexSkipEllipsis);
    }
}
