package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.node.table.TableBody;
import com.github.leeonky.dal.ast.node.table.TableHeadRow;
import com.github.leeonky.dal.ast.node.table.TableRowNode;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
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
    public boolean verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        Data actual = actualNode.evaluateData(context);
        try {
            return convertToVerificationNode(actual, operator, context).verify(actualNode, operator, context);
        } catch (RowAssertionFailure rowAssertionFailure) {
            throw rowAssertionFailure.linePositionException(this);
        }
    }

    @Override
    public boolean verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        Data actual = actualNode.evaluateData(context);
        try {
            return convertToVerificationNode(actual, operator, context).verify(actualNode, operator, context);
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
        return (headRow.inspect() + tableBody.inspect()).trim();
    }

    public static String printLine(List<? extends DALNode> nodes) {
        return nodes.stream().map(DALNode::inspect).collect(Collectors.joining(" | ", "| ", " |"));
    }

    public TableRowNode fetchDataRowSkipEllipsis(int indexSkipEllipsis) {
        return tableBody.dataRowSkipEllipsis(indexSkipEllipsis);
    }
}
