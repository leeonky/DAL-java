package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.table.TableBody;
import com.github.leeonky.dal.ast.table.TableHead;
import com.github.leeonky.dal.ast.table.TableRowNode;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RowAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.List;
import java.util.stream.Collectors;

public class TableNode extends DALNode {
    private final TableHead tableHead;
    private final TableBody tableBody;

    public TableNode(TableHead tableHead, TableBody tableBody) {
        this.tableHead = tableHead;
        this.tableBody = tableBody.checkTable(this.tableHead);
        setPositionBegin(tableHead.getPositionBegin());
    }

    @Override
    protected boolean verify(Data actual, DALOperator.Equal operator, DALRuntimeContext context, DALNode actualNode) {
        try {
            return transformToVerificationNode(actual, operator, context).verify(actual, operator, context, actualNode);
        } catch (RowAssertionFailure rowAssertionFailure) {
            throw rowAssertionFailure.linePositionException(this);
        }
    }

    @Override
    protected boolean verify(Data actual, DALOperator.Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        try {
            return transformToVerificationNode(actual, operator, context).verify(actual, operator, context, actualNode);
        } catch (RowAssertionFailure rowAssertionFailure) {
            throw rowAssertionFailure.linePositionException(this);
        }
    }

    public DALNode transformToVerificationNode(Data actual, DALOperator operator, DALRuntimeContext context) {
        return tableBody.convertToVerificationNode(actual, operator, tableHead.collectComparator(context))
                .setPositionBegin(getPositionBegin());
    }

    @Override
    public String inspect() {
        return tableHead.inspect() + tableBody.inspect();
    }

    public static String printLine(List<? extends DALNode> nodes) {
        return nodes.stream().map(DALNode::inspect).collect(Collectors.joining(" | ", "| ", " |"));
    }

    public TableRowNode getDataRowByDataIndex(int row) {
        return tableBody.getDataRowByDataIndex(row);
    }
}
