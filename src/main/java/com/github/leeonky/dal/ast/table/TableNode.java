package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
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
    public boolean verify(DALNode actualNode, DALOperator.Matcher operator, DALRuntimeContext context) {
        try {
            return transformToVerificationNode(operator, context).verify(actualNode, operator, context);
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.linePositionException(this);
        }
    }

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Equal operator, DALRuntimeContext context) {
        try {
            return transformToVerificationNode(operator, context).verify(actualNode, operator, context);
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.linePositionException(this);
        }
    }

    public DALNode transformToVerificationNode(DALOperator operator, DALRuntimeContext context) {
        return tableBody.transformToListScope(operator, tableHead.collectComparator(context))
                .setPositionBegin(getPositionBegin());
    }

    @Override
    public String inspect() {
        return tableHead.inspect() + tableBody.inspect();
    }

    public static String printLine(List<? extends DALNode> nodes) {
        return nodes.stream().map(DALNode::inspect).collect(Collectors.joining(" | ", "| ", " |"));
    }

    public RowNode getDataRowByDataIndex(int row) {
        return tableBody.getDataRowByDataIndex(row);
    }
}
