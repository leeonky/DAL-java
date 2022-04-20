package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.ListScopeNode;
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
        return verifyAsListAndReThrow(actualNode, operator, context);
    }

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Equal operator, DALRuntimeContext context) {
        return verifyAsListAndReThrow(actualNode, operator, context);
    }

    private boolean verifyAsListAndReThrow(DALNode actualNode, DALOperator operator, DALRuntimeContext context) {
        try {
            return verifyAsList(actualNode, operator, context);
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.linePositionException(this);
        }
    }

    public boolean verifyAsList(DALNode actualNode, DALOperator operator, DALRuntimeContext context) {
        return ((ListScopeNode) tableBody.transformToListScope(operator).setPositionBegin(getPositionBegin()))
                .verifyAll(context, actualNode.evaluateData(context).setListComparator(tableHead.collectComparator(context)));
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
