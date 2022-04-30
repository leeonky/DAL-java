package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.table.RowNode;
import com.github.leeonky.dal.ast.table.TableBody;
import com.github.leeonky.dal.ast.table.TableHead;
import com.github.leeonky.dal.runtime.Data;
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
    protected boolean verify(Data actual, DALOperator.Equal operator, DALRuntimeContext context, DALNode actualNode) {
        try {
            return transformToVerificationNode(operator, context).verify(actual, operator, context, actualNode);
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.linePositionException(this);
        }
    }

    @Override
    protected boolean verify(Data actual, DALOperator.Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        try {
            return transformToVerificationNode(operator, context).verify(actual, operator, context, actualNode);
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
