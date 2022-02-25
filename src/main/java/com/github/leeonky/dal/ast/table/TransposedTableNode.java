package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.ListScopeNode;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.List;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toList;

public class TransposedTableNode extends DALNode {
    private final List<TransposedRowNode> rows;

    public TransposedTableNode(List<DALNode> rows) {
        this.rows = rows.stream().map(TransposedRowNode.class::cast).collect(toList());
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Equal operator, DALRuntimeContext context) {
        return judgeAsList(actualNode, operator, context);
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Matcher operator, DALRuntimeContext context) {
        return judgeAsList(actualNode, operator, context);
    }

    private boolean judgeAsList(DALNode actualNode, DALOperator operator, DALRuntimeContext context) {
        try {
            return getListScopeNode(operator).judgeAll(context, actualNode.evaluateData(context));
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.columnPositionException(this);
        }
    }

    private ListScopeNode getListScopeNode(DALOperator operator) {
        return new TableBody(transposeRows())
                .transformToListScope(operator);
    }

    public List<RowNode> transposeRows() {
        return rows.get(0).transpose();
    }

    @Override
    public String inspect() {
        return ">>" + rows.stream().map(TransposedRowNode::inspect).collect(Collectors.joining("\n"));
    }
}
