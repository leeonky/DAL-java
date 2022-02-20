package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.List;
import java.util.stream.Collectors;

public class TransposedTableNode extends DALNode {
    private final List<TransposedRowNode> rows;

    public TransposedTableNode(List<DALNode> rows) {
        this.rows = rows.stream().map(TransposedRowNode.class::cast).collect(Collectors.toList());
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Equal operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return true;
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Matcher operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return true;
    }

    @Override
    public String inspect() {
        return ">>" + rows.stream().map(TransposedRowNode::inspect).collect(Collectors.joining("\n"));
    }
}
