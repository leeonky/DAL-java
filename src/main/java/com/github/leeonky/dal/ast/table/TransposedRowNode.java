package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;

public class TransposedRowNode extends DALNode {

    private final HeaderNode headerNode;

    public TransposedRowNode(DALNode headerNode) {
        this.headerNode = (HeaderNode) headerNode;
    }

    @Override
    public String inspect() {
        return "| " + headerNode.inspect() + " |";
    }
}
