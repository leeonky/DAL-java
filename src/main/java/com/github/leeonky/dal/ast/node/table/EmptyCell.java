package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.DALNode;

public class EmptyCell extends DALNode {
    @Override
    public String inspect() {
        return "";
    }
}
