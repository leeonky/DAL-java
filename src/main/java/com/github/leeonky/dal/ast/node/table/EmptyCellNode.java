package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.DALNode;

public class EmptyCellNode extends DALNode {
    @Override
    public String inspect() {
        return "";
    }
}
