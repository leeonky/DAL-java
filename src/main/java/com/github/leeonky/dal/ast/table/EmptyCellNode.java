package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;

public class EmptyCellNode extends DALNode {
    public EmptyCellNode() {
        return;
    }

    @Override
    public String inspect() {
        return "";
    }
}
