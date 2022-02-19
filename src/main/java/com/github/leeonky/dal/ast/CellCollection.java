package com.github.leeonky.dal.ast;

import java.util.List;

@Deprecated
public class CellCollection extends DALNode {
    private final List<DALNode> cells;

    public CellCollection(List<DALNode> cells) {
        this.cells = cells;
    }

    @Override
    public String inspect() {
        return null;
    }

    public List<DALNode> getCells() {
        return cells;
    }
}
