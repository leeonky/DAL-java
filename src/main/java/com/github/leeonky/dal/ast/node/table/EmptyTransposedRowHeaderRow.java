package com.github.leeonky.dal.ast.node.table;

import java.util.Collections;

public class EmptyTransposedRowHeaderRow extends TransposedRowHeaderRow {
    public EmptyTransposedRowHeaderRow() {
        super(Collections.emptyList());
    }

    @Override
    public String inspect() {
        return ">>";
    }

    @Override
    public void checkSize(TransposedRow row) {
    }
}

