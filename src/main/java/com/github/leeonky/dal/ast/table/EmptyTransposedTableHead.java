package com.github.leeonky.dal.ast.table;

import java.util.Collections;

public class EmptyTransposedTableHead extends TransposedTableHead {
    public EmptyTransposedTableHead() {
        super(Collections.emptyList());
    }

    @Override
    public String inspect() {
        return ">>";
    }

    @Override
    public void checkSize(TransposedRowNode r) {
    }
}

