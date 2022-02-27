package com.github.leeonky.dal.ast.table;

import java.util.Collections;

import static java.util.Optional.empty;

public class EmptyPrefixHeadNode extends PrefixHeadNode {
    public EmptyPrefixHeadNode() {
        super(Collections.emptyList());
    }

    @Override
    public String inspect() {
        return ">>";
    }

    @Override
    public RowPrefixNode getPrefix(int i) {
        return new RowPrefixNode(empty(), empty(), empty());
    }

    @Override
    public void checkSize(TransposedRowNode r) {
    }
}

