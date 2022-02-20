package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;

public class HeaderNode extends DALNode {
    private final DALNode property;

    public HeaderNode(DALNode property) {
        this.property = property;
    }

    public DALNode getProperty() {
        return property;
    }

    @Override
    public String inspect() {
        return property.inspect();
    }
}
