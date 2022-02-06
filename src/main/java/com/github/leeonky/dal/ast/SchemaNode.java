package com.github.leeonky.dal.ast;

public class SchemaNode extends DALNode {
    private final String schema;

    public SchemaNode(String schema) {
        this.schema = schema;
    }

    @Override
    public String inspect() {
        return schema;
    }
}
