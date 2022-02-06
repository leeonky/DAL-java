package com.github.leeonky.dal.ast;

import java.util.List;
import java.util.stream.Collectors;

public class SchemaComposeNode extends DALNode {

    private final List<SchemaNode> schemas;

    public SchemaComposeNode(List<SchemaNode> schemas, boolean isList) {
        this.schemas = schemas;
    }

    @Override
    public String inspect() {
        return schemas.stream().map(SchemaNode::inspect).collect(Collectors.joining(" / "));
    }

    @Deprecated
    @Override
    public int getPositionBegin() {
        return schemas.get(0).getPositionBegin();
    }
}
