package com.github.leeonky.dal.ast;

import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class SchemaComposeNode extends DALNode {

    private final List<SchemaNode> schemas;
    private final boolean isList;

    public SchemaComposeNode(List<SchemaNode> schemas, boolean isList) {
        this.schemas = schemas;
        this.isList = isList;
    }

    @Override
    public String inspect() {
        Collector<CharSequence, ?, String> joining = isList ? Collectors.joining(" / ", "[", "]") : Collectors.joining(" / ");
        return schemas.stream().map(SchemaNode::inspect).collect(joining);
    }
}
