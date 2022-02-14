package com.github.leeonky.dal.ast;

import java.util.List;
import java.util.stream.Collectors;

public class NodeCollection extends DALNode {
    private final List<DALNode> dalNodes;

    public NodeCollection(List<DALNode> dalNodes) {
        this.dalNodes = dalNodes;
    }

    @Override
    public String inspect() {
        return null;
    }

    public DALNode toConstString(int position) {
        return new ConstNode(getString()).setPositionBegin(position);
    }

    private String getString() {
        return dalNodes.stream().map(ConstNode.class::cast).map(ConstNode::getValue)
                .map(Object::toString).collect(Collectors.joining());
    }

    public DALNode teRegexNode(int position) {
        return new RegexNode(getString()).setPositionBegin(position);
    }

    public DALNode toSchemaComposeNode(int position) {
        return new SchemaComposeNode(dalNodes.stream().map(SchemaNode.class::cast)
                .collect(Collectors.toList()), true).setPositionBegin(position);
    }

    public DALNode objectScopeNode(int position) {
        return new ObjectScopeNode(dalNodes).setPositionBegin(position);
    }
}
