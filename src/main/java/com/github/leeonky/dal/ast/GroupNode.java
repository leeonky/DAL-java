package com.github.leeonky.dal.ast;

import java.util.List;
import java.util.stream.Collectors;

public class GroupNode extends DALNode {
    //    TODO private *************
    final List<DALNode> elements;

    public GroupNode(List<DALNode> elements) {
        this.elements = elements;
    }

    @Override
    public String inspect() {
        return elements.stream().map(DALNode::inspect).collect(Collectors.joining(", ", "<<", ">>"));
    }
}
