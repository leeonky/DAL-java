package com.github.leeonky.dal.ast;

import java.util.List;
import java.util.stream.Collectors;

public class TableNode extends Node {
    private final List<HeaderNode> headers;

    public TableNode(List<HeaderNode> headers) {
        this.headers = headers;
    }

    @Override
    public String inspect() {
        return headers.stream().map(HeaderNode::inspect).collect(Collectors.joining(" | ", "| ", " |"));
    }
}
