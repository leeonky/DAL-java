package com.github.leeonky.dal.ast;

import java.util.List;

import static java.util.stream.Collectors.joining;

public class TableNode extends Node {
    private final List<HeaderNode> headers;
    private final List<List<Node>> rowsCells;

    public TableNode(List<HeaderNode> headers, List<List<Node>> rowsCells) {
        this.headers = headers;
        this.rowsCells = rowsCells;
    }

    @Override
    public String inspect() {
        return String.join("\n", headers.stream().map(HeaderNode::inspect).collect(joining(" | ", "| ", " |")),
                rowsCells.stream().map(cells -> cells.stream().map(Node::inspectClause).collect(joining(" | ", "| ", " |")))
                        .collect(joining("\n")));
    }
}
