package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.TokenStream;

public class NodeParser {
    //TODO to be private
    final TokenStream tokenStream;

    private Node thisNode = InputNode.INSTANCE;

    public NodeParser(TokenStream tokenStream) {
        this.tokenStream = tokenStream;
    }

    //TODO logic for clear
    public Node setThis(Node node) {
        thisNode = node;
        return node;
    }

    public Node getThisNode() {
        return thisNode;
    }
}
