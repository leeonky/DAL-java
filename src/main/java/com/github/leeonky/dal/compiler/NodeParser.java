package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.TokenStream;

import java.util.function.Supplier;

public class NodeParser {
    //TODO to be private
    final TokenStream tokenStream;
    private int bracketCount = 0;
    private Node thisNode = InputNode.INSTANCE;

    public NodeParser(TokenStream tokenStream) {
        this.tokenStream = tokenStream;
    }

    //TODO logic for clear
    public Node setThis(Node node) {
        return thisNode = node;
    }

    public Node getThisNode() {
        return thisNode;
    }

    public Node compileNodeInBracket(Supplier<Node> nodeFactory) {
        return tokenStream.popBracket(() -> {
            bracketCount++;
            Node node = nodeFactory.get();
            bracketCount--;
            return node;
        });
    }

    public boolean isInBracket() {
        return bracketCount == 0;
    }
}
