package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;

public interface NodeCompiler {

    Node fetch(SourceCode sourceCode);

    default NodeCompiler recursive(ExpressionParser expressionParser) {
        return sourceCode -> {
            Node node = fetch(sourceCode);
            Optional<Node> optionalNode = expressionParser.fetch(sourceCode, node);
            while (optionalNode.isPresent())
                optionalNode = expressionParser.fetch(sourceCode, node = optionalNode.get());
            return node;
        };
    }
}
