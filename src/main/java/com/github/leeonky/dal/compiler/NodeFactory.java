package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.function.Function;

public interface NodeFactory {

    Node fetch(SourceCode sourceCode);

    default NodeFactory recursive(ExpressionMatcher expressionMatcher) {
        return sourceCode -> {
            Node node = fetch(sourceCode);
            Optional<Node> optionalNode = expressionMatcher.fetch(sourceCode, node);
            while (optionalNode.isPresent())
                optionalNode = expressionMatcher.fetch(sourceCode, node = optionalNode.get());
            return node;
        };
    }

    default NodeFactory map(Function<Node, Node> mapping) {
        return sourceCode -> mapping.apply(fetch(sourceCode));
    }
}
