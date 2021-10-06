package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.function.Function;

public interface NodeMatcher {

    Optional<Node> fetch(SourceCode sourceCode);

    default NodeMatcher map(Function<Node, Node> mapping) {
        return sourceCode -> fetch(sourceCode).map(mapping);
    }

    default NodeFactory or(NodeFactory nodeFactory) {
        return sourceCode -> fetch(sourceCode).orElseGet(() -> nodeFactory.fetch(sourceCode));
    }

    default NodeFactory or(String message) {
        return sourceCode -> fetch(sourceCode).orElseThrow(() -> new SyntaxException(message, sourceCode.getPosition()));
    }
}
