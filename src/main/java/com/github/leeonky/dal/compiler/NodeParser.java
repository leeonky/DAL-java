package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.function.Function;

public interface NodeParser {

    Optional<Node> fetch(SourceCode sourceCode);

    default NodeParser map(Function<Node, Node> mapping) {
        return sourceCode -> fetch(sourceCode).map(mapping);
    }

    default NodeCompiler or(NodeCompiler nodeCompiler) {
        return sourceCode -> fetch(sourceCode).orElseGet(() -> nodeCompiler.fetch(sourceCode));
    }

    default NodeCompiler or(String message) {
        return sourceCode -> fetch(sourceCode).orElseThrow(() -> new SyntaxException(message, sourceCode.getPosition()));
    }
}
