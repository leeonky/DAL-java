package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;

import java.util.Optional;

public interface ExpressionParser {
    Optional<Node> fetch(SourceCode sourceCode, Node previous);

    default ExpressionParser combine(ExpressionParser another) {
        return (nodeParser, previous) -> {
            Optional<Node> optionalNode = fetch(nodeParser, previous);
            if (optionalNode.isPresent())
                return optionalNode;
            return another.fetch(nodeParser, previous);
        };
    }

    default NodeParser defaultInputNode() {
        return sourceCode -> fetch(sourceCode, InputNode.INSTANCE);
    }
}
