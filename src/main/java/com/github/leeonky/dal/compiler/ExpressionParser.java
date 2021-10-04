package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;

import java.util.Optional;

public interface ExpressionParser {
    Optional<Node> fetch(SourceCode sourceCode, Node previous);

    default NodeParser defaultInputNode() {
        return sourceCode -> fetch(sourceCode, InputNode.INSTANCE);
    }
}
