package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public interface ExpressionFactory {
    Node fetch(SourceCode sourceCode, Node previous);

    default NodeFactory toNode(Node previous) {
        return sourceCode -> fetch(sourceCode, previous);
    }
}
