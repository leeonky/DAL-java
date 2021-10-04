package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public interface ExpressionCompiler {
    Node fetch(SourceCode sourceCode, Node previous);

    default NodeCompiler toNode(Node previous) {
        return sourceCode -> fetch(sourceCode, previous);
    }
}
