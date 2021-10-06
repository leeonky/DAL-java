package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;

import java.util.Optional;

public interface ExpressionMatcher {
    Optional<Node> fetch(SourceCode sourceCode, Node previous);

    default NodeMatcher defaultInputNode() {
        return sourceCode -> fetch(sourceCode, InputNode.INSTANCE);
    }
}
