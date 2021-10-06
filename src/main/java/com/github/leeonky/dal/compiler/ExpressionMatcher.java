package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;

import java.util.Optional;

public interface ExpressionMatcher {
    Optional<Node> fetch(TokenParser tokenParser, Node previous);

    default NodeMatcher defaultInputNode() {
        return parser -> fetch(parser, InputNode.INSTANCE);
    }
}
