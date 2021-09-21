package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;

public interface ExpressionParser {

    ExpressionParser DOT_PROPERTY = (sourceCode, previous) ->
            sourceCode.fetchProperty().map(token -> token.toDotProperty(previous));

    Optional<Node> fetch(SourceCode sourceCode, Node previous);
}
