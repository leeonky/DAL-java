package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;

import java.util.Optional;

public interface ExpressionParser {

    ExpressionParser DOT_PROPERTY = (sourceCode, previous) ->
            sourceCode.fetchProperty().map(token -> token.toDotProperty(previous)),
            BRACKET_PROPERTY = (sourceCode, previous) -> sourceCode.fetchBetween('[', ']', args ->
                    new PropertyNode(previous, ((ConstNode) args.get(0)).toPropertyOrListIndex(),
                            PropertyNode.Type.BRACKET), () -> NodeParser.CONST.fetch(sourceCode).get());

    Optional<Node> fetch(SourceCode sourceCode, Node previous);
}
