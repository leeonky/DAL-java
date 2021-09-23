package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;

import java.util.List;
import java.util.Optional;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.cucumber.SourceCode.FetchBy.BY_NODE;

public interface ExpressionParser {
    ExpressionParser
            DOT_PROPERTY = (sourceCode, previous) -> sourceCode.fetchProperty().map(token -> token.toDotProperty(previous)),
            BRACKET_PROPERTY = new BracketPropertyExpressionParser(),
            EXPLICIT_PROPERTY = DOT_PROPERTY.combine(BRACKET_PROPERTY);

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

    class BracketPropertyExpressionParser implements ExpressionParser {
        @Override
        public Optional<Node> fetch(SourceCode sourceCode, Node previous) {
            return sourceCode.fetchElements(BY_NODE, '[', ']', args -> createNode(previous, args, sourceCode),
                    () -> indexOrKey(sourceCode));
        }

        private PropertyNode createNode(Node previous, List<Node> args, SourceCode sourceCode) {
            if (args.size() != 1)
                throw new SyntaxException("should given one property or array index in `[]`", sourceCode.getPosition() - 1);
            return new PropertyNode(previous, ((ConstNode) args.get(0)).toPropertyOrListIndex(), BRACKET);
        }

        private Node indexOrKey(SourceCode sourceCode) {
            return NodeParser.CONST.fetch(sourceCode).orElseThrow(() ->
                    new SyntaxException("should given one property or array index in `[]`", sourceCode.getPosition()));
        }
    }
}
