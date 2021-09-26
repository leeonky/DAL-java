package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.*;

import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.cucumber.MandatoryNodeParser.OPERAND;
import static com.github.leeonky.dal.cucumber.NodeParser.INTEGER_OR_STRING_INDEX;
import static com.github.leeonky.dal.cucumber.SourceCode.FetchBy.BY_NODE;

public interface ExpressionParser {
    ExpressionParser
            DOT_PROPERTY = (sourceCode, previous) -> sourceCode.fetchProperty().map(token -> token.toDotProperty(previous)),
            BRACKET_PROPERTY = new BracketPropertyExpressionParser(),
            EXPLICIT_PROPERTY = DOT_PROPERTY.combine(BRACKET_PROPERTY),
            BINARY_OPERATOR_EXPRESSION = new BinaryOperatorExpressionParser(),
            SCHEMA_EXPRESSION = new SchemaExpressionParser();

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

    default Node recursiveCompile(SourceCode sourceCode, Node input,
                                  BiFunction<SourceCode, Node, Node> method) {
        return fetch(sourceCode, input).map(node -> method.apply(sourceCode, node)).orElse(input);
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
            return INTEGER_OR_STRING_INDEX.fetch(sourceCode).orElseThrow(() ->
                    new SyntaxException("should given one property or array index in `[]`", sourceCode.getPosition()));
        }
    }

    class BinaryOperatorExpressionParser implements ExpressionParser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode, Node previous) {
            return sourceCode.popBinaryOperator().map(operator -> recursiveCompile(sourceCode,
                    new Expression(previous, operator, OPERAND.fetch(sourceCode)).adjustOperatorOrder(),

//                   TODO chain is operator or is which
                    (sourceCode1, node) -> fetch(sourceCode1, node).orElse(node)));

//            TODO test for mix opt expression and schema expression chain
        }
    }

    class SchemaExpressionParser implements ExpressionParser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode, Node previous) {
            return sourceCode.fetchWord(Constants.KeyWords.IS)
                    .map(is -> new SchemaExpression(previous, sourceCode.fetchIdentityToken().toSchemaNode())
                            .setPositionBegin(is.getPosition()));
        }
    }
}
