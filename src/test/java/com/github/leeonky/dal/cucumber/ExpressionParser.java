package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.ast.*;

import java.util.Optional;
import java.util.function.BiFunction;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.cucumber.MandatoryNodeParser.JUDGEMENT_OR_OPERAND;
import static com.github.leeonky.dal.cucumber.MandatoryNodeParser.OPERAND;
import static com.github.leeonky.dal.cucumber.NodeParser.INTEGER_OR_STRING_INDEX;

//TODO use generic
public interface ExpressionParser {
    ExpressionParser
            DOT_PROPERTY = (sourceCode, previous) -> sourceCode.fetchProperty().map(token -> token.toDotProperty(previous)),
            BRACKET_PROPERTY = (sourceCode, previous) -> {
                String message = "should given one property or array index in `[]`";
                return sourceCode.fetchNode('[', ']', node ->
                                new PropertyNode(previous, ((ConstNode) node).toPropertyOrListIndex(), BRACKET),
                        INTEGER_OR_STRING_INDEX.toMandatoryNodeParser(message), message);
            },
            EXPLICIT_PROPERTY = DOT_PROPERTY.combine(BRACKET_PROPERTY),
            BINARY_ARITHMETIC_EXPRESSION = (sourceCode, previous) -> sourceCode.popBinaryArithmeticOperator().map(operator ->
                    new Expression(previous, operator, OPERAND.fetch(sourceCode)).adjustOperatorOrder()),
            BINARY_JUDGEMENT_EXPRESSION = (sourceCode, previous) -> sourceCode.popJudgementOperator().map(operator ->
                    new Expression(previous, operator, JUDGEMENT_OR_OPERAND.fetch(sourceCode)).adjustOperatorOrder()),
            BINARY_OPERATOR_EXPRESSION = BINARY_ARITHMETIC_EXPRESSION.combine(BINARY_JUDGEMENT_EXPRESSION),
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

    default MandatoryExpressionParser defaultPrevious() {
        return (sourceCode, previous) -> fetch(sourceCode, previous).orElse(previous);
    }

    default Node recursiveCompile(SourceCode sourceCode, Node input,
                                  BiFunction<SourceCode, Node, Node> method) {
        return fetch(sourceCode, input).map(node -> method.apply(sourceCode, node)).orElse(input);
    }

    class SchemaExpressionParser implements ExpressionParser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode, Node previous) {
            return sourceCode.fetchWord(Constants.KeyWords.IS)
                    .map(is -> compile(sourceCode, previous).setPositionBegin(is.getPosition()));
        }

        private SchemaExpression compile(SourceCode sourceCode, Node previous) {
            SchemaExpression expression = new SchemaExpression(previous, sourceCode.fetchIdentityToken().toSchemaNode());
            while (sourceCode.fetchWord("/").isPresent())
                expression.appendSchema(sourceCode.fetchIdentityToken().toSchemaNode());
            return expression;
        }
    }
}
