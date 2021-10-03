package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.ast.*;

import java.util.Optional;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.compiler.MandatoryNodeParser.*;
import static com.github.leeonky.dal.compiler.NodeParser.INTEGER_OR_STRING_INDEX;
import static com.github.leeonky.dal.util.IfThenFactory.anyOf;

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
            BINARY_JUDGEMENT_EXPRESSION = (sourceCode, previous) -> sourceCode.popJudgementOperatorAndCompile(operator ->
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

    //    TODO use MandatoryNodeParser::recursive ?
    default MandatoryExpressionParser defaultPreviousRecursive() {
        return (sourceCode, previous) -> fetch(sourceCode, previous).map(p ->
                defaultPreviousRecursive().fetch(sourceCode, p)).orElse(previous);
    }

    class SchemaExpressionParser implements ExpressionParser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode, Node previous) {
            return sourceCode.fetchWord(Constants.KeyWords.IS)
                    .map(is -> compile(sourceCode, previous).setPositionBegin(is.getPosition()));
        }

        private Node compile(SourceCode sourceCode, Node previous) {
//            TODO return generic
            SchemaExpression expression = new SchemaExpression(previous, (SchemaNode) SCHEMA.fetch(sourceCode));
            while (sourceCode.fetchWord("/").isPresent())
                expression.appendSchema((SchemaNode) SCHEMA.fetch(sourceCode));
            return anyOf(judgementClause(sourceCode, expression), whichClause(sourceCode, expression)).orElse(expression);
        }

        private Optional<Node> whichClause(SourceCode sourceCode, SchemaExpression expression) {
            return sourceCode.fetchWord(Constants.KeyWords.WHICH).map(_ignore -> expression.which(
                    sourceCode.<Node>popJudgementOperatorAndCompile(operator -> new Expression(InputNode.INSTANCE,
                            operator, JUDGEMENT_EXPRESSION_OPERAND.fetch(sourceCode)))
                            .orElseGet(() -> EXPRESSION.fetch(sourceCode)), false));
        }

        private Optional<Node> judgementClause(SourceCode sourceCode, SchemaExpression expression) {
            return sourceCode.popJudgementOperatorAndCompile(operator -> expression.which(new Expression(
                    InputNode.INSTANCE, operator, JUDGEMENT_EXPRESSION_OPERAND.fetch(sourceCode)), true));
        }
    }
}
