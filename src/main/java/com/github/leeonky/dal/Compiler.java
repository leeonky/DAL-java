package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.compiler.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.dal.Constants.KeyWords.WHICH;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.compiler.SourceCode.FetchBy.BY_CHAR;
import static com.github.leeonky.dal.compiler.SourceCode.FetchBy.BY_NODE;

public class Compiler {
    EscapeChars SINGLE_QUOTED_ESCAPES = new EscapeChars()
            .escape("\\\\", '\\')
            .escape("\\'", '\'');
    EscapeChars DOUBLE_QUOTED_ESCAPES = new EscapeChars()
            .escape("\\\\", '\\')
            .escape("\\n", '\n')
            .escape("\\t", '\t')
            .escape("\\\"", '"');
    EscapeChars REGEX_ESCAPES = new EscapeChars()
            .escape("\\/", '/');

    NodeParser INPUT, NUMBER, INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING,
            CONST_TRUE, CONST_FALSE, CONST_NULL, CONST, REGEX, LIST_INDEX_OR_MAP_KEY,
            PARENTHESES, IDENTITY_PROPERTY, PROPERTY, OBJECT, LIST_TAIL, LIST, WILDCARD, JUDGEMENT,
            SCHEMA_WHICH_CLAUSE, SCHEMA_JUDGEMENT_CLAUSE;

    MandatoryNodeParser OPERAND, JUDGEMENT_OR_OPERAND, EXPRESSION, ARITHMETIC_EXPRESSION,
            JUDGEMENT_EXPRESSION_OPERAND;

    MandatoryNodeParser SCHEMA;

    ExpressionParser DOT_PROPERTY, BRACKET_PROPERTY, EXPLICIT_PROPERTY;

    ExpressionParser BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION,
            BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION;

    public Compiler() {
        INPUT = SourceCode::fetchInput;
        NUMBER = sourceCode -> sourceCode.fetchNumber().map(Token::toConstNumber);
        INTEGER = sourceCode -> sourceCode.fetchInteger().map(Token::toConstInteger);
        SINGLE_QUOTED_STRING = sourceCode -> sourceCode.fetchElements(BY_CHAR, '\'', '\'',
                create(ConstNode::new), i -> sourceCode.escapedPop(SINGLE_QUOTED_ESCAPES));
        DOUBLE_QUOTED_STRING = sourceCode -> sourceCode.fetchElements(BY_CHAR, '"', '"',
                create(ConstNode::new), i -> sourceCode.escapedPop(DOUBLE_QUOTED_ESCAPES));
        CONST_TRUE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.TRUE).map(Token::toConstTrue);
        CONST_FALSE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.FALSE).map(Token::toConstFalse);
        CONST_NULL = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.NULL).map(Token::toConstNull);
        CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL);
        REGEX = sourceCode -> sourceCode.fetchElements(BY_CHAR, '/', '/',
                create(RegexNode::new), i -> sourceCode.escapedPop(REGEX_ESCAPES));
        LIST_INDEX_OR_MAP_KEY = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING);
        PARENTHESES = sourceCode -> sourceCode.enableCommaAnd(() -> sourceCode.fetchNode('(', ')',
                ParenthesesNode::new, EXPRESSION, "expect a value or expression"));
        IDENTITY_PROPERTY = sourceCode -> sourceCode.fetchIdentityProperty().map(Token::toIdentityProperty);


        DOT_PROPERTY = (sourceCode, previous) -> sourceCode.fetchProperty().map(token -> token.toDotProperty(previous));
        BRACKET_PROPERTY = (sourceCode, previous) -> {
            String message = "should given one property or array index in `[]`";
            return sourceCode.fetchNode('[', ']', node ->
                            new PropertyNode(previous, ((ConstNode) node).toPropertyOrListIndex(), BRACKET),
                    LIST_INDEX_OR_MAP_KEY.toMandatory(message), message);
        };
        EXPLICIT_PROPERTY = DOT_PROPERTY.combine(BRACKET_PROPERTY);
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(), IDENTITY_PROPERTY);
        OBJECT = sourceCode -> sourceCode.disableCommaAnd(() -> sourceCode.fetchElements(BY_NODE, '{', '}',
                ObjectNode::new, i -> {
                    Node property = PROPERTY.toMandatory("expect a object property").recursive(EXPLICIT_PROPERTY).fetch(sourceCode);
                    return sourceCode.compileJudgement(operator ->
                            new Expression(property, operator, JUDGEMENT_EXPRESSION_OPERAND.fetch(sourceCode)))
                            .orElseThrow(() -> new SyntaxException("expect operator `:` or `=`", sourceCode.getPosition()));
                }));
        LIST_TAIL = sourceCode -> sourceCode.fetchWord("...").map(Token::toListTail);
        LIST = sourceCode -> sourceCode.disableCommaAnd(() -> sourceCode.fetchElements(BY_NODE, '[', ']',
                ListNode::new, i -> LIST_TAIL.fetch(sourceCode).isPresent() ? null :
                        new Expression(new PropertyNode(InputNode.INSTANCE, i, BRACKET),
                                sourceCode.popJudgementOperatorOrDefault(),
                                JUDGEMENT_EXPRESSION_OPERAND.fetch(sourceCode))));
        WILDCARD = sourceCode -> sourceCode.fetchWord("*").map(Token::toWildcardNode);
        JUDGEMENT = oneOf(REGEX, OBJECT, LIST, WILDCARD);

        OPERAND = sourceCode -> sourceCode.popUnaryOperator().<Node>map(operator -> new Expression(operator, OPERAND.fetch(sourceCode)))
                .orElseGet(() -> oneOf(CONST, PROPERTY, PARENTHESES, INPUT).toMandatory("expect a value or expression")
                        .recursive(EXPLICIT_PROPERTY).fetch(sourceCode)).avoidListMapping();
        JUDGEMENT_OR_OPERAND = JUDGEMENT.or(OPERAND);
        SCHEMA = sourceCode -> sourceCode.fetchSchemaToken().toSchemaNode();


        BINARY_ARITHMETIC_EXPRESSION = (sourceCode, previous) -> sourceCode.popBinaryArithmeticOperator().map(operator ->
                new Expression(previous, operator, OPERAND.fetch(sourceCode)).adjustOperatorOrder());
        BINARY_JUDGEMENT_EXPRESSION = (sourceCode, previous) -> sourceCode.compileJudgement(operator ->
                new Expression(previous, operator, JUDGEMENT_OR_OPERAND.fetch(sourceCode)).adjustOperatorOrder());
        BINARY_OPERATOR_EXPRESSION = BINARY_ARITHMETIC_EXPRESSION.combine(BINARY_JUDGEMENT_EXPRESSION);

        ARITHMETIC_EXPRESSION = OPERAND.recursive(BINARY_ARITHMETIC_EXPRESSION);
        JUDGEMENT_EXPRESSION_OPERAND = JUDGEMENT.or(ARITHMETIC_EXPRESSION);

        SCHEMA_EXPRESSION = expressionAfterToken(Constants.KeyWords.IS, schemaAndClause());
        SCHEMA_JUDGEMENT_CLAUSE = judgementClause(InputNode.INSTANCE, JUDGEMENT_EXPRESSION_OPERAND);
        EXPRESSION = OPERAND.recursive(oneOf(BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION));
        SCHEMA_WHICH_CLAUSE = expressionAfterToken(WHICH, SCHEMA_JUDGEMENT_CLAUSE.or(EXPRESSION));
    }

    private MandatoryExpressionParser schemaAndClause() {
        return (sourceCode, previous) -> {
//            TODO return generic
            SchemaExpression expression = new SchemaExpression(previous, (SchemaNode) SCHEMA.fetch(sourceCode));
            while (sourceCode.fetchWord("/").isPresent())
                expression.appendSchema((SchemaNode) SCHEMA.fetch(sourceCode));
            return oneOf(SCHEMA_JUDGEMENT_CLAUSE.map(expression::omitWhich),
                    SCHEMA_WHICH_CLAUSE.map(expression::which)).fetch(sourceCode).orElse(expression);
        };
    }

    private NodeParser expressionAfterToken(String token, MandatoryNodeParser mandatoryNodeParser) {
        return sourceCode -> sourceCode.fetchWord(token).map(t -> mandatoryNodeParser.fetch(sourceCode)
                .setPositionBegin(t.getPosition()));
    }

    private ExpressionParser expressionAfterToken(String token, MandatoryExpressionParser mandatoryExpressionParser) {
        return (sourceCode, previous) -> sourceCode.fetchWord(token).map(t ->
                mandatoryExpressionParser.fetch(sourceCode, previous).setPositionBegin(t.getPosition()));
    }

    private static <T> Function<List<Character>, T> create(Function<String, T> factory) {
        return chars -> factory.apply(chars.stream().map(String::valueOf).collect(Collectors.joining("")));
    }

    public Node compile(SourceCode sourceCode) {
        Node node = EXPRESSION.fetch(sourceCode);
        if (sourceCode.hasCode())
            throw new SyntaxException("unexpected token", sourceCode.getPosition());
        return node;
    }

    public List<Object> toChainNodes(String s) {
        SourceCode sourceCode = new SourceCode(s);
        Optional<Node> fetch = PROPERTY.fetch(sourceCode);
        List<Object> result = new ArrayList<>();
        while (fetch.isPresent()) {
            result.add(((PropertyNode) fetch.get()).getName());
            fetch = PROPERTY.fetch(sourceCode);
        }
        return result;
    }

    private NodeParser judgementClause(Node left, MandatoryNodeParser rightParser) {
        return sourceCode -> sourceCode.compileJudgement(operator ->
                new Expression(left, operator, rightParser.fetch(sourceCode)));
    }

    private static NodeParser oneOf(NodeParser parser, NodeParser... parsers) {
        return sourceCode -> Stream.concat(Stream.of(parser), Stream.of(parsers))
                .map(p -> p.fetch(sourceCode)).filter(Optional::isPresent).findFirst().orElse(Optional.empty());
    }

    private static ExpressionParser oneOf(ExpressionParser parser, ExpressionParser... parsers) {
        return (sourceCode, previous) -> Stream.concat(Stream.of(parser), Stream.of(parsers))
                .map(p -> p.fetch(sourceCode, previous)).filter(Optional::isPresent).findFirst().orElse(Optional.empty());
    }
}
