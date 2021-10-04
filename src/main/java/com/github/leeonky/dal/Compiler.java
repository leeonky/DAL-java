package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.compiler.*;

import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.github.leeonky.dal.Constants.KeyWords.IS;
import static com.github.leeonky.dal.Constants.KeyWords.WHICH;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.compiler.SourceCode.*;
import static com.github.leeonky.dal.compiler.SourceCode.FetchBy.BY_NODE;
import static java.util.Optional.empty;

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

    NodeParser INPUT, NUMBER, INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
            CONST, REGEX, LIST_INDEX_OR_MAP_KEY, PARENTHESES, IDENTITY_PROPERTY, PROPERTY, OBJECT, LIST,
            WILDCARD = sourceCode -> sourceCode.fetchWord("*").map(Token::toWildcardNode),
            JUDGEMENT, SCHEMA_WHICH_CLAUSE, SCHEMA_JUDGEMENT_CLAUSE, UNARY_OPERATOR_EXPRESSION;

    NodeCompiler OPERAND, EXPRESSION, ARITHMETIC_EXPRESSION, JUDGEMENT_EXPRESSION_OPERAND,
            SCHEMA = sourceCode -> sourceCode.fetchSchemaToken().toSchemaNode(), PROPERTY_CHAIN;

    ExpressionParser DOT_PROPERTY, BRACKET_PROPERTY, EXPLICIT_PROPERTY, BINARY_ARITHMETIC_EXPRESSION,
            BINARY_JUDGEMENT_EXPRESSION, BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION;

    public Compiler() {
        INPUT = SourceCode::fetchInput;
        NUMBER = sourceCode -> sourceCode.fetchNumber().map(Token::toConstNumber);
        INTEGER = sourceCode -> sourceCode.fetchInteger().map(Token::toConstInteger);
        SINGLE_QUOTED_STRING = sourceCode -> sourceCode.fetchString('\'', '\'', ConstNode::new, SINGLE_QUOTED_ESCAPES);
        DOUBLE_QUOTED_STRING = sourceCode -> sourceCode.fetchString('"', '"', ConstNode::new, DOUBLE_QUOTED_ESCAPES);
        CONST_TRUE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.TRUE).map(Token::toConstTrue);
        CONST_FALSE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.FALSE).map(Token::toConstFalse);
        CONST_NULL = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.NULL).map(Token::toConstNull);
        CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL);
        REGEX = sourceCode -> sourceCode.fetchString('/', '/', RegexNode::new, REGEX_ESCAPES);
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
        EXPLICIT_PROPERTY = oneOf(DOT_PROPERTY, BRACKET_PROPERTY);
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(), IDENTITY_PROPERTY);
        PROPERTY_CHAIN = sourceCode -> PROPERTY.toMandatory("expect a object property").recursive(EXPLICIT_PROPERTY).fetch(sourceCode);
        OBJECT = sourceCode -> sourceCode.disableCommaAnd(() -> sourceCode.fetchElements(BY_NODE, '{', '}',
                ObjectNode::new, i -> (Expression) sourceCode.fetchExpression(
                        PROPERTY_CHAIN.fetch(sourceCode), JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND)
                        .orElseThrow(() -> new SyntaxException("expect operator `:` or `=`", sourceCode.getPosition()))));
        LIST = sourceCode -> sourceCode.disableCommaAnd(() -> sourceCode.fetchElements(BY_NODE, '[', ']',
                ListNode::new, i -> sourceCode.fetchWord(Constants.LIST_TAIL).isPresent() ? null
                        : sourceCode.fetchExpression(new PropertyNode(InputNode.INSTANCE, i, BRACKET),
                        sourceCode.popJudgementOperatorOrDefault(),
                        JUDGEMENT_EXPRESSION_OPERAND)));
        JUDGEMENT = oneOf(REGEX, OBJECT, LIST, WILDCARD);
        UNARY_OPERATOR_EXPRESSION = sourceCode -> sourceCode.fetchExpression(
                new ConstNode(null), UNARY_OPERATORS, OPERAND);
        OPERAND = UNARY_OPERATOR_EXPRESSION.or(oneOf(CONST, PROPERTY, PARENTHESES, INPUT)
                .toMandatory("expect a value or expression").recursive(EXPLICIT_PROPERTY).map(Node::avoidListMapping));
        BINARY_ARITHMETIC_EXPRESSION = (sourceCode, previous) -> sourceCode.fetchExpression(
                previous, BINARY_ARITHMETIC_OPERATORS, OPERAND);
        BINARY_JUDGEMENT_EXPRESSION = (sourceCode, previous) -> sourceCode.fetchExpression(
                previous, JUDGEMENT_OPERATORS, JUDGEMENT.or(OPERAND));
        BINARY_OPERATOR_EXPRESSION = oneOf(BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION);
        ARITHMETIC_EXPRESSION = OPERAND.recursive(BINARY_ARITHMETIC_EXPRESSION);
        JUDGEMENT_EXPRESSION_OPERAND = JUDGEMENT.or(ARITHMETIC_EXPRESSION);
        SCHEMA_JUDGEMENT_CLAUSE = sourceCode -> sourceCode.fetchExpression(
                InputNode.INSTANCE, JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND);
        SCHEMA_WHICH_CLAUSE = sourceCode -> sourceCode.expressionAfter(WHICH, SCHEMA_JUDGEMENT_CLAUSE.or(EXPRESSION));
        SCHEMA_EXPRESSION = (sourceCode, previous) -> sourceCode.expressionAfter(IS, compileSchemaExpression(
                whichClause(SCHEMA_WHICH_CLAUSE, SchemaExpression::which),
                whichClause(SCHEMA_JUDGEMENT_CLAUSE, SchemaExpression::omitWhich)).toNode(previous));
        EXPRESSION = OPERAND.recursive(oneOf(BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION));
    }

    private ExpressionCompiler compileSchemaExpression(Function<SchemaExpression, NodeParser> whichClause1,
                                                       Function<SchemaExpression, NodeParser> whichClause2) {
        return (sourceCode, previous) -> {
            SchemaExpression schemaExpression = new SchemaExpression(previous, sourceCode.fetchNodes("/", SCHEMA));
            return oneOf(whichClause1.apply(schemaExpression), whichClause2.apply(schemaExpression))
                    .fetch(sourceCode).orElse(schemaExpression);
        };
    }

    private Function<SchemaExpression, NodeParser> whichClause(NodeParser clauseNodeParser, BiFunction<SchemaExpression,
            Node, SchemaWhichExpression> appendWay) {
        return schemaExpression -> clauseNodeParser.map(node -> appendWay.apply(schemaExpression, node));
    }

    public Node compile(SourceCode sourceCode) {
        Node node = EXPRESSION.fetch(sourceCode);
        if (sourceCode.hasCode())
//TODO should trim blank
            throw new SyntaxException("unexpected token", sourceCode.getPosition());
        return node;
    }

    public List<Object> toChainNodes(String s) {
        return ((PropertyNode) PROPERTY_CHAIN.fetch(new SourceCode(s))).getChain();
    }

    private static NodeParser oneOf(NodeParser parser, NodeParser... parsers) {
        return sourceCode -> Stream.concat(Stream.of(parser), Stream.of(parsers))
                .map(p -> p.fetch(sourceCode)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    private static ExpressionParser oneOf(ExpressionParser parser, ExpressionParser... parsers) {
        return (sourceCode, previous) -> Stream.concat(Stream.of(parser), Stream.of(parsers))
                .map(p -> p.fetch(sourceCode, previous)).filter(Optional::isPresent).findFirst().orElse(empty());
    }
}
