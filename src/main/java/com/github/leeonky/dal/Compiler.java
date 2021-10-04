package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.compiler.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.github.leeonky.dal.Constants.KeyWords.IS;
import static com.github.leeonky.dal.Constants.KeyWords.WHICH;
import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
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

    NodeParser INPUT, NUMBER, INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
            CONST, REGEX, LIST_INDEX_OR_MAP_KEY, PARENTHESES, IDENTITY_PROPERTY, PROPERTY, OBJECT, LIST, WILDCARD,
            JUDGEMENT, SCHEMA_WHICH_CLAUSE, SCHEMA_JUDGEMENT_CLAUSE;

    NodeCompiler OPERAND, JUDGEMENT_OR_OPERAND, EXPRESSION, ARITHMETIC_EXPRESSION, JUDGEMENT_EXPRESSION_OPERAND, SCHEMA;

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
        OBJECT = sourceCode -> sourceCode.disableCommaAnd(() -> sourceCode.fetchElements(BY_NODE, '{', '}',
                ObjectNode::new, i -> {
                    Node property = PROPERTY.toMandatory("expect a object property").recursive(EXPLICIT_PROPERTY).fetch(sourceCode);
                    return sourceCode.compileJudgement(operator ->
                            new Expression(property, operator, JUDGEMENT_EXPRESSION_OPERAND.fetch(sourceCode)))
                            .orElseThrow(() -> new SyntaxException("expect operator `:` or `=`", sourceCode.getPosition()));
                }));
        LIST = sourceCode -> sourceCode.disableCommaAnd(() -> sourceCode.fetchElements(BY_NODE, '[', ']',
                ListNode::new, i -> sourceCode.fetchWord(Constants.LIST_TAIL).isPresent() ? null :
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
        BINARY_JUDGEMENT_EXPRESSION = (sourceCode, previous) -> sourceCode.fetchJudgement(previous, JUDGEMENT_OR_OPERAND);
        BINARY_OPERATOR_EXPRESSION = oneOf(BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION);
        ARITHMETIC_EXPRESSION = OPERAND.recursive(BINARY_ARITHMETIC_EXPRESSION);
        JUDGEMENT_EXPRESSION_OPERAND = JUDGEMENT.or(ARITHMETIC_EXPRESSION);
        SCHEMA_JUDGEMENT_CLAUSE = sourceCode -> sourceCode.fetchJudgement(InputNode.INSTANCE, JUDGEMENT_EXPRESSION_OPERAND);
        SCHEMA_WHICH_CLAUSE = sourceCode -> sourceCode.expressionAfter(WHICH, SCHEMA_JUDGEMENT_CLAUSE.or(EXPRESSION));
        SCHEMA_EXPRESSION = (sourceCode, previous) -> sourceCode.expressionAfter(IS, compileSchemaExpression(
                whichClause(SCHEMA_JUDGEMENT_CLAUSE, SchemaExpression::omitWhich),
                whichClause(SCHEMA_WHICH_CLAUSE, SchemaExpression::which)).toNode(previous));
        EXPRESSION = OPERAND.recursive(oneOf(BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION));
    }

    private ExpressionCompiler compileSchemaExpression(Function<SchemaExpression, NodeParser> whichClause1,
                                                       Function<SchemaExpression, NodeParser> whichClause2) {
        return (sourceCode, previous) -> schemaAndClause(sourceCode,
                new SchemaExpression(previous, sourceCode.fetchNodes("/", SCHEMA)), whichClause1, whichClause2);
    }

    private Node schemaAndClause(SourceCode sourceCode1, SchemaExpression schemaExpression,
                                 Function<SchemaExpression, NodeParser> whichClause1,
                                 Function<SchemaExpression, NodeParser> whichClause2) {
        return oneOf(whichClause1.apply(schemaExpression), whichClause2.apply(schemaExpression))
                .fetch(sourceCode1).orElse(schemaExpression);
    }

    private Function<SchemaExpression, NodeParser> whichClause(NodeParser clauseNodeParser, BiFunction<SchemaExpression,
            Node, SchemaWhichExpression> appendWay) {
        return schemaExpression -> clauseNodeParser.map(node -> appendWay.apply(schemaExpression, node));
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

    private static NodeParser oneOf(NodeParser parser, NodeParser... parsers) {
        return sourceCode -> Stream.concat(Stream.of(parser), Stream.of(parsers))
                .map(p -> p.fetch(sourceCode)).filter(Optional::isPresent).findFirst().orElse(Optional.empty());
    }

    private static ExpressionParser oneOf(ExpressionParser parser, ExpressionParser... parsers) {
        return (sourceCode, previous) -> Stream.concat(Stream.of(parser), Stream.of(parsers))
                .map(p -> p.fetch(sourceCode, previous)).filter(Optional::isPresent).findFirst().orElse(Optional.empty());
    }
}
