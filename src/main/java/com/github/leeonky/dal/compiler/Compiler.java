package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.ast.PropertyNode.Type.IDENTIFIER;
import static com.github.leeonky.dal.compiler.Constants.KeyWords.IS;
import static com.github.leeonky.dal.compiler.Constants.KeyWords.WHICH;
import static com.github.leeonky.dal.compiler.TokenParser.operatorMatcher;
import static java.util.Optional.empty;

public class Compiler {
    private static final OperatorMatcher AND = operatorMatcher("&&", () -> new Operator.And("&&")),
            OR = operatorMatcher("||", () -> new Operator.Or("||")),
            AND_IN_KEY_WORD = operatorMatcher("and", () -> new Operator.And("and")),
            COMMA_AND = operatorMatcher(",", () -> new Operator.And(","), TokenParser::isEnableCommaAnd),
            OR_IN_KEY_WORD = operatorMatcher("or", () -> new Operator.Or("or")),
            GREATER_OR_EQUAL = operatorMatcher(">=", Operator.GreaterOrEqual::new),
            LESS_OR_EQUAL = operatorMatcher("<=", Operator.LessOrEqual::new),
            GREATER = operatorMatcher(">", Operator.Greater::new),
            LESS = operatorMatcher("<", Operator.Less::new),
            PLUS = operatorMatcher("+", Operator.Plus::new),
            SUBTRACTION = operatorMatcher("-", Operator.Subtraction::new),
            MULTIPLICATION = operatorMatcher("*", Operator.Multiplication::new),
            DIVISION = operatorMatcher("/", Operator.Division::new),
            NOT_EQUAL = operatorMatcher("!=", Operator.NotEqual::new),
            MINUS = operatorMatcher("-", Operator.Minus::new, tokenParser -> !tokenParser.getSourceCode().isBeginning()),
            NOT = operatorMatcher("!", Operator.Not::new, tokenParser -> !tokenParser.getSourceCode().startsWith("!=")),
            MATCHER = operatorMatcher(":", Operator.Matcher::new),
            EQUAL = operatorMatcher("=", Operator.Equal::new),
            BINARY_ARITHMETIC_OPERATORS = oneOf(AND, OR, AND_IN_KEY_WORD, COMMA_AND, NOT_EQUAL, OR_IN_KEY_WORD,
                    GREATER_OR_EQUAL, LESS_OR_EQUAL, GREATER, LESS, PLUS, SUBTRACTION, MULTIPLICATION, DIVISION),
            UNARY_OPERATORS = oneOf(MINUS, NOT),
            JUDGEMENT_OPERATORS = oneOf(MATCHER, EQUAL);

    private static final EscapeChars SINGLE_QUOTED_ESCAPES = new EscapeChars().escape("\\\\", '\\').escape("\\'", '\''),
            DOUBLE_QUOTED_ESCAPES = new EscapeChars().escape("\\\\", '\\').escape("\\n", '\n').escape("\\t", '\t')
                    .escape("\\\"", '"'),
            REGEX_ESCAPES = new EscapeChars().escape("\\/", '/');

    NodeMatcher INPUT = TokenParser::fetchInput,
            NUMBER = TokenParser.NUMBER.map(token -> new ConstNode(token.getNumber())),
            INTEGER = TokenParser.INTEGER.map(token -> new ConstNode(token.getInteger())),
            SINGLE_QUOTED_STRING = parser -> parser.fetchString('\'', '\'', ConstNode::new, SINGLE_QUOTED_ESCAPES),
            DOUBLE_QUOTED_STRING = parser -> parser.fetchString('"', '"', ConstNode::new, DOUBLE_QUOTED_ESCAPES),
            CONST_TRUE = parser -> parser.wordToken(Constants.KeyWords.TRUE, token -> new ConstNode(true)),
            CONST_FALSE = parser -> parser.wordToken(Constants.KeyWords.FALSE, token -> new ConstNode(false)),
            CONST_NULL = parser -> parser.wordToken(Constants.KeyWords.NULL, token -> new ConstNode(null)),
            REGEX = parser -> parser.fetchString('/', '/', RegexNode::new, REGEX_ESCAPES),
            IDENTITY_PROPERTY = TokenParser.IDENTITY_PROPERTY.map(token ->
                    new PropertyNode(InputNode.INSTANCE, token.getContent(), IDENTIFIER)),
            WILDCARD = parser -> parser.wordToken("*", token -> new WildcardNode()),
            PROPERTY, OBJECT, LIST, CONST, PARENTHESES, JUDGEMENT, SCHEMA_WHICH_CLAUSE, SCHEMA_JUDGEMENT_CLAUSE,
            UNARY_OPERATOR_EXPRESSION;

    NodeFactory SCHEMA = TokenParser.SCHEMA.map(token -> new SchemaNode(token.getContent())),
            PROPERTY_CHAIN, OPERAND, EXPRESSION, LIST_INDEX_OR_MAP_KEY, ARITHMETIC_EXPRESSION,
            JUDGEMENT_EXPRESSION_OPERAND;

    ExpressionMatcher DOT_PROPERTY = (parser, previous) -> TokenParser.DOT_PROPERTY.
            map(token -> token.toDotProperty(previous)).fetch(parser),
            BRACKET_PROPERTY, EXPLICIT_PROPERTY, BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION,
            BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION;

    private ExpressionClauseFactory shortJudgementClause(OperatorFactory operatorFactory) {
        return ((ExpressionClauseMatcher) parser -> parser.fetchNodeAfter(IS, (ExpressionClauseFactory) parser1 -> {
            List<SchemaNode> schemaNodes = parser1.fetchNodes("/", SCHEMA);
            return parser.fetchExpressionClause(JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND)
                    .<ExpressionClause>map(clause ->
                            previous -> clause.makeExpression(new SchemaExpression(previous, schemaNodes)))
                    .orElseGet(() -> previous -> new SchemaExpression(previous, schemaNodes));
        })).or(parser -> parser.fetchExpressionClause(operatorFactory, JUDGEMENT_EXPRESSION_OPERAND));
    }

    public Compiler() {
        CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL);
        LIST_INDEX_OR_MAP_KEY = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING)
                .or("should given one property or array index in `[]`");
        PARENTHESES = parser -> parser.enableCommaAnd(() -> parser.fetchNode('(', ')',
                ParenthesesNode::new, EXPRESSION, "expect a value or expression"));
        BRACKET_PROPERTY = (parser, previous) -> parser.fetchNode('[', ']', node ->
                        new PropertyNode(previous, ((ConstNode) node).getValue(), BRACKET),
                LIST_INDEX_OR_MAP_KEY, "should given one property or array index in `[]`");
        EXPLICIT_PROPERTY = oneOf(DOT_PROPERTY, BRACKET_PROPERTY);
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(), IDENTITY_PROPERTY);
        PROPERTY_CHAIN = parser -> PROPERTY.or("expect a object property").recursive(EXPLICIT_PROPERTY).fetch(parser);
        OBJECT = parser -> parser.disableCommaAnd(() -> parser.fetchNodes('{', '}', ObjectNode::new,
                () -> PROPERTY_CHAIN.withClause(
                        shortJudgementClause(JUDGEMENT_OPERATORS.or("expect operator `:` or `=`"))).fetch(parser)));
        LIST = parser -> parser.disableCommaAnd(() -> parser.fetchNodes('[', ']', ListNode::new,
                () -> parser.wordToken(Constants.LIST_ELLIPSIS, token -> new ListEllipsisNode()).isPresent() ? null
                        : shortJudgementClause(JUDGEMENT_OPERATORS.or(TokenParser.DEFAULT_JUDGEMENT_OPERATOR))
                        .fetch(parser)));
        JUDGEMENT = oneOf(REGEX, OBJECT, LIST, WILDCARD);
        UNARY_OPERATOR_EXPRESSION = parser -> parser.fetchExpression(null, UNARY_OPERATORS, OPERAND);
        OPERAND = UNARY_OPERATOR_EXPRESSION.or(oneOf(CONST, PROPERTY, PARENTHESES, INPUT)
                .or("expect a value or expression").recursive(EXPLICIT_PROPERTY).map(Node::avoidListMapping));
        BINARY_ARITHMETIC_EXPRESSION = (parser, previous) -> parser.fetchExpression(
                previous, BINARY_ARITHMETIC_OPERATORS, OPERAND);
        BINARY_JUDGEMENT_EXPRESSION = (parser, previous) -> parser.fetchExpression(
                previous, JUDGEMENT_OPERATORS, JUDGEMENT.or(OPERAND));
        BINARY_OPERATOR_EXPRESSION = oneOf(BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION);
        ARITHMETIC_EXPRESSION = OPERAND.recursive(BINARY_ARITHMETIC_EXPRESSION);
        JUDGEMENT_EXPRESSION_OPERAND = JUDGEMENT.or(ARITHMETIC_EXPRESSION);
        SCHEMA_JUDGEMENT_CLAUSE = parser -> parser.fetchExpression(
                InputNode.INSTANCE, JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND);
        SCHEMA_WHICH_CLAUSE = parser -> parser.fetchNodeAfter(WHICH, SCHEMA_JUDGEMENT_CLAUSE.or(EXPRESSION));
        SCHEMA_EXPRESSION = (parser, previous) -> parser.fetchNodeAfter(IS, compileSchemaExpression(
                whichClause(SCHEMA_WHICH_CLAUSE, SchemaExpression::which),
                whichClause(SCHEMA_JUDGEMENT_CLAUSE, SchemaExpression::omitWhich)).toNode(previous));
        EXPRESSION = OPERAND.recursive(oneOf(BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION));
    }

    private ExpressionFactory compileSchemaExpression(Function<SchemaExpression, NodeMatcher> whichClause1,
                                                      Function<SchemaExpression, NodeMatcher> whichClause2) {
        return (parser, previous) -> {
            SchemaExpression schemaExpression = new SchemaExpression(previous, parser.fetchNodes("/", SCHEMA));
            return oneOf(whichClause1.apply(schemaExpression), whichClause2.apply(schemaExpression))
                    .fetch(parser).orElse(schemaExpression);
        };
    }

    private Function<SchemaExpression, NodeMatcher> whichClause(
            NodeMatcher clauseNodeMatcher, BiFunction<SchemaExpression, Node, SchemaWhichExpression> appendWay) {
        return schemaExpression -> clauseNodeMatcher.map(node -> appendWay.apply(schemaExpression, node));
    }

    public List<Node> compile(SourceCode sourceCode) {
        return new ArrayList<Node>() {{
            TokenParser parser = new TokenParser(sourceCode);
            add(EXPRESSION.fetch(parser));
            if (sourceCode.isBeginning() && sourceCode.isEndOfCode())
                throw sourceCode.syntaxError("unexpected token", 0);
            while (sourceCode.isEndOfCode())
                add(EXPRESSION.fetch(parser));
        }};
    }

    public List<Object> toChainNodes(String sourceCode) {
        return ((PropertyNode) PROPERTY_CHAIN.fetch(new TokenParser(new SourceCode(sourceCode)))).getChain();
    }

    private static NodeMatcher oneOf(NodeMatcher matcher, NodeMatcher... matchers) {
        return parser -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(parser)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    private static ExpressionMatcher oneOf(ExpressionMatcher matcher, ExpressionMatcher... matchers) {
        return (parser, previous) -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(parser, previous)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    private static OperatorMatcher oneOf(OperatorMatcher matcher, OperatorMatcher... matchers) {
        return parser -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(parser)).filter(Optional::isPresent).findFirst().orElse(empty());
    }
}
