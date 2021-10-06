package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;

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
    public static final OperatorMatcher AND = operatorMatcher("&&", () -> new Operator.And("&&"));
    public static final OperatorMatcher OR = operatorMatcher("||", () -> new Operator.Or("||"));
    public static final OperatorMatcher AND_IN_KEY_WORD = operatorMatcher("and", () -> new Operator.And("and"));
    public static final OperatorMatcher COMMA_AND = operatorMatcher(",", () -> new Operator.And(","),
            TokenParser::isEnableCommaAnd);
    public static final OperatorMatcher OR_IN_KEY_WORD = operatorMatcher("or", () -> new Operator.Or("or"));
    public static final OperatorMatcher GREATER_OR_EQUAL = operatorMatcher(">=", Operator.GreaterOrEqual::new);
    public static final OperatorMatcher LESS_OR_EQUAL = operatorMatcher("<=", Operator.LessOrEqual::new);
    public static final OperatorMatcher GREATER = operatorMatcher(">", Operator.Greater::new);
    public static final OperatorMatcher LESS = operatorMatcher("<", Operator.Less::new);
    public static final OperatorMatcher PLUS = operatorMatcher("+", Operator.Plus::new);
    public static final OperatorMatcher SUBTRACTION = operatorMatcher("-", Operator.Subtraction::new);
    public static final OperatorMatcher MULTIPLICATION = operatorMatcher("*", Operator.Multiplication::new);
    public static final OperatorMatcher DIVISION = operatorMatcher("/", Operator.Division::new);
    public static final OperatorMatcher NOT_EQUAL = operatorMatcher("!=", Operator.NotEqual::new);
    public static final OperatorMatcher MINUS = operatorMatcher("-", Operator.Minus::new,
            tokenParser -> !tokenParser.getSourceCode().isBeginning());
    public static final OperatorMatcher NOT = operatorMatcher("!", Operator.Not::new,
            tokenParser -> !tokenParser.getSourceCode().startsWith("!="));
    public static final OperatorMatcher MATCHER = operatorMatcher(":", Operator.Matcher::new);
    public static final OperatorMatcher EQUAL = operatorMatcher("=", Operator.Equal::new);

    public static final OperatorMatcher BINARY_ARITHMETIC_OPERATORS = oneOf(AND, OR, AND_IN_KEY_WORD, COMMA_AND, NOT_EQUAL,
            OR_IN_KEY_WORD, GREATER_OR_EQUAL, LESS_OR_EQUAL, GREATER, LESS, PLUS, SUBTRACTION, MULTIPLICATION, DIVISION);
    //    TODO complex expression test
    public static final OperatorMatcher UNARY_OPERATORS = oneOf(MINUS, NOT);
    public static final OperatorMatcher JUDGEMENT_OPERATORS = oneOf(MATCHER, EQUAL);
    private static final EscapeChars
            SINGLE_QUOTED_ESCAPES = new EscapeChars().escape("\\\\", '\\').escape("\\'", '\''),
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
                i -> parser.fetchExpression(PROPERTY_CHAIN.fetch(parser),
                        JUDGEMENT_OPERATORS.or("expect operator `:` or `=`"), JUDGEMENT_EXPRESSION_OPERAND)));
        LIST = parser -> parser.disableCommaAnd(() -> parser.fetchNodes('[', ']', ListNode::new,
                i -> parser.wordToken(Constants.LIST_TAIL, token -> new ListTailNode()).isPresent() ? null
                        : parser.fetchExpression(new PropertyNode(InputNode.INSTANCE, i, BRACKET),
                        JUDGEMENT_OPERATORS.or(parser.DEFAULT_JUDGEMENT_OPERATOR), JUDGEMENT_EXPRESSION_OPERAND)));
        JUDGEMENT = oneOf(REGEX, OBJECT, LIST, WILDCARD);
        UNARY_OPERATOR_EXPRESSION = parser -> parser.fetchExpression(new ConstNode(null), UNARY_OPERATORS, OPERAND);
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

    public Node compile(SourceCode sourceCode) {
        Node node = EXPRESSION.fetch(new TokenParser(sourceCode));
        if (sourceCode.hasCode())
            throw sourceCode.syntaxError("unexpected token", 0);
        return node;
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
