package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;

import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.compiler.Constants.KeyWords.IS;
import static com.github.leeonky.dal.compiler.Constants.KeyWords.WHICH;
import static com.github.leeonky.dal.compiler.SourceCode.DEFAULT_JUDGEMENT_OPERATOR;
import static com.github.leeonky.dal.compiler.SourceCode.operatorMatcher;
import static com.github.leeonky.dal.runtime.Function.not;
import static java.util.Optional.empty;

public class Compiler {
    //    TODO complex expression test
    public static final OperatorMatcher BINARY_ARITHMETIC_OPERATORS = oneOf(
            operatorMatcher("&&", () -> new Operator.And("&&")),
            operatorMatcher("||", () -> new Operator.Or("||")),
            operatorMatcher("and", () -> new Operator.And("and")),
            SourceCode.operatorMatcher(",", () -> new Operator.And(","), SourceCode::isEnableAndComma),
            operatorMatcher("or", () -> new Operator.Or("or")),
            operatorMatcher(">=", Operator.GreaterOrEqual::new),
            operatorMatcher("<=", Operator.LessOrEqual::new),
            operatorMatcher(">", Operator.Greater::new),
            operatorMatcher("<", Operator.Less::new),
            operatorMatcher("+", Operator.Plus::new),
            operatorMatcher("-", Operator.Subtraction::new),
            operatorMatcher("*", Operator.Multiplication::new),
            operatorMatcher("/", Operator.Division::new),
            operatorMatcher("!=", Operator.NotEqual::new));
    public static final OperatorMatcher UNARY_OPERATORS = oneOf(
            SourceCode.operatorMatcher("-", Operator.Minus::new, not(SourceCode::isBeginning)),
            SourceCode.operatorMatcher("!", Operator.Not::new, sourceCode -> !sourceCode.startsWith("!=")));
    public static final OperatorMatcher JUDGEMENT_OPERATORS = oneOf(
            operatorMatcher(":", Operator.Matcher::new),
            operatorMatcher("=", Operator.Equal::new));

    private static final EscapeChars
            SINGLE_QUOTED_ESCAPES = new EscapeChars().escape("\\\\", '\\').escape("\\'", '\''),
            DOUBLE_QUOTED_ESCAPES = new EscapeChars().escape("\\\\", '\\').escape("\\n", '\n').escape("\\t", '\t')
                    .escape("\\\"", '"'),
            REGEX_ESCAPES = new EscapeChars().escape("\\/", '/');

    NodeMatcher INPUT = SourceCode::fetchInput,
            NUMBER = SourceCode.NUMBER.map(Token::toConstNumber),
            INTEGER = SourceCode.INTEGER.map(Token::toConstInteger),
            SINGLE_QUOTED_STRING = sourceCode -> sourceCode.fetchString('\'', '\'', ConstNode::new, SINGLE_QUOTED_ESCAPES),
            DOUBLE_QUOTED_STRING = sourceCode -> sourceCode.fetchString('"', '"', ConstNode::new, DOUBLE_QUOTED_ESCAPES),
            CONST_TRUE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.TRUE).map(Token::toConstTrue),
            CONST_FALSE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.FALSE).map(Token::toConstFalse),
            CONST_NULL = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.NULL).map(Token::toConstNull),
            REGEX = sourceCode -> sourceCode.fetchString('/', '/', RegexNode::new, REGEX_ESCAPES),
            IDENTITY_PROPERTY = SourceCode.IDENTITY_PROPERTY.map(Token::toIdentityProperty),
            WILDCARD = sourceCode -> sourceCode.fetchWord("*").map(Token::toWildcardNode),
            PROPERTY, OBJECT, LIST, CONST, PARENTHESES, JUDGEMENT, SCHEMA_WHICH_CLAUSE, SCHEMA_JUDGEMENT_CLAUSE,
            UNARY_OPERATOR_EXPRESSION;

    NodeFactory SCHEMA = SourceCode.SCHEMA.map(Token::toSchemaNode),
            PROPERTY_CHAIN, OPERAND, EXPRESSION, LIST_INDEX_OR_MAP_KEY, ARITHMETIC_EXPRESSION,
            JUDGEMENT_EXPRESSION_OPERAND;

    ExpressionMatcher DOT_PROPERTY = (sourceCode, previous) -> SourceCode.DOT_PROPERTY.fetch(sourceCode)
            .map(token -> token.toDotProperty(previous)),
            BRACKET_PROPERTY, EXPLICIT_PROPERTY, BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION,
            BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION;

    public Compiler() {
        CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL);
        LIST_INDEX_OR_MAP_KEY = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING)
                .or("should given one property or array index in `[]`");
        PARENTHESES = sourceCode -> sourceCode.enableCommaAnd(() -> sourceCode.fetchNode('(', ')',
                ParenthesesNode::new, EXPRESSION, "expect a value or expression"));
        BRACKET_PROPERTY = (sourceCode, previous) -> sourceCode.fetchNode('[', ']', node ->
                        new PropertyNode(previous, ((ConstNode) node).getValue(), BRACKET),
                LIST_INDEX_OR_MAP_KEY, "should given one property or array index in `[]`");
        EXPLICIT_PROPERTY = oneOf(DOT_PROPERTY, BRACKET_PROPERTY);
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(), IDENTITY_PROPERTY);
        PROPERTY_CHAIN = sourceCode -> PROPERTY.or("expect a object property").recursive(EXPLICIT_PROPERTY).fetch(sourceCode);
        OBJECT = sourceCode -> sourceCode.disableCommaAnd(() -> sourceCode.fetchNodes('{', '}', ObjectNode::new,
                i -> sourceCode.fetchExpression(PROPERTY_CHAIN.fetch(sourceCode),
                        JUDGEMENT_OPERATORS.or("expect operator `:` or `=`"),
                        JUDGEMENT_EXPRESSION_OPERAND)));
        LIST = sourceCode -> sourceCode.disableCommaAnd(() -> sourceCode.fetchNodes('[', ']', ListNode::new,
                i -> sourceCode.fetchWord(Constants.LIST_TAIL).isPresent() ? null
                        : sourceCode.fetchExpression(new PropertyNode(InputNode.INSTANCE, i, BRACKET),
                        JUDGEMENT_OPERATORS.or(DEFAULT_JUDGEMENT_OPERATOR), JUDGEMENT_EXPRESSION_OPERAND)));
        JUDGEMENT = oneOf(REGEX, OBJECT, LIST, WILDCARD);
        UNARY_OPERATOR_EXPRESSION = sourceCode -> sourceCode.fetchExpression(new ConstNode(null), UNARY_OPERATORS, OPERAND);
        OPERAND = UNARY_OPERATOR_EXPRESSION.or(
                oneOf(CONST, PROPERTY, PARENTHESES, INPUT).or("expect a value or expression")
                        .recursive(EXPLICIT_PROPERTY).map(Node::avoidListMapping));
        BINARY_ARITHMETIC_EXPRESSION = (sourceCode, previous) -> sourceCode.fetchExpression(
                previous, BINARY_ARITHMETIC_OPERATORS, OPERAND);
        BINARY_JUDGEMENT_EXPRESSION = (sourceCode, previous) -> sourceCode.fetchExpression(
                previous, JUDGEMENT_OPERATORS, JUDGEMENT.or(OPERAND));
        BINARY_OPERATOR_EXPRESSION = oneOf(BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION);
        ARITHMETIC_EXPRESSION = OPERAND.recursive(BINARY_ARITHMETIC_EXPRESSION);
        JUDGEMENT_EXPRESSION_OPERAND = JUDGEMENT.or(ARITHMETIC_EXPRESSION);
        SCHEMA_JUDGEMENT_CLAUSE = sourceCode -> sourceCode.fetchExpression(
                InputNode.INSTANCE, JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND);
        SCHEMA_WHICH_CLAUSE = sourceCode -> sourceCode.fetchNodeAfter(WHICH, SCHEMA_JUDGEMENT_CLAUSE.or(EXPRESSION));
        SCHEMA_EXPRESSION = (sourceCode, previous) -> sourceCode.fetchNodeAfter(IS, compileSchemaExpression(
                whichClause(SCHEMA_WHICH_CLAUSE, SchemaExpression::which),
                whichClause(SCHEMA_JUDGEMENT_CLAUSE, SchemaExpression::omitWhich)).toNode(previous));
        EXPRESSION = OPERAND.recursive(oneOf(BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION));
    }

    private ExpressionFactory compileSchemaExpression(Function<SchemaExpression, NodeMatcher> whichClause1,
                                                      Function<SchemaExpression, NodeMatcher> whichClause2) {
        return (sourceCode, previous) -> {
            SchemaExpression schemaExpression = new SchemaExpression(previous, sourceCode.fetchNodes("/", SCHEMA));
            return oneOf(whichClause1.apply(schemaExpression), whichClause2.apply(schemaExpression))
                    .fetch(sourceCode).orElse(schemaExpression);
        };
    }

    private Function<SchemaExpression, NodeMatcher> whichClause(NodeMatcher clauseNodeMatcher, BiFunction<SchemaExpression,
            Node, SchemaWhichExpression> appendWay) {
        return schemaExpression -> clauseNodeMatcher.map(node -> appendWay.apply(schemaExpression, node));
    }

    public Node compile(SourceCode sourceCode) {
        Node node = EXPRESSION.fetch(sourceCode);
        if (sourceCode.hasCode())
            throw new SyntaxException("unexpected token", sourceCode.getPosition());
        return node;
    }

    public List<Object> toChainNodes(String s) {
        return ((PropertyNode) PROPERTY_CHAIN.fetch(new SourceCode(s))).getChain();
    }

    private static NodeMatcher oneOf(NodeMatcher matcher, NodeMatcher... matchers) {
        return sourceCode -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(sourceCode)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    private static ExpressionMatcher oneOf(ExpressionMatcher matcher, ExpressionMatcher... matchers) {
        return (sourceCode, previous) -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(sourceCode, previous)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    private static OperatorMatcher oneOf(OperatorMatcher matcher, OperatorMatcher... matchers) {
        return sourceCode -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(sourceCode)).filter(Optional::isPresent).findFirst().orElse(empty());
    }
}
