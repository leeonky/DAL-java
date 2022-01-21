package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.PropertyNode.Type.BRACKET;
import static com.github.leeonky.dal.ast.PropertyNode.Type.IDENTIFIER;
import static com.github.leeonky.dal.compiler.Constants.KeyWords.IS;
import static com.github.leeonky.dal.compiler.Constants.KeyWords.WHICH;
import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.interpreter.FunctionUtil.allOptional;
import static com.github.leeonky.interpreter.FunctionUtil.transpose;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.TokenParser.operatorMatcher;
import static java.util.Collections.emptyList;
import static java.util.Optional.empty;
import static java.util.stream.Collectors.toList;

public class Compiler {

    private static final OperatorMatcher<DALNode, DALRuntimeContext> AND = operatorMatcher("&&", () -> new Operator.And("&&")),
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

    NodeMatcher<DALNode, DALRuntimeContext> INPUT = tokenParser -> when(tokenParser.getSourceCode().isBeginning()).optional(() -> InputNode.INSTANCE),
            NUMBER = TokenParser.NUMBER.map(token -> new ConstNode(token.getNumber())),
            INTEGER = TokenParser.INTEGER.map(token -> new ConstNode(token.getInteger())),
            SINGLE_QUOTED_STRING = parser -> parser.fetchString('\'', '\'', ConstNode::new, SINGLE_QUOTED_ESCAPES),
            DOUBLE_QUOTED_STRING = parser -> parser.fetchString('"', '"', ConstNode::new, DOUBLE_QUOTED_ESCAPES),
            CONST_TRUE = parser -> parser.wordToken(Constants.KeyWords.TRUE, token -> new ConstNode(true)),
            CONST_FALSE = parser -> parser.wordToken(Constants.KeyWords.FALSE, token -> new ConstNode(false)),
            CONST_NULL = parser -> parser.wordToken(Constants.KeyWords.NULL, token -> new ConstNode(null)),
            CONST_USER_DEFINED_LITERAL = this::compileUserDefinedLiteral,
            REGEX = parser -> parser.fetchString('/', '/', RegexNode::new, REGEX_ESCAPES),
            IDENTITY_PROPERTY = TokenParser.IDENTITY_PROPERTY.map(token ->
                    new PropertyNode(InputNode.INSTANCE, token.getContent(), IDENTIFIER)),
            WILDCARD = parser -> parser.wordToken("*", token -> new WildcardNode("*")),
            ROW_WILDCARD = parser -> parser.wordToken("***", token -> new WildcardNode("***")),
            PROPERTY, OBJECT, LIST, CONST, PARENTHESES, JUDGEMENT, SCHEMA_WHICH_CLAUSE, SCHEMA_JUDGEMENT_CLAUSE,
            ELEMENT_ELLIPSIS, UNARY_OPERATOR_EXPRESSION,
            EMPTY_CELL = parser -> when(parser.getSourceCode().startsWith("|")).optional(EmptyCellNode::new),
            TABLE = oneOf(new TransposedTableWithRowOperator(), new TableMatcher(), new TransposedTable());

    public NodeFactory<DALNode, DALRuntimeContext> PROPERTY_CHAIN, OPERAND, EXPRESSION, LIST_INDEX_OR_MAP_KEY, ARITHMETIC_EXPRESSION,
            JUDGEMENT_EXPRESSION_OPERAND;

    ExpressionMatcher<DALNode, DALRuntimeContext> DOT_PROPERTY = (parser, previous) -> TokenParser.DOT_PROPERTY.
            map(token -> token.toDotProperty(previous)).fetch(parser),
            BRACKET_PROPERTY, EXPLICIT_PROPERTY, BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION,
            BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION;

    private ExpressionClauseFactory<DALNode, DALRuntimeContext> shortJudgementClause(OperatorFactory<DALNode, DALRuntimeContext> operatorFactory) {
        return ((ExpressionClauseMatcher<DALNode, DALRuntimeContext>) parser -> parser.fetchNodeAfter(IS, (ExpressionClauseFactory<DALNode, DALRuntimeContext>) parser1 ->
                schemaJudgement(parser, SCHEMA_CLAUSE.fetch(parser))))
                .or(parser -> parser.fetchExpressionClause(operatorFactory, JUDGEMENT_EXPRESSION_OPERAND));
    }

    private ExpressionClause<DALNode, DALRuntimeContext> schemaJudgement(TokenParser<DALNode, DALRuntimeContext> parser, ExpressionClause<DALNode, DALRuntimeContext> expressionClause) {
        return parser.fetchExpressionClause(JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND)
                .<ExpressionClause<DALNode, DALRuntimeContext>>map(clause -> previous ->
                        clause.makeExpression(expressionClause.makeExpression(previous))).orElse(expressionClause);
    }

    private static final ExpressionClauseFactory<DALNode, DALRuntimeContext> SCHEMA_CLAUSE = new SchemaExpressionClauseFactory();

    public Compiler() {
        CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
                CONST_USER_DEFINED_LITERAL);
        LIST_INDEX_OR_MAP_KEY = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING)
                .or("should given one property or array index in `[]`");
        PARENTHESES = parser -> parser.enableCommaAnd(() -> parser.fetchNode('(', ')',
                ParenthesesNode::new, EXPRESSION, "expect a value or expression"));
        BRACKET_PROPERTY = (parser, previous) -> parser.fetchNode('[', ']', node ->
                        new PropertyNode(previous, ((ConstNode) node).getValue(), BRACKET),
                LIST_INDEX_OR_MAP_KEY, "should given one property or array index in `[]`");
        EXPLICIT_PROPERTY = oneOf(DOT_PROPERTY, BRACKET_PROPERTY);
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(InputNode.INSTANCE), IDENTITY_PROPERTY);
        PROPERTY_CHAIN = parser -> PROPERTY.or("expect a object property").recursive(EXPLICIT_PROPERTY).fetch(parser);
        OBJECT = parser -> parser.disableCommaAnd(() -> parser.fetchNodes('{', '}', ObjectNode::new,
                () -> PROPERTY_CHAIN.withClause(shortJudgementClause(JUDGEMENT_OPERATORS.or("expect operator `:` or `=`"))).fetch(parser)));
        ELEMENT_ELLIPSIS = parser -> parser.wordToken(Constants.ELEMENT_ELLIPSIS, token -> new ListEllipsisNode());
        LIST = parser -> parser.disableCommaAnd(() -> parser.fetchNodes('[', ']', ListNode::new,
                () -> ELEMENT_ELLIPSIS.fetch(parser).<ExpressionClause<DALNode, DALRuntimeContext>>map(node -> p -> node).orElseGet(() ->
                        shortJudgementClause(JUDGEMENT_OPERATORS.or(TokenParser.DEFAULT_JUDGEMENT_OPERATOR)).fetch(parser))));
        JUDGEMENT = oneOf(REGEX, OBJECT, LIST, WILDCARD, TABLE);
        UNARY_OPERATOR_EXPRESSION = parser -> parser.fetchExpression(null, UNARY_OPERATORS, OPERAND);
        OPERAND = UNARY_OPERATOR_EXPRESSION.or(oneOf(CONST, PROPERTY, PARENTHESES, INPUT)
                .or("expect a value or expression").recursive(EXPLICIT_PROPERTY).map(DALNode::avoidListMapping));
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

    private ExpressionFactory<DALNode, DALRuntimeContext> compileSchemaExpression(Function<SchemaExpression, NodeMatcher<DALNode, DALRuntimeContext>> whichClause1,
                                                                                  Function<SchemaExpression, NodeMatcher<DALNode, DALRuntimeContext>> whichClause2) {
        return (parser, previous) -> {
            SchemaExpression schemaExpression = (SchemaExpression) SCHEMA_CLAUSE.fetch(parser).makeExpression(previous);
            return oneOf(whichClause1.apply(schemaExpression), whichClause2.apply(schemaExpression))
                    .fetch(parser).orElse(schemaExpression);
        };
    }

    private Function<SchemaExpression, NodeMatcher<DALNode, DALRuntimeContext>> whichClause(
            NodeMatcher<DALNode, DALRuntimeContext> clauseNodeMatcher, BiFunction<SchemaExpression, DALNode, SchemaWhichExpression> appendWay) {
        return schemaExpression -> clauseNodeMatcher.map(node -> appendWay.apply(schemaExpression, node));
    }

    public List<DALNode> compile(SourceCode sourceCode, DALRuntimeContext DALRuntimeContext) {
        return new ArrayList<DALNode>() {{
            TokenParser<DALNode, DALRuntimeContext> parser = new TokenParser<>(sourceCode, DALRuntimeContext, DALExpression::new);
            add(EXPRESSION.fetch(parser));
            if (sourceCode.isBeginning() && sourceCode.hasCode())
                throw sourceCode.syntaxError("unexpected token", 0);
            while (sourceCode.hasCode())
                add(EXPRESSION.fetch(parser));
        }};
    }

    public List<Object> toChainNodes(String sourceCode) {
        return ((PropertyNode) PROPERTY_CHAIN.fetch(new TokenParser<>(new SourceCode(sourceCode), null, DALExpression::new))).getChain();
    }

    private static <N extends Node<N, C>, C extends RuntimeContext<C>> NodeMatcher<N, C> oneOf(NodeMatcher<N, C> matcher, NodeMatcher<N, C>... matchers) {
        return parser -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(parser)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    private static <N extends Node<N, C>, C extends RuntimeContext<C>> ExpressionMatcher<N, C> oneOf(ExpressionMatcher<N, C> matcher, ExpressionMatcher<N, C>... matchers) {
        return (parser, previous) -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(parser, previous)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    private static <N extends Node<N, C>, C extends RuntimeContext<C>> OperatorMatcher<N, C> oneOf(OperatorMatcher<N, C> matcher, OperatorMatcher<N, C>... matchers) {
        return parser -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.fetch(parser)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    private final NodeFactory<DALNode, DALRuntimeContext> SEQUENCE = parser -> FunctionUtil.oneOf(
            parser.sequenceOf(SEQUENCE_AZ, SequenceNode.Type.AZ),
            parser.sequenceOf(SEQUENCE_ZA, SequenceNode.Type.ZA),
            parser.sequenceOf(SEQUENCE_AZ_2, SequenceNode.Type.AZ),
            parser.sequenceOf(SEQUENCE_ZA_2, SequenceNode.Type.ZA))
            .orElse(SequenceNode.noSequence());

    private final NodeFactory<DALNode, DALRuntimeContext> TABLE_HEADER = parser -> {
        SequenceNode sequence = (SequenceNode) SEQUENCE.fetch(parser);
        DALNode property = PROPERTY_CHAIN.fetch(parser);
        return new HeaderNode(sequence, parser.fetchNodeAfter(IS, SCHEMA_CLAUSE)
                .map(expressionClause -> expressionClause.makeExpression(property)).orElse(property),
                JUDGEMENT_OPERATORS.fetch(parser));
    };

    public class TableMatcher implements NodeMatcher<DALNode, DALRuntimeContext> {
        @Override
        public Optional<DALNode> fetch(TokenParser<DALNode, DALRuntimeContext> parser) {
            try {
                return parser.fetchRow(columnIndex -> (HeaderNode) TABLE_HEADER.fetch(parser))
                        .map(headers -> new TableNode(headers, getRowNodes(parser, headers)));
            } catch (IndexOutOfBoundsException ignore) {
                throw parser.getSourceCode().syntaxError("Different cell size", 0);
            }
        }

        protected List<RowNode> getRowNodes(TokenParser<DALNode, DALRuntimeContext> parser, List<HeaderNode> headers) {
            return allOptional(() -> {
                Optional<Integer> index = getRowIndex(parser);
                Optional<ExpressionClause<DALNode, DALRuntimeContext>> rowSchemaClause = parser.fetchNodeAfter(IS, SCHEMA_CLAUSE);
                Optional<Operator<DALNode, DALRuntimeContext>> rowOperator = JUDGEMENT_OPERATORS.fetch(parser);
                return FunctionUtil.oneOf(
                        () -> parser.fetchBetween("|", "|", ELEMENT_ELLIPSIS).map(Collections::singletonList),
                        () -> parser.fetchBetween("|", "|", ROW_WILDCARD).map(Collections::singletonList),
                        () -> parser.fetchRow(column -> getRowCell(parser, rowOperator, headers.get(column)))
                                .map(cellClauses -> checkCellSize(parser, headers, cellClauses))
                ).map(nodes -> new RowNode(index, rowSchemaClause, rowOperator, nodes));
            });
        }

        private List<DALNode> checkCellSize(TokenParser<DALNode, DALRuntimeContext> parser, List<HeaderNode> headers, List<DALNode> cellClauses) {
            if (cellClauses.size() != headers.size())
                throw parser.getSourceCode().syntaxError("Different cell size", 0);
            return cellClauses;
        }
    }

    private DALNode getRowCell(TokenParser<DALNode, DALRuntimeContext> parser, Optional<Operator<DALNode, DALRuntimeContext>> rowOperator, HeaderNode headerNode) {
        int cellPosition = parser.getSourceCode().nextPosition();
        return oneOf(ELEMENT_ELLIPSIS, EMPTY_CELL).or(ROW_WILDCARD.or(
                shortJudgementClause(oneOf(JUDGEMENT_OPERATORS, headerNode.headerOperator(), parser1 -> rowOperator)
                        .or(TokenParser.DEFAULT_JUDGEMENT_OPERATOR)).input(headerNode.getProperty()))).fetch(parser)
                .setPositionBegin(cellPosition);
    }

    public class TransposedTable implements NodeMatcher<DALNode, DALRuntimeContext> {
        @Override
        public Optional<DALNode> fetch(TokenParser<DALNode, DALRuntimeContext> parser) {
            return parser.getSourceCode().popWord(">>").map(x -> {
                List<HeaderNode> headerNodes = new ArrayList<>();
                return new TableNode(headerNodes, getRowNodes(parser, headerNodes), TableNode.Type.TRANSPOSED);
            });
        }

        private List<RowNode> getRowNodes(TokenParser<DALNode, DALRuntimeContext> parser, List<HeaderNode> headerNodes) {
            return transpose(allOptional(() -> parser.fetchNodeAfter("|", TABLE_HEADER)
                    .map(HeaderNode.class::cast).map(headerNode -> {
                        headerNodes.add(headerNode);
                        return parser.fetchRow(row -> getRowCell(parser, empty(), headerNode))
                                .orElseThrow(() -> parser.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream().map(row -> new RowNode(empty(), empty(), empty(), row)).collect(toList());
        }
    }

    private Optional<Integer> getRowIndex(TokenParser<DALNode, DALRuntimeContext> parser) {
        return INTEGER.fetch(parser).map(node -> (Integer) ((ConstNode) node).getValue());
    }

    public class TransposedTableWithRowOperator implements NodeMatcher<DALNode, DALRuntimeContext> {

        @Override
        public Optional<DALNode> fetch(TokenParser<DALNode, DALRuntimeContext> parser) {
            return parser.getSourceCode().tryFetch(() -> when(parser.getSourceCode().popWord("|").isPresent()
                    && parser.getSourceCode().popWord(">>").isPresent()).optional(() -> {
                List<Optional<Integer>> rowIndexes = new ArrayList<>();
                List<Optional<ExpressionClause<DALNode, DALRuntimeContext>>> rowSchemaClauses = new ArrayList<>();
                List<Optional<Operator<DALNode, DALRuntimeContext>>> rowOperators = parser.fetchRow(row -> {
                    rowIndexes.add(getRowIndex(parser));
                    rowSchemaClauses.add(parser.fetchNodeAfter(IS, SCHEMA_CLAUSE));
                    return JUDGEMENT_OPERATORS.fetch(parser);
                }).orElse(emptyList());
                List<HeaderNode> headerNodes = new ArrayList<>();
                return new TableNode(headerNodes, getRowNodes(parser, headerNodes, rowSchemaClauses, rowOperators,
                        rowIndexes), TableNode.Type.TRANSPOSED);
            }));
        }

        private List<RowNode> getRowNodes(TokenParser<DALNode, DALRuntimeContext> parser, List<HeaderNode> headerNodes,
                                          List<Optional<ExpressionClause<DALNode, DALRuntimeContext>>> rowSchemaClauses,
                                          List<Optional<Operator<DALNode, DALRuntimeContext>>> rowOperators, List<Optional<Integer>> rowIndexes) {
            return FunctionUtil.mapWithIndex(getCells(parser, headerNodes, rowOperators), (i, row) ->
                    new RowNode(rowIndexes.get(i), rowSchemaClauses.get(i), rowOperators.get(i), row)).collect(toList());
        }

        private Stream<List<DALNode>> getCells(TokenParser<DALNode, DALRuntimeContext> parser, List<HeaderNode> headerNodes,
                                               List<Optional<Operator<DALNode, DALRuntimeContext>>> rowOperators) {
            return transpose(allOptional(() -> parser.fetchNodeAfter("|", TABLE_HEADER).map(HeaderNode.class::cast)
                    .map(headerNode -> {
                        headerNodes.add(headerNode);
                        return parser.fetchRow(row -> getRowCell(parser, rowOperators.get(row), headerNode))
                                .orElseThrow(() -> parser.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream();
        }
    }

    private Optional<DALNode> compileUserDefinedLiteral(TokenParser<DALNode, DALRuntimeContext> parser) {
        return parser.getSourceCode().<DALNode, DALRuntimeContext>tryFetch(() -> TokenParser.IDENTITY_PROPERTY.fetch(parser.getSourceCode())
                .flatMap(token -> parser.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }
}
