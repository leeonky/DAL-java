package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.compiler.Notations.Keywords;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;
import static com.github.leeonky.dal.ast.PropertyNode.Type.*;
import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.dal.compiler.Notations.Operators;
import static com.github.leeonky.interpreter.ExpressionClauseParser.ExpressionClauseFactory.after;
import static com.github.leeonky.interpreter.ExpressionClauseParser.oneOf;
import static com.github.leeonky.interpreter.FunctionUtil.*;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.NodeParser.oneOf;
import static com.github.leeonky.interpreter.OperatorParser.oneOf;
import static java.util.Collections.emptyList;
import static java.util.Optional.empty;
import static java.util.stream.Collectors.toList;

public class Compiler {

    private static final OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser>
            BINARY_ARITHMETIC_OPERATORS = oneOf(
            Operators.AND.operatorMatcher(DALOperator::operatorAnd),
            Operators.OR.operatorMatcher(DALOperator::operatorOr),
            Keywords.AND.operatorMatcher(DALOperator::keywordAnd),
            Operators.COMMA.operatorMatcher(DALOperator::commaAnd, DALParser::isEnableCommaAnd),
            Operators.NOT_EQUAL.operatorMatcher(DALOperator.NotEqual::new),
            Keywords.OR.operatorMatcher(DALOperator::keywordOr),
            Operators.GREATER_OR_EQUAL.operatorMatcher(DALOperator.GreaterOrEqual::new),
            Operators.LESS_OR_EQUAL.operatorMatcher(DALOperator.LessOrEqual::new),
            Operators.GREATER.operatorMatcher(DALOperator.Greater::new),
            Operators.LESS.operatorMatcher(DALOperator.Less::new),
            Operators.PLUS.operatorMatcher(DALOperator.Plus::new),
            Operators.SUBTRACTION.operatorMatcher(DALOperator.Subtraction::new),
            Operators.MULTIPLICATION.operatorMatcher(DALOperator.Multiplication::new),
            Operators.DIVISION.operatorMatcher(DALOperator.Division::new));

    private static final OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser>
            UNARY_OPERATORS = oneOf(
            Operators.MINUS.operatorMatcher(DALOperator.Minus::new, not(DALParser::isCodeBeginning)),
            Operators.NOT.operatorMatcher(DALOperator.Not::new, not(DALParser::mayBeUnEqual)));

    private static final OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser>
            JUDGEMENT_OPERATORS = oneOf(
            Operators.MATCHER.<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser>
                    operatorMatcher(DALOperator.Matcher::new),
            Operators.EQUAL.operatorMatcher(DALOperator.Equal::new));

    private static final EscapeChars SINGLE_QUOTED_ESCAPES = new EscapeChars()
            .escape("\\\\", '\\')
            .escape("\\'", '\'');
    private static final EscapeChars DOUBLE_QUOTED_ESCAPES = new EscapeChars()
            .escape("\\\\", '\\')
            .escape("\\n", '\n')
            .escape("\\t", '\t')
            .escape("\\\"", '"');
    private static final EscapeChars REGEX_ESCAPES = new EscapeChars()
            .escape("\\/", '/');

    NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser>
            INPUT = parser -> when(parser.isCodeBeginning()).optional(() -> InputNode.INSTANCE),
            NUMBER = Tokens.NUMBER.nodeMatcher(token -> new ConstNode(token.getNumber())),
            INTEGER = Tokens.INTEGER.nodeMatcher(token -> new ConstNode(token.getInteger())),
            SINGLE_QUOTED_STRING = parser -> parser.fetchString('\'', '\'', ConstNode::new, SINGLE_QUOTED_ESCAPES),
            DOUBLE_QUOTED_STRING = parser -> parser.fetchString('"', '"', ConstNode::new, DOUBLE_QUOTED_ESCAPES),
            CONST_TRUE = Keywords.TRUE.nodeMatcher(token -> new ConstNode(true)),
            CONST_FALSE = Keywords.FALSE.nodeMatcher(token -> new ConstNode(false)),
            CONST_NULL = Keywords.NULL.nodeMatcher(token -> new ConstNode(null)),
            CONST_USER_DEFINED_LITERAL = this::compileUserDefinedLiteral,
            REGEX = parser -> parser.fetchString('/', '/', RegexNode::new, REGEX_ESCAPES),
            IDENTITY_PROPERTY = Tokens.IDENTITY_PROPERTY.nodeMatcher(token ->
                    new PropertyNode(InputNode.INSTANCE, token.getContent(), IDENTIFIER)),
            WILDCARD = Notations.Operators.WILDCARD.nodeMatcher(token -> new WildcardNode(token.getContent())),
            ROW_WILDCARD = Notations.Operators.ROW_WILDCARD.nodeMatcher(token -> new WildcardNode(token.getContent())),
            PROPERTY, OBJECT, LIST, CONST, PARENTHESES, JUDGEMENT, SCHEMA_JUDGEMENT_CLAUSE,
            ELEMENT_ELLIPSIS, UNARY_OPERATOR_EXPRESSION,
            EMPTY_CELL = parser -> when(parser.getSourceCode().startsWith("|")).optional(EmptyCellNode::new),
            TABLE = oneOf(new TransposedTableWithRowOperator(), new TableParser(), new TransposedTable());

    public NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> PROPERTY_CHAIN, OPERAND, EXPRESSION,
            LIST_INDEX_OR_MAP_KEY, ARITHMETIC_EXPRESSION, JUDGEMENT_EXPRESSION_OPERAND;

    ExpressionClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser>
            DOT_PROPERTY = Tokens.DOT_PROPERTY.toClauseMatcher((token, previous) -> new PropertyNode(previous,
            token.getContentOrThrow("property is not finished"), DOT)),
            BRACKET_PROPERTY = parser -> parser.fetchExpressionClauseBetween('[', ']', (previous, node) ->
                            new PropertyNode(previous, ((ConstNode) node).getValue(), BRACKET), LIST_INDEX_OR_MAP_KEY,
                    "should given one property or array index in `[]`"),
            EXPLICIT_PROPERTY = oneOf(DOT_PROPERTY, BRACKET_PROPERTY),
            BINARY_ARITHMETIC_EXPRESSION,
            BINARY_JUDGEMENT_EXPRESSION,
            BINARY_OPERATOR_EXPRESSION,
            SCHEMA_EXPRESSION;

    private ExpressionClauseParser.ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> shortJudgementClause(
            OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> mandatoryParser) {
        return ((ExpressionClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser>) parser ->
                parser.fetchClauseAfter(Notations.IS_s, parser1 -> schemaJudgement(parser, SCHEMA_CLAUSE.parse(parser))))
                .or(parser -> parser.fetchExpressionClause(mandatoryParser, JUDGEMENT_EXPRESSION_OPERAND));
    }

    private ExpressionClause<DALRuntimeContext, DALNode> schemaJudgement(
            Parser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> parser,
            ExpressionClause<DALRuntimeContext, DALNode> expressionClause) {
        return parser.fetchExpressionClause(JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND)
                .<ExpressionClause<DALRuntimeContext, DALNode>>map(clause -> previous ->
                        clause.makeExpression(expressionClause.makeExpression(previous))).orElse(expressionClause);
    }

    private static final ExpressionClauseParser.ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser>
            SCHEMA_CLAUSE = new SchemaExpressionClauseFactory();

    public Compiler() {
        CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
                CONST_USER_DEFINED_LITERAL);
        LIST_INDEX_OR_MAP_KEY = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING)
                .or("should given one property or array index in `[]`");
        PARENTHESES = parser -> parser.enableCommaAnd(() -> parser.fetchNodeWithOneChildNodeBetween('(', ')',
                ParenthesesNode::new, EXPRESSION, "expect a value or expression"));
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(InputNode.INSTANCE), IDENTITY_PROPERTY);
        PROPERTY_CHAIN = parser -> PROPERTY.or("expect a object property").recursive(EXPLICIT_PROPERTY).parse(parser);
        OBJECT = parser -> parser.disableCommaAnd(() -> parser.fetchNodeWithElementsBetween('{', '}', ObjectNode::new,
                () -> PROPERTY_CHAIN.withClause(shortJudgementClause(JUDGEMENT_OPERATORS.or("expect operator `:` or `=`"))).parse(parser)));
        ELEMENT_ELLIPSIS = Notations.Operators.ELEMENT_ELLIPSIS.nodeMatcher(token -> new ListEllipsisNode());
        LIST = parser -> parser.disableCommaAnd(() -> parser.fetchNodeWithElementsBetween('[', ']', ListNode::new,
                () -> ELEMENT_ELLIPSIS.parse(parser).<ExpressionClause<DALRuntimeContext, DALNode>>map(node -> p -> node).orElseGet(() ->
                        shortJudgementClause(JUDGEMENT_OPERATORS.or(Tokens.DEFAULT_JUDGEMENT_OPERATOR)).parse(parser))));
        JUDGEMENT = oneOf(REGEX, OBJECT, LIST, WILDCARD, TABLE);
        UNARY_OPERATOR_EXPRESSION = parser -> parser.fetchExpression(null, UNARY_OPERATORS, OPERAND);
        OPERAND = UNARY_OPERATOR_EXPRESSION.or(oneOf(CONST, PROPERTY, PARENTHESES, INPUT)
                .or("expect a value or expression").recursive(EXPLICIT_PROPERTY).map(DALNode::avoidListMapping));
        BINARY_ARITHMETIC_EXPRESSION = BINARY_ARITHMETIC_OPERATORS.toClause(OPERAND);
        BINARY_JUDGEMENT_EXPRESSION = JUDGEMENT_OPERATORS.toClause(JUDGEMENT.or(OPERAND));
        BINARY_OPERATOR_EXPRESSION = oneOf(BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION);
        ARITHMETIC_EXPRESSION = OPERAND.recursive(BINARY_ARITHMETIC_EXPRESSION);
        JUDGEMENT_EXPRESSION_OPERAND = JUDGEMENT.or(ARITHMETIC_EXPRESSION);
        SCHEMA_JUDGEMENT_CLAUSE = parser -> parser.fetchExpression(
                InputNode.INSTANCE, JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND);
        SCHEMA_EXPRESSION = parser -> after(Notations.IS_s, SCHEMA_CLAUSE).concat(omitWhich(SCHEMA_JUDGEMENT_CLAUSE),
                after(Notations.WHICH_S, which(SCHEMA_JUDGEMENT_CLAUSE.or(EXPRESSION)))).parse(parser);
        EXPRESSION = OPERAND.recursive(oneOf(BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION));
    }

    private ExpressionClauseParser.ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> which(
            NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> nodeFactory) {
        return parser -> previous -> ((SchemaExpression) previous).which(nodeFactory.parse(parser));
    }

    private ExpressionClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> omitWhich(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> nodeParser) {
        return parser -> nodeParser.parse(parser).map(node -> previous ->
                ((SchemaExpression) previous).omitWhich(node));
    }

    public List<DALNode> compile(SourceCode sourceCode, DALRuntimeContext DALRuntimeContext) {
        return new ArrayList<DALNode>() {{
            DALParser dalParser = new DALParser(sourceCode, DALRuntimeContext, DALExpression::new);
            add(EXPRESSION.parse(dalParser));
            if (sourceCode.isBeginning() && sourceCode.hasCode())
                throw sourceCode.syntaxError("unexpected token", 0);
            while (sourceCode.hasCode())
                add(EXPRESSION.parse(dalParser));
        }};
    }

    public List<Object> toChainNodes(String sourceCode) {
        return ((PropertyNode) PROPERTY_CHAIN.parse(new DALParser(new SourceCode(sourceCode),
                null, DALExpression::new))).getChain();
    }

    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser>
            SEQUENCE = parser -> FunctionUtil.oneOf(
            parser.sequenceOf(SEQUENCE_AZ, SequenceNode.Type.AZ),
            parser.sequenceOf(SEQUENCE_ZA, SequenceNode.Type.ZA),
            parser.sequenceOf(SEQUENCE_AZ_2, SequenceNode.Type.AZ),
            parser.sequenceOf(SEQUENCE_ZA_2, SequenceNode.Type.ZA))
            .orElse(SequenceNode.noSequence());

    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> TABLE_HEADER = parser -> {
        SequenceNode sequence = (SequenceNode) SEQUENCE.parse(parser);
        DALNode property = PROPERTY_CHAIN.parse(parser);
        return new HeaderNode(sequence, parser.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE)
                .map(expressionClause -> expressionClause.makeExpression(property)).orElse(property),
                JUDGEMENT_OPERATORS.parse(parser));
    };

    public class TableParser implements NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> {
        @Override
        public Optional<DALNode> parse(DALParser parser) {
            try {
                return parser.fetchRow(columnIndex -> (HeaderNode) TABLE_HEADER.parse(parser))
                        .map(headers -> new TableNode(headers, getRowNodes(parser, headers)));
            } catch (IndexOutOfBoundsException ignore) {
                throw parser.getSourceCode().syntaxError("Different cell size", 0);
            }
        }

        protected List<RowNode> getRowNodes(DALParser parser, List<HeaderNode> headers) {
            return allOptional(() -> {
                Optional<Integer> index = getRowIndex(parser);
                Optional<ExpressionClause<DALRuntimeContext, DALNode>> rowSchemaClause = parser.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE);
                Optional<DALOperator> rowOperator = JUDGEMENT_OPERATORS.parse(parser);
                return FunctionUtil.oneOf(
                        () -> parser.fetchNodeBetween("|", "|", ELEMENT_ELLIPSIS).map(Collections::singletonList),
                        () -> parser.fetchNodeBetween("|", "|", ROW_WILDCARD).map(Collections::singletonList),
                        () -> parser.fetchRow(column -> getRowCell(parser, rowOperator, headers.get(column)))
                                .map(cellClauses -> checkCellSize(parser, headers, cellClauses))
                ).map(nodes -> new RowNode(index, rowSchemaClause, rowOperator, nodes));
            });
        }

        private List<DALNode> checkCellSize(DALParser parser, List<HeaderNode> headers, List<DALNode> cellClauses) {
            if (cellClauses.size() != headers.size())
                throw parser.getSourceCode().syntaxError("Different cell size", 0);
            return cellClauses;
        }
    }

    private DALNode getRowCell(DALParser parser, Optional<DALOperator> rowOperator, HeaderNode headerNode) {
        int cellPosition = parser.getSourceCode().nextPosition();
        return oneOf(ELEMENT_ELLIPSIS, EMPTY_CELL).or(ROW_WILDCARD.or(
                shortJudgementClause(oneOf(JUDGEMENT_OPERATORS, headerNode.headerOperator(), parser1 -> rowOperator)
                        .or(Tokens.DEFAULT_JUDGEMENT_OPERATOR)).input(headerNode.getProperty()))).parse(parser)
                .setPositionBegin(cellPosition);
    }

    public class TransposedTable implements NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> {
        @Override
        public Optional<DALNode> parse(DALParser parser) {
            return parser.getSourceCode().popWord(">>").map(x -> {
                List<HeaderNode> headerNodes = new ArrayList<>();
                return new TableNode(headerNodes, getRowNodes(parser, headerNodes), TableNode.Type.TRANSPOSED);
            });
        }

        private List<RowNode> getRowNodes(DALParser parser, List<HeaderNode> headerNodes) {
            return transpose(allOptional(() -> parser.fetchNodeAfter2("|", TABLE_HEADER)
                    .map(HeaderNode.class::cast).map(headerNode -> {
                        headerNodes.add(headerNode);
                        return parser.fetchRow(row -> getRowCell(parser, empty(), headerNode))
                                .orElseThrow(() -> parser.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream().map(row -> new RowNode(empty(), empty(), empty(), row)).collect(toList());
        }
    }

    private Optional<Integer> getRowIndex(DALParser parser) {
        return INTEGER.parse(parser).map(node -> (Integer) ((ConstNode) node).getValue());
    }

    public class TransposedTableWithRowOperator implements NodeParser<DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALParser> {

        @Override
        public Optional<DALNode> parse(DALParser parser) {
            return parser.getSourceCode().tryFetch(() -> when(parser.getSourceCode().popWord("|").isPresent()
                    && parser.getSourceCode().popWord(">>").isPresent()).optional(() -> {
                List<Optional<Integer>> rowIndexes = new ArrayList<>();
                List<Optional<ExpressionClause<DALRuntimeContext, DALNode>>> rowSchemaClauses = new ArrayList<>();
                List<Optional<DALOperator>> rowOperators = parser.fetchRow(row -> {
                    rowIndexes.add(getRowIndex(parser));
                    rowSchemaClauses.add(parser.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE));
                    return JUDGEMENT_OPERATORS.parse(parser);
                }).orElse(emptyList());
                List<HeaderNode> headerNodes = new ArrayList<>();
                return new TableNode(headerNodes, getRowNodes(parser, headerNodes, rowSchemaClauses, rowOperators,
                        rowIndexes), TableNode.Type.TRANSPOSED);
            }));
        }

        private List<RowNode> getRowNodes(DALParser parser, List<HeaderNode> headerNodes,
                                          List<Optional<ExpressionClause<DALRuntimeContext, DALNode>>> rowSchemaClauses,
                                          List<Optional<DALOperator>> rowOperators, List<Optional<Integer>> rowIndexes) {
            return FunctionUtil.mapWithIndex(getCells(parser, headerNodes, rowOperators), (i, row) ->
                    new RowNode(rowIndexes.get(i), rowSchemaClauses.get(i), rowOperators.get(i), row)).collect(toList());
        }

        private Stream<List<DALNode>> getCells(DALParser parser, List<HeaderNode> headerNodes,
                                               List<Optional<DALOperator>> rowOperators) {
            return transpose(allOptional(() -> parser.fetchNodeAfter2("|", TABLE_HEADER).map(HeaderNode.class::cast)
                    .map(headerNode -> {
                        headerNodes.add(headerNode);
                        return parser.fetchRow(row -> getRowCell(parser, rowOperators.get(row), headerNode))
                                .orElseThrow(() -> parser.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream();
        }
    }

    private Optional<DALNode> compileUserDefinedLiteral(DALParser parser) {
        return parser.getSourceCode().tryFetch(() -> Tokens.IDENTITY_PROPERTY.scan(parser.getSourceCode())
                .flatMap(token -> parser.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }
}
