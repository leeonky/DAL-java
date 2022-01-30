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
import static com.github.leeonky.interpreter.ExpressionClauseFactory.after;
import static com.github.leeonky.interpreter.ExpressionClauseMatcher.oneOf;
import static com.github.leeonky.interpreter.FunctionUtil.allOptional;
import static com.github.leeonky.interpreter.FunctionUtil.transpose;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.NodeMatcher.oneOf;
import static com.github.leeonky.interpreter.OperatorMatcher.oneOf;
import static java.util.Collections.emptyList;
import static java.util.Optional.empty;
import static java.util.stream.Collectors.toList;

public class Compiler {

    private static final OperatorMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
            BINARY_ARITHMETIC_OPERATORS = oneOf(
            Operators.AND.operatorMatcher(() -> new DALOperator.And(Operators.AND.getLabel())),
            Operators.OR.operatorMatcher(() -> new DALOperator.Or(Operators.OR.getLabel())),
            Keywords.AND.operatorMatcher(() -> new DALOperator.And(Keywords.AND.getLabel())),
            Operators.COMMA.operatorMatcher(() -> new DALOperator.And(","), DALScanner::isEnableCommaAnd),
            Operators.NOT_EQUAL.operatorMatcher(DALOperator.NotEqual::new),
            Keywords.OR.operatorMatcher(() -> new DALOperator.Or(Keywords.OR.getLabel())),
            Operators.GREATER_OR_EQUAL.operatorMatcher(DALOperator.GreaterOrEqual::new),
            Operators.LESS_OR_EQUAL.operatorMatcher(DALOperator.LessOrEqual::new),
            Operators.GREATER.operatorMatcher(DALOperator.Greater::new),
            Operators.LESS.operatorMatcher(DALOperator.Less::new),
            Operators.PLUS.operatorMatcher(DALOperator.Plus::new),
            Operators.SUBTRACTION.operatorMatcher(DALOperator.Subtraction::new),
            Operators.MULTIPLICATION.operatorMatcher(DALOperator.Multiplication::new),
            Operators.DIVISION.operatorMatcher(DALOperator.Division::new));
    private static final OperatorMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
            UNARY_OPERATORS = oneOf(
            Operators.MINUS.<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
                    operatorMatcher(DALOperator.Minus::new, tokenParser -> !tokenParser.getSourceCode().isBeginning()),
            Operators.NOT.operatorMatcher(DALOperator.Not::new, tokenParser -> !tokenParser.getSourceCode().startsWith("!=")));
    private static final OperatorMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
            JUDGEMENT_OPERATORS = oneOf(
            Operators.MATCHER.<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
                    operatorMatcher(DALOperator.Matcher::new),
            Operators.EQUAL.operatorMatcher(DALOperator.Equal::new));

    private static final EscapeChars SINGLE_QUOTED_ESCAPES = new EscapeChars().escape("\\\\", '\\').escape("\\'", '\''),
            DOUBLE_QUOTED_ESCAPES = new EscapeChars().escape("\\\\", '\\').escape("\\n", '\n').escape("\\t", '\t')
                    .escape("\\\"", '"'),
            REGEX_ESCAPES = new EscapeChars().escape("\\/", '/');

    NodeMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
            INPUT = tokenParser -> when(tokenParser.getSourceCode().isBeginning()).optional(() -> InputNode.INSTANCE),
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
            TABLE = oneOf(new TransposedTableWithRowOperator(), new TableMatcher(), new TransposedTable());

    public NodeFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> PROPERTY_CHAIN, OPERAND, EXPRESSION,
            LIST_INDEX_OR_MAP_KEY, ARITHMETIC_EXPRESSION, JUDGEMENT_EXPRESSION_OPERAND;

    ExpressionClauseMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
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

    private ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> shortJudgementClause(
            OperatorFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> operatorFactory) {
        return ((ExpressionClauseMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>) parser ->
                parser.fetchClauseAfter(Notations.IS_s, parser1 -> schemaJudgement(parser, SCHEMA_CLAUSE.fetch(parser))))
                .or(parser -> parser.fetchExpressionClause(operatorFactory, JUDGEMENT_EXPRESSION_OPERAND));
    }

    private ExpressionClause<DALRuntimeContext, DALNode> schemaJudgement(
            Scanner<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> parser,
            ExpressionClause<DALRuntimeContext, DALNode> expressionClause) {
        return parser.fetchExpressionClause(JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND)
                .<ExpressionClause<DALRuntimeContext, DALNode>>map(clause -> previous ->
                        clause.makeExpression(expressionClause.makeExpression(previous))).orElse(expressionClause);
    }

    private static final ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
            SCHEMA_CLAUSE = new SchemaExpressionClauseFactory();

    public Compiler() {
        CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
                CONST_USER_DEFINED_LITERAL);
        LIST_INDEX_OR_MAP_KEY = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING)
                .or("should given one property or array index in `[]`");
        PARENTHESES = parser -> parser.enableCommaAnd(() -> parser.fetchNodeWithOneChildNodeBetween('(', ')',
                ParenthesesNode::new, EXPRESSION, "expect a value or expression"));
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(InputNode.INSTANCE), IDENTITY_PROPERTY);
        PROPERTY_CHAIN = parser -> PROPERTY.or("expect a object property").recursive(EXPLICIT_PROPERTY).fetch(parser);
        OBJECT = parser -> parser.disableCommaAnd(() -> parser.fetchNodeWithElementsBetween('{', '}', ObjectNode::new,
                () -> PROPERTY_CHAIN.withClause(shortJudgementClause(JUDGEMENT_OPERATORS.or("expect operator `:` or `=`"))).fetch(parser)));
        ELEMENT_ELLIPSIS = Notations.Operators.ELEMENT_ELLIPSIS.nodeMatcher(token -> new ListEllipsisNode());
        LIST = parser -> parser.disableCommaAnd(() -> parser.fetchNodeWithElementsBetween('[', ']', ListNode::new,
                () -> ELEMENT_ELLIPSIS.fetch(parser).<ExpressionClause<DALRuntimeContext, DALNode>>map(node -> p -> node).orElseGet(() ->
                        shortJudgementClause(JUDGEMENT_OPERATORS.or(Tokens.DEFAULT_JUDGEMENT_OPERATOR)).fetch(parser))));
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
                after(Notations.WHICH_S, which(SCHEMA_JUDGEMENT_CLAUSE.or(EXPRESSION)))).fetch(parser);
        EXPRESSION = OPERAND.recursive(oneOf(BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION));
    }

    private ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> which(
            NodeFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> nodeFactory) {
        return parser -> previous -> ((SchemaExpression) previous).which(nodeFactory.fetch(parser));
    }

    private ExpressionClauseMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> omitWhich(
            NodeMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> nodeMatcher) {
        return parser -> nodeMatcher.fetch(parser).map(node -> previous ->
                ((SchemaExpression) previous).omitWhich(node));
    }

    public List<DALNode> compile(SourceCode sourceCode, DALRuntimeContext DALRuntimeContext) {
        return new ArrayList<DALNode>() {{
            DALScanner parser = new DALScanner(sourceCode, DALRuntimeContext, DALExpression::new);
            add(EXPRESSION.fetch(parser));
            if (sourceCode.isBeginning() && sourceCode.hasCode())
                throw sourceCode.syntaxError("unexpected token", 0);
            while (sourceCode.hasCode())
                add(EXPRESSION.fetch(parser));
        }};
    }

    public List<Object> toChainNodes(String sourceCode) {
        return ((PropertyNode) PROPERTY_CHAIN.fetch(new DALScanner(new SourceCode(sourceCode),
                null, DALExpression::new))).getChain();
    }

    private final NodeFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
            SEQUENCE = parser -> FunctionUtil.oneOf(
            parser.sequenceOf(SEQUENCE_AZ, SequenceNode.Type.AZ),
            parser.sequenceOf(SEQUENCE_ZA, SequenceNode.Type.ZA),
            parser.sequenceOf(SEQUENCE_AZ_2, SequenceNode.Type.AZ),
            parser.sequenceOf(SEQUENCE_ZA_2, SequenceNode.Type.ZA))
            .orElse(SequenceNode.noSequence());

    private final NodeFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> TABLE_HEADER = parser -> {
        SequenceNode sequence = (SequenceNode) SEQUENCE.fetch(parser);
        DALNode property = PROPERTY_CHAIN.fetch(parser);
        return new HeaderNode(sequence, parser.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE)
                .map(expressionClause -> expressionClause.makeExpression(property)).orElse(property),
                JUDGEMENT_OPERATORS.fetch(parser));
    };

    public class TableMatcher implements NodeMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> {
        @Override
        public Optional<DALNode> fetch(DALScanner parser) {
            try {
                return parser.fetchRow(columnIndex -> (HeaderNode) TABLE_HEADER.fetch(parser))
                        .map(headers -> new TableNode(headers, getRowNodes(parser, headers)));
            } catch (IndexOutOfBoundsException ignore) {
                throw parser.getSourceCode().syntaxError("Different cell size", 0);
            }
        }

        protected List<RowNode> getRowNodes(DALScanner parser, List<HeaderNode> headers) {
            return allOptional(() -> {
                Optional<Integer> index = getRowIndex(parser);
                Optional<ExpressionClause<DALRuntimeContext, DALNode>> rowSchemaClause = parser.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE);
                Optional<DALOperator> rowOperator = JUDGEMENT_OPERATORS.fetch(parser);
                return FunctionUtil.oneOf(
                        () -> parser.fetchNodeBetween("|", "|", ELEMENT_ELLIPSIS).map(Collections::singletonList),
                        () -> parser.fetchNodeBetween("|", "|", ROW_WILDCARD).map(Collections::singletonList),
                        () -> parser.fetchRow(column -> getRowCell(parser, rowOperator, headers.get(column)))
                                .map(cellClauses -> checkCellSize(parser, headers, cellClauses))
                ).map(nodes -> new RowNode(index, rowSchemaClause, rowOperator, nodes));
            });
        }

        private List<DALNode> checkCellSize(DALScanner parser, List<HeaderNode> headers, List<DALNode> cellClauses) {
            if (cellClauses.size() != headers.size())
                throw parser.getSourceCode().syntaxError("Different cell size", 0);
            return cellClauses;
        }
    }

    private DALNode getRowCell(DALScanner parser, Optional<DALOperator> rowOperator, HeaderNode headerNode) {
        int cellPosition = parser.getSourceCode().nextPosition();
        return oneOf(ELEMENT_ELLIPSIS, EMPTY_CELL).or(ROW_WILDCARD.or(
                shortJudgementClause(oneOf(JUDGEMENT_OPERATORS, headerNode.headerOperator(), parser1 -> rowOperator)
                        .or(Tokens.DEFAULT_JUDGEMENT_OPERATOR)).input(headerNode.getProperty()))).fetch(parser)
                .setPositionBegin(cellPosition);
    }

    public class TransposedTable implements NodeMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> {
        @Override
        public Optional<DALNode> fetch(DALScanner parser) {
            return parser.getSourceCode().popWord(">>").map(x -> {
                List<HeaderNode> headerNodes = new ArrayList<>();
                return new TableNode(headerNodes, getRowNodes(parser, headerNodes), TableNode.Type.TRANSPOSED);
            });
        }

        private List<RowNode> getRowNodes(DALScanner parser, List<HeaderNode> headerNodes) {
            return transpose(allOptional(() -> parser.fetchNodeAfter2("|", TABLE_HEADER)
                    .map(HeaderNode.class::cast).map(headerNode -> {
                        headerNodes.add(headerNode);
                        return parser.fetchRow(row -> getRowCell(parser, empty(), headerNode))
                                .orElseThrow(() -> parser.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream().map(row -> new RowNode(empty(), empty(), empty(), row)).collect(toList());
        }
    }

    private Optional<Integer> getRowIndex(DALScanner parser) {
        return INTEGER.fetch(parser).map(node -> (Integer) ((ConstNode) node).getValue());
    }

    public class TransposedTableWithRowOperator implements NodeMatcher<DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALScanner> {

        @Override
        public Optional<DALNode> fetch(DALScanner parser) {
            return parser.getSourceCode().tryFetch(() -> when(parser.getSourceCode().popWord("|").isPresent()
                    && parser.getSourceCode().popWord(">>").isPresent()).optional(() -> {
                List<Optional<Integer>> rowIndexes = new ArrayList<>();
                List<Optional<ExpressionClause<DALRuntimeContext, DALNode>>> rowSchemaClauses = new ArrayList<>();
                List<Optional<DALOperator>> rowOperators = parser.fetchRow(row -> {
                    rowIndexes.add(getRowIndex(parser));
                    rowSchemaClauses.add(parser.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE));
                    return JUDGEMENT_OPERATORS.fetch(parser);
                }).orElse(emptyList());
                List<HeaderNode> headerNodes = new ArrayList<>();
                return new TableNode(headerNodes, getRowNodes(parser, headerNodes, rowSchemaClauses, rowOperators,
                        rowIndexes), TableNode.Type.TRANSPOSED);
            }));
        }

        private List<RowNode> getRowNodes(DALScanner parser, List<HeaderNode> headerNodes,
                                          List<Optional<ExpressionClause<DALRuntimeContext, DALNode>>> rowSchemaClauses,
                                          List<Optional<DALOperator>> rowOperators, List<Optional<Integer>> rowIndexes) {
            return FunctionUtil.mapWithIndex(getCells(parser, headerNodes, rowOperators), (i, row) ->
                    new RowNode(rowIndexes.get(i), rowSchemaClauses.get(i), rowOperators.get(i), row)).collect(toList());
        }

        private Stream<List<DALNode>> getCells(DALScanner parser, List<HeaderNode> headerNodes,
                                               List<Optional<DALOperator>> rowOperators) {
            return transpose(allOptional(() -> parser.fetchNodeAfter2("|", TABLE_HEADER).map(HeaderNode.class::cast)
                    .map(headerNode -> {
                        headerNodes.add(headerNode);
                        return parser.fetchRow(row -> getRowCell(parser, rowOperators.get(row), headerNode))
                                .orElseThrow(() -> parser.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream();
        }
    }

    private Optional<DALNode> compileUserDefinedLiteral(DALScanner parser) {
        return parser.getSourceCode().tryFetch(() -> Tokens.IDENTITY_PROPERTY.fetch(parser.getSourceCode())
                .flatMap(token -> parser.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }
}
