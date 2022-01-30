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
                    operatorMatcher(DALOperator.Minus::new, scanner -> !scanner.getSourceCode().isBeginning()),
            Operators.NOT.operatorMatcher(DALOperator.Not::new, scanner -> !scanner.getSourceCode().startsWith("!=")));
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
            INPUT = scanner -> when(scanner.getSourceCode().isBeginning()).optional(() -> InputNode.INSTANCE),
            NUMBER = Tokens.NUMBER.nodeMatcher(token -> new ConstNode(token.getNumber())),
            INTEGER = Tokens.INTEGER.nodeMatcher(token -> new ConstNode(token.getInteger())),
            SINGLE_QUOTED_STRING = scanner -> scanner.fetchString('\'', '\'', ConstNode::new, SINGLE_QUOTED_ESCAPES),
            DOUBLE_QUOTED_STRING = scanner -> scanner.fetchString('"', '"', ConstNode::new, DOUBLE_QUOTED_ESCAPES),
            CONST_TRUE = Keywords.TRUE.nodeMatcher(token -> new ConstNode(true)),
            CONST_FALSE = Keywords.FALSE.nodeMatcher(token -> new ConstNode(false)),
            CONST_NULL = Keywords.NULL.nodeMatcher(token -> new ConstNode(null)),
            CONST_USER_DEFINED_LITERAL = this::compileUserDefinedLiteral,
            REGEX = scanner -> scanner.fetchString('/', '/', RegexNode::new, REGEX_ESCAPES),
            IDENTITY_PROPERTY = Tokens.IDENTITY_PROPERTY.nodeMatcher(token ->
                    new PropertyNode(InputNode.INSTANCE, token.getContent(), IDENTIFIER)),
            WILDCARD = Notations.Operators.WILDCARD.nodeMatcher(token -> new WildcardNode(token.getContent())),
            ROW_WILDCARD = Notations.Operators.ROW_WILDCARD.nodeMatcher(token -> new WildcardNode(token.getContent())),
            PROPERTY, OBJECT, LIST, CONST, PARENTHESES, JUDGEMENT, SCHEMA_JUDGEMENT_CLAUSE,
            ELEMENT_ELLIPSIS, UNARY_OPERATOR_EXPRESSION,
            EMPTY_CELL = scanner -> when(scanner.getSourceCode().startsWith("|")).optional(EmptyCellNode::new),
            TABLE = oneOf(new TransposedTableWithRowOperator(), new TableMatcher(), new TransposedTable());

    public NodeFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> PROPERTY_CHAIN, OPERAND, EXPRESSION,
            LIST_INDEX_OR_MAP_KEY, ARITHMETIC_EXPRESSION, JUDGEMENT_EXPRESSION_OPERAND;

    ExpressionClauseMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
            DOT_PROPERTY = Tokens.DOT_PROPERTY.toClauseMatcher((token, previous) -> new PropertyNode(previous,
            token.getContentOrThrow("property is not finished"), DOT)),
            BRACKET_PROPERTY = scanner -> scanner.fetchExpressionClauseBetween('[', ']', (previous, node) ->
                            new PropertyNode(previous, ((ConstNode) node).getValue(), BRACKET), LIST_INDEX_OR_MAP_KEY,
                    "should given one property or array index in `[]`"),
            EXPLICIT_PROPERTY = oneOf(DOT_PROPERTY, BRACKET_PROPERTY),
            BINARY_ARITHMETIC_EXPRESSION,
            BINARY_JUDGEMENT_EXPRESSION,
            BINARY_OPERATOR_EXPRESSION,
            SCHEMA_EXPRESSION;

    private ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> shortJudgementClause(
            OperatorFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> operatorFactory) {
        return ((ExpressionClauseMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>) scanner ->
                scanner.fetchClauseAfter(Notations.IS_s, scanner1 -> schemaJudgement(scanner, SCHEMA_CLAUSE.fetch(scanner))))
                .or(scanner -> scanner.fetchExpressionClause(operatorFactory, JUDGEMENT_EXPRESSION_OPERAND));
    }

    private ExpressionClause<DALRuntimeContext, DALNode> schemaJudgement(
            Scanner<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> scanner,
            ExpressionClause<DALRuntimeContext, DALNode> expressionClause) {
        return scanner.fetchExpressionClause(JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND)
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
        PARENTHESES = scanner -> scanner.enableCommaAnd(() -> scanner.fetchNodeWithOneChildNodeBetween('(', ')',
                ParenthesesNode::new, EXPRESSION, "expect a value or expression"));
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(InputNode.INSTANCE), IDENTITY_PROPERTY);
        PROPERTY_CHAIN = scanner -> PROPERTY.or("expect a object property").recursive(EXPLICIT_PROPERTY).fetch(scanner);
        OBJECT = scanner -> scanner.disableCommaAnd(() -> scanner.fetchNodeWithElementsBetween('{', '}', ObjectNode::new,
                () -> PROPERTY_CHAIN.withClause(shortJudgementClause(JUDGEMENT_OPERATORS.or("expect operator `:` or `=`"))).fetch(scanner)));
        ELEMENT_ELLIPSIS = Notations.Operators.ELEMENT_ELLIPSIS.nodeMatcher(token -> new ListEllipsisNode());
        LIST = scanner -> scanner.disableCommaAnd(() -> scanner.fetchNodeWithElementsBetween('[', ']', ListNode::new,
                () -> ELEMENT_ELLIPSIS.fetch(scanner).<ExpressionClause<DALRuntimeContext, DALNode>>map(node -> p -> node).orElseGet(() ->
                        shortJudgementClause(JUDGEMENT_OPERATORS.or(Tokens.DEFAULT_JUDGEMENT_OPERATOR)).fetch(scanner))));
        JUDGEMENT = oneOf(REGEX, OBJECT, LIST, WILDCARD, TABLE);
        UNARY_OPERATOR_EXPRESSION = scanner -> scanner.fetchExpression(null, UNARY_OPERATORS, OPERAND);
        OPERAND = UNARY_OPERATOR_EXPRESSION.or(oneOf(CONST, PROPERTY, PARENTHESES, INPUT)
                .or("expect a value or expression").recursive(EXPLICIT_PROPERTY).map(DALNode::avoidListMapping));
        BINARY_ARITHMETIC_EXPRESSION = BINARY_ARITHMETIC_OPERATORS.toClause(OPERAND);
        BINARY_JUDGEMENT_EXPRESSION = JUDGEMENT_OPERATORS.toClause(JUDGEMENT.or(OPERAND));
        BINARY_OPERATOR_EXPRESSION = oneOf(BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION);
        ARITHMETIC_EXPRESSION = OPERAND.recursive(BINARY_ARITHMETIC_EXPRESSION);
        JUDGEMENT_EXPRESSION_OPERAND = JUDGEMENT.or(ARITHMETIC_EXPRESSION);
        SCHEMA_JUDGEMENT_CLAUSE = scanner -> scanner.fetchExpression(
                InputNode.INSTANCE, JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND);
        SCHEMA_EXPRESSION = scanner -> after(Notations.IS_s, SCHEMA_CLAUSE).concat(omitWhich(SCHEMA_JUDGEMENT_CLAUSE),
                after(Notations.WHICH_S, which(SCHEMA_JUDGEMENT_CLAUSE.or(EXPRESSION)))).fetch(scanner);
        EXPRESSION = OPERAND.recursive(oneOf(BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION));
    }

    private ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> which(
            NodeFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> nodeFactory) {
        return scanner -> previous -> ((SchemaExpression) previous).which(nodeFactory.fetch(scanner));
    }

    private ExpressionClauseMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> omitWhich(
            NodeMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> nodeMatcher) {
        return scanner -> nodeMatcher.fetch(scanner).map(node -> previous ->
                ((SchemaExpression) previous).omitWhich(node));
    }

    public List<DALNode> compile(SourceCode sourceCode, DALRuntimeContext DALRuntimeContext) {
        return new ArrayList<DALNode>() {{
            DALScanner scanner = new DALScanner(sourceCode, DALRuntimeContext, DALExpression::new);
            add(EXPRESSION.fetch(scanner));
            if (sourceCode.isBeginning() && sourceCode.hasCode())
                throw sourceCode.syntaxError("unexpected token", 0);
            while (sourceCode.hasCode())
                add(EXPRESSION.fetch(scanner));
        }};
    }

    public List<Object> toChainNodes(String sourceCode) {
        return ((PropertyNode) PROPERTY_CHAIN.fetch(new DALScanner(new SourceCode(sourceCode),
                null, DALExpression::new))).getChain();
    }

    private final NodeFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner>
            SEQUENCE = scanner -> FunctionUtil.oneOf(
            scanner.sequenceOf(SEQUENCE_AZ, SequenceNode.Type.AZ),
            scanner.sequenceOf(SEQUENCE_ZA, SequenceNode.Type.ZA),
            scanner.sequenceOf(SEQUENCE_AZ_2, SequenceNode.Type.AZ),
            scanner.sequenceOf(SEQUENCE_ZA_2, SequenceNode.Type.ZA))
            .orElse(SequenceNode.noSequence());

    private final NodeFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> TABLE_HEADER = scanner -> {
        SequenceNode sequence = (SequenceNode) SEQUENCE.fetch(scanner);
        DALNode property = PROPERTY_CHAIN.fetch(scanner);
        return new HeaderNode(sequence, scanner.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE)
                .map(expressionClause -> expressionClause.makeExpression(property)).orElse(property),
                JUDGEMENT_OPERATORS.fetch(scanner));
    };

    public class TableMatcher implements NodeMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> {
        @Override
        public Optional<DALNode> fetch(DALScanner scanner) {
            try {
                return scanner.fetchRow(columnIndex -> (HeaderNode) TABLE_HEADER.fetch(scanner))
                        .map(headers -> new TableNode(headers, getRowNodes(scanner, headers)));
            } catch (IndexOutOfBoundsException ignore) {
                throw scanner.getSourceCode().syntaxError("Different cell size", 0);
            }
        }

        protected List<RowNode> getRowNodes(DALScanner scanner, List<HeaderNode> headers) {
            return allOptional(() -> {
                Optional<Integer> index = getRowIndex(scanner);
                Optional<ExpressionClause<DALRuntimeContext, DALNode>> rowSchemaClause = scanner.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE);
                Optional<DALOperator> rowOperator = JUDGEMENT_OPERATORS.fetch(scanner);
                return FunctionUtil.oneOf(
                        () -> scanner.fetchNodeBetween("|", "|", ELEMENT_ELLIPSIS).map(Collections::singletonList),
                        () -> scanner.fetchNodeBetween("|", "|", ROW_WILDCARD).map(Collections::singletonList),
                        () -> scanner.fetchRow(column -> getRowCell(scanner, rowOperator, headers.get(column)))
                                .map(cellClauses -> checkCellSize(scanner, headers, cellClauses))
                ).map(nodes -> new RowNode(index, rowSchemaClause, rowOperator, nodes));
            });
        }

        private List<DALNode> checkCellSize(DALScanner scanner, List<HeaderNode> headers, List<DALNode> cellClauses) {
            if (cellClauses.size() != headers.size())
                throw scanner.getSourceCode().syntaxError("Different cell size", 0);
            return cellClauses;
        }
    }

    private DALNode getRowCell(DALScanner scanner, Optional<DALOperator> rowOperator, HeaderNode headerNode) {
        int cellPosition = scanner.getSourceCode().nextPosition();
        return oneOf(ELEMENT_ELLIPSIS, EMPTY_CELL).or(ROW_WILDCARD.or(
                shortJudgementClause(oneOf(JUDGEMENT_OPERATORS, headerNode.headerOperator(), scanner1 -> rowOperator)
                        .or(Tokens.DEFAULT_JUDGEMENT_OPERATOR)).input(headerNode.getProperty()))).fetch(scanner)
                .setPositionBegin(cellPosition);
    }

    public class TransposedTable implements NodeMatcher<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> {
        @Override
        public Optional<DALNode> fetch(DALScanner scanner) {
            return scanner.getSourceCode().popWord(">>").map(x -> {
                List<HeaderNode> headerNodes = new ArrayList<>();
                return new TableNode(headerNodes, getRowNodes(scanner, headerNodes), TableNode.Type.TRANSPOSED);
            });
        }

        private List<RowNode> getRowNodes(DALScanner scanner, List<HeaderNode> headerNodes) {
            return transpose(allOptional(() -> scanner.fetchNodeAfter2("|", TABLE_HEADER)
                    .map(HeaderNode.class::cast).map(headerNode -> {
                        headerNodes.add(headerNode);
                        return scanner.fetchRow(row -> getRowCell(scanner, empty(), headerNode))
                                .orElseThrow(() -> scanner.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream().map(row -> new RowNode(empty(), empty(), empty(), row)).collect(toList());
        }
    }

    private Optional<Integer> getRowIndex(DALScanner scanner) {
        return INTEGER.fetch(scanner).map(node -> (Integer) ((ConstNode) node).getValue());
    }

    public class TransposedTableWithRowOperator implements NodeMatcher<DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALScanner> {

        @Override
        public Optional<DALNode> fetch(DALScanner scanner) {
            return scanner.getSourceCode().tryFetch(() -> when(scanner.getSourceCode().popWord("|").isPresent()
                    && scanner.getSourceCode().popWord(">>").isPresent()).optional(() -> {
                List<Optional<Integer>> rowIndexes = new ArrayList<>();
                List<Optional<ExpressionClause<DALRuntimeContext, DALNode>>> rowSchemaClauses = new ArrayList<>();
                List<Optional<DALOperator>> rowOperators = scanner.fetchRow(row -> {
                    rowIndexes.add(getRowIndex(scanner));
                    rowSchemaClauses.add(scanner.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE));
                    return JUDGEMENT_OPERATORS.fetch(scanner);
                }).orElse(emptyList());
                List<HeaderNode> headerNodes = new ArrayList<>();
                return new TableNode(headerNodes, getRowNodes(scanner, headerNodes, rowSchemaClauses, rowOperators,
                        rowIndexes), TableNode.Type.TRANSPOSED);
            }));
        }

        private List<RowNode> getRowNodes(DALScanner scanner, List<HeaderNode> headerNodes,
                                          List<Optional<ExpressionClause<DALRuntimeContext, DALNode>>> rowSchemaClauses,
                                          List<Optional<DALOperator>> rowOperators, List<Optional<Integer>> rowIndexes) {
            return FunctionUtil.mapWithIndex(getCells(scanner, headerNodes, rowOperators), (i, row) ->
                    new RowNode(rowIndexes.get(i), rowSchemaClauses.get(i), rowOperators.get(i), row)).collect(toList());
        }

        private Stream<List<DALNode>> getCells(DALScanner scanner, List<HeaderNode> headerNodes,
                                               List<Optional<DALOperator>> rowOperators) {
            return transpose(allOptional(() -> scanner.fetchNodeAfter2("|", TABLE_HEADER).map(HeaderNode.class::cast)
                    .map(headerNode -> {
                        headerNodes.add(headerNode);
                        return scanner.fetchRow(row -> getRowCell(scanner, rowOperators.get(row), headerNode))
                                .orElseThrow(() -> scanner.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream();
        }
    }

    private Optional<DALNode> compileUserDefinedLiteral(DALScanner scanner) {
        return scanner.getSourceCode().tryFetch(() -> Tokens.IDENTITY_PROPERTY.fetch(scanner.getSourceCode())
                .flatMap(token -> scanner.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }
}
