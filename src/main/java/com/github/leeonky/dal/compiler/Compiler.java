package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.ast.table.RowNode;
import com.github.leeonky.dal.ast.table.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.DALNode.constNode;
import static com.github.leeonky.dal.compiler.DALProcedure.enableCommaAnd;
import static com.github.leeonky.dal.compiler.Notations.*;
import static com.github.leeonky.interpreter.ClauseParser.oneOf;
import static com.github.leeonky.interpreter.FunctionUtil.*;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.NodeParser.lazy;
import static com.github.leeonky.interpreter.NodeParser.oneOf;
import static com.github.leeonky.interpreter.Notation.notation;
import static com.github.leeonky.interpreter.OperatorParser.oneOf;
import static com.github.leeonky.interpreter.Parser.endWith;
import static com.github.leeonky.interpreter.Sequence.atLeast;
import static com.github.leeonky.interpreter.Sequence.severalTimes;
import static java.util.Collections.singletonList;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static java.util.stream.Collectors.toList;

public class Compiler {

    private static final OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            BINARY_ARITHMETIC_OPERATORS = oneOf(
            Operators.AND.operatorParser(DALOperator::operatorAnd),
            Operators.OR.operatorParser(DALOperator::operatorOr),
            Keywords.AND.operatorParser(DALOperator::keywordAnd),
            Operators.COMMA.operatorParser(DALOperator::commaAnd, DALProcedure::isEnableCommaAnd),
            Operators.NOT_EQUAL.operatorParser(DALOperator.NotEqual::new),
            Keywords.OR.operatorParser(DALOperator::keywordOr),
            Operators.GREATER_OR_EQUAL.operatorParser(DALOperator.GreaterOrEqual::new),
            Operators.LESS_OR_EQUAL.operatorParser(DALOperator.LessOrEqual::new),
            Operators.GREATER.operatorParser(DALOperator.Greater::new),
            Operators.LESS.operatorParser(DALOperator.Less::new),
            Operators.PLUS.operatorParser(DALOperator.Plus::new),
            Operators.SUBTRACTION.operatorParser(DALOperator.Subtraction::new),
            Operators.MULTIPLICATION.operatorParser(DALOperator.Multiplication::new),
            Operators.DIVISION.operatorParser(DALOperator.Division::new));

    private static final OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            PROPERTY_DOT = Operators.DOT.operatorParser(DALOperator.PropertyDot::new, not(DALProcedure::mayBeElementEllipsis));

    private static final OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            UNARY_OPERATORS = oneOf(
            Operators.MINUS.operatorParser(DALOperator.Minus::new, not(DALProcedure::isCodeBeginning)),
            Operators.NOT.operatorParser(DALOperator.Not::new, not(DALProcedure::mayBeUnEqual)));

    private static final OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            JUDGEMENT_OPERATORS = oneOf(
            Operators.MATCHER.<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
                    operatorParser(DALOperator.Matcher::new),
            Operators.EQUAL.operatorParser(DALOperator.Equal::new));

    private static final OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            IS = Operators.IS.operatorParser(DALOperator.Is::new),
            WHICH = Operators.WHICH.operatorParser(DALOperator.Which::new);
    public static final Notation TRANSPOSE_MARK = notation(">>");

    @Deprecated
    public static boolean NewTransposed = false;
    public static boolean NewTable = false;

    private static OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            PROPERTY_IMPLICIT = procedure -> of(new DALOperator.PropertyImplicit());

    private static final OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
//    TODO remove default matcher logic
            DEFAULT_JUDGEMENT_OPERATOR = procedure -> procedure.currentOperator().orElseGet(DALOperator.Matcher::new);

    private static final EscapeChars SINGLE_QUOTED_ESCAPES = new EscapeChars()
            .escape("\\\\", '\\')
            .escape("\\'", '\'');
    private static final EscapeChars DOUBLE_QUOTED_ESCAPES = new EscapeChars()
            .escape("\\\\", '\\')
            .escape("\\n", '\n')
            .escape("\\r", '\r')
            .escape("\\t", '\t')
            .escape("\\\"", '"');
    private static final EscapeChars REGEX_ESCAPES = new EscapeChars()
            .escape("\\/", '/');

    NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            PROPERTY, OBJECT, LIST, PARENTHESES, JUDGEMENT,
            INPUT = procedure -> when(procedure.isCodeBeginning()).optional(() -> InputNode.INSTANCE),
            NUMBER = Tokens.NUMBER.nodeParser(constNode(Token::getNumber)),
            INTEGER = Tokens.INTEGER.nodeParser(constNode(Token::getInteger)),
            SINGLE_QUOTED_STRING = SINGLE_QUOTED.and(charNode(SINGLE_QUOTED_ESCAPES).sequence(severalTimes().endWith(
                    SINGLE_QUOTED.getLabel()), DALNode::constString)),
            DOUBLE_QUOTED_STRING = DOUBLE_QUOTED.and(charNode(DOUBLE_QUOTED_ESCAPES).sequence(severalTimes().endWith(
                    DOUBLE_QUOTED.getLabel()), DALNode::constString)),
            CONST_TRUE = Keywords.TRUE.nodeParser(DALNode::constTrue),
            CONST_FALSE = Keywords.FALSE.nodeParser(DALNode::constFalse),
            CONST_NULL = Keywords.NULL.nodeParser(DALNode::constNull),
            CONST_USER_DEFINED_LITERAL = this::compileUserDefinedLiteral,
            REGEX = REGEX_NOTATION.and(charNode(REGEX_ESCAPES).sequence(severalTimes().endWith(
                    REGEX_NOTATION.getLabel()), DALNode::regex)),
            IMPLICIT_PROPERTY = PROPERTY_IMPLICIT.clause(Tokens.SYMBOL.nodeParser(DALNode::symbolNode))
                    .defaultInputNode(InputNode.INSTANCE),
            WILDCARD = Notations.Operators.WILDCARD.nodeParser(WildcardNode::new),
            ROW_WILDCARD = Notations.Operators.ROW_WILDCARD.nodeParser(WildcardNode::new),
            CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
                    CONST_USER_DEFINED_LITERAL),
            ELEMENT_ELLIPSIS = Operators.ELEMENT_ELLIPSIS.nodeParser(token -> new ListEllipsisNode()),
            EMPTY_CELL = procedure -> when(procedure.emptyCell()).optional(EmptyCellNode::new),
            TABLE = oneOf(new TransposedTableWithRowOperator(), new TableParserBk(), new TransposedTable()),
            SCHEMA = Tokens.SCHEMA.nodeParser(DALNode::schema),
            INTEGER_OR_STRING = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING);

    public NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SCHEMA_COMPOSE = OPENING_BRACKET.and(SCHEMA.mandatory("expect a schema").sequence(atLeast(1,
            "expect at least one schema").splitBy(SCHEMA_AND).endWith(CLOSING_BRACKET), DALNode::elementSchemas))
            .or(SCHEMA.mandatory("expect a schema").sequence(severalTimes().splitBy(SCHEMA_AND), DALNode::schemas)),
            PROPERTY_CHAIN, OPERAND, EXPRESSION, SHORT_JUDGEMENT_OPERAND;

    public NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SYMBOL = Tokens.SYMBOL.nodeParser(DALNode::symbolNode);

    public ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            ARITHMETIC_CLAUSE, JUDGEMENT_CLAUSE,
            SCHEMA_CLAUSE = IS.clause(SCHEMA_COMPOSE),
            WHICH_CLAUSE = ClauseParser.lazy(() -> WHICH.clause(EXPRESSION)),

    EXPLICIT_PROPERTY = oneOf(PROPERTY_DOT.clause(Tokens.DOT_SYMBOL.nodeParser(DALNode::symbolNode).mandatory(
            "expect a symbol")), PROPERTY_IMPLICIT.clause(OPENING_BRACKET.and(INTEGER_OR_STRING.mandatory(
            "should given one property or array index in `[]`").map(DALNode::bracketSymbolNode).closeBy(endWith(CLOSING_BRACKET)))));

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALProcedure> shortJudgementClause(OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALProcedure> operatorMandatory) {
        return procedure -> SCHEMA_CLAUSE.concat(JUDGEMENT_OPERATORS.clause(SHORT_JUDGEMENT_OPERAND))
                .or(operatorMandatory.clause(SHORT_JUDGEMENT_OPERAND)).parse(procedure);
    }

    private ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> ARITHMETIC_CLAUSE_CHAIN,
            JUDGEMENT_CLAUSE_CHAIN, EXPLICIT_PROPERTY_CHAIN, WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN, EXPRESSION_CLAUSE;

    public Compiler() {
        PARENTHESES = lazy(() -> enableCommaAnd(OPENING_PARENTHESES.and(
                EXPRESSION.closeBy(endWith(CLOSING_PARENTHESES)).map(DALNode::parenthesesNode))));
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(InputNode.INSTANCE), IMPLICIT_PROPERTY);
        PROPERTY_CHAIN = PROPERTY.mandatory("expect a object property").recursive(EXPLICIT_PROPERTY);
        OBJECT = DALProcedure.disableCommaAnd(OPENING_BRACES.and(PROPERTY_CHAIN.expression(shortJudgementClause(
                JUDGEMENT_OPERATORS.mandatory("expect operator `:` or `=`"))).sequence(severalTimes().
                optionalSplitBy(Notations.COMMA).endWith(CLOSING_BRACES), ObjectScopeNode::new)));
        LIST = DALProcedure.disableCommaAnd(OPENING_BRACKET.and(ELEMENT_ELLIPSIS.ignoreInput().or(shortJudgementClause(
                JUDGEMENT_OPERATORS.or(DEFAULT_JUDGEMENT_OPERATOR))).sequence(severalTimes().optionalSplitBy(COMMA)
                .endWith(CLOSING_BRACKET), ListScopeNode::new)));
        JUDGEMENT = oneOf(REGEX, OBJECT, LIST, WILDCARD, TABLE);
        OPERAND = lazy(() -> oneOf(UNARY_OPERATORS.unary(OPERAND), CONST, PROPERTY, PARENTHESES, INPUT))
                .mandatory("expect a value or expression").map(DALNode::avoidListMapping);
        ARITHMETIC_CLAUSE = BINARY_ARITHMETIC_OPERATORS.clause(OPERAND);
        JUDGEMENT_CLAUSE = JUDGEMENT_OPERATORS.clause(JUDGEMENT.or(OPERAND));
        ARITHMETIC_CLAUSE_CHAIN = ClauseParser.lazy(() -> ARITHMETIC_CLAUSE.concat(EXPRESSION_CLAUSE));
        JUDGEMENT_CLAUSE_CHAIN = ClauseParser.lazy(() -> JUDGEMENT_CLAUSE.concat(EXPRESSION_CLAUSE));
        EXPLICIT_PROPERTY_CHAIN = ClauseParser.lazy(() -> EXPLICIT_PROPERTY.concat(EXPRESSION_CLAUSE));
        WHICH_CLAUSE_CHAIN = ClauseParser.lazy(() -> WHICH_CLAUSE.concat(EXPRESSION_CLAUSE));
        SCHEMA_CLAUSE_CHAIN = ClauseParser.lazy(() -> SCHEMA_CLAUSE.concat(oneOf(JUDGEMENT_CLAUSE_CHAIN,
                WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN)));
        EXPRESSION_CLAUSE = oneOf(ARITHMETIC_CLAUSE_CHAIN, JUDGEMENT_CLAUSE_CHAIN, EXPLICIT_PROPERTY_CHAIN,
                WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN);
        EXPRESSION = OPERAND.concat(EXPRESSION_CLAUSE);
        SHORT_JUDGEMENT_OPERAND = JUDGEMENT.or(OPERAND.recursive(oneOf(ARITHMETIC_CLAUSE, /*need test*/EXPLICIT_PROPERTY)));
    }

    public List<DALNode> compile(SourceCode sourceCode, DALRuntimeContext DALRuntimeContext) {
        return new ArrayList<DALNode>() {{
            DALProcedure dalParser = new DALProcedure(sourceCode, DALRuntimeContext, DALExpression::new);
            add(EXPRESSION.parse(dalParser));
            if (sourceCode.isBeginning() && sourceCode.hasCode())
                throw sourceCode.syntaxError("unexpected token", 0);
            while (sourceCode.hasCode())
                add(EXPRESSION.parse(dalParser));
        }};
    }

    private static NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> charNode(
            EscapeChars escapeChars) {
        return procedure -> new ConstNode(procedure.getSourceCode().popChar(escapeChars));
    }

    public List<Object> toChainNodes(String sourceCode) {
        return PROPERTY_CHAIN.parse(new DALProcedure(new SourceCode(sourceCode),
                null, DALExpression::new)).propertyChain();
    }

    private final NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SEQUENCE_AZ = Notations.SEQUENCE_AZ.nodeParser(SortNode::new),
            SEQUENCE_ZA = Notations.SEQUENCE_ZA.nodeParser(SortNode::new),
            SEQUENCE_AZ_2 = Notations.SEQUENCE_AZ_2.nodeParser(SortNode::new),
            SEQUENCE_ZA_2 = Notations.SEQUENCE_ZA_2.nodeParser(SortNode::new);

    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SEQUENCE = oneOf(SEQUENCE_AZ.sequence(severalTimes(), SortSequenceNode::new),
            SEQUENCE_ZA.sequence(severalTimes(), SortSequenceNode::new),
            SEQUENCE_AZ_2.sequence(severalTimes(), SortSequenceNode::new),
            SEQUENCE_ZA_2.sequence(severalTimes(), SortSequenceNode::new))
            .or(procedure -> SortSequenceNode.noSequence());

    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> TABLE_HEADER = procedure -> {
        SortSequenceNode sequence = (SortSequenceNode) SEQUENCE.parse(procedure);
        DALNode property = PROPERTY_CHAIN.parse(procedure);
        return new HeaderNodeBk(sequence, SCHEMA_CLAUSE.parse(procedure)
                .map(expressionClause -> expressionClause.makeExpression(property)).orElse(property),
                JUDGEMENT_OPERATORS.parse(procedure));
    };

    private final NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            TRANSPOSED_TABLE_HEADER = COLUMN_SPLITTER.before(TABLE_HEADER);

    public class TableParserBk implements NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> {
        private final NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
                ELLIPSIS_ROW = COLUMN_SPLITTER.before(ELEMENT_ELLIPSIS.closeBy(endWith(COLUMN_SPLITTER))),
                WILDCARD_ROW = COLUMN_SPLITTER.before(ROW_WILDCARD.closeBy(endWith(COLUMN_SPLITTER)));

        @Override
        public Optional<DALNode> parse(DALProcedure procedure) {
            if (NewTable)
                return new TableParser().parse(procedure);
            return COLUMN_SPLITTER.before(TABLE_HEADER.sequence(byTableRow(),
                    nodes -> {
                        List<HeaderNodeBk> headers = nodes.stream().map(HeaderNodeBk.class::cast).collect(toList());
                        try {
                            return new TableNodeBk(headers, getRowNodes(procedure, headers));
                        } catch (IndexOutOfBoundsException ignore) {
                            throw procedure.getSourceCode().syntaxError("Different cell size", 0);
                        }
                    }
            )).parse(procedure);
        }

        private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> cell(
                Optional<DALOperator> rowOperator, List<HeaderNodeBk> headers) {
            return procedure -> getRowCell(procedure, rowOperator, headers.get(procedure.getIndex()));
        }

        protected List<com.github.leeonky.dal.ast.RowNode> getRowNodes(DALProcedure procedure, List<HeaderNodeBk> headers) {
            return allOptional(() -> {
                Optional<Integer> index = getRowIndex(procedure);
                Optional<Clause<DALRuntimeContext, DALNode>> rowSchemaClause = SCHEMA_CLAUSE.parse(procedure);
                Optional<DALOperator> rowOperator = JUDGEMENT_OPERATORS.parse(procedure);
                COLUMN_SPLITTER.before(cell(rowOperator, headers).sequence(byTableRow(), cells -> {
                    checkCellSize(procedure, headers, cells);
                    return new com.github.leeonky.dal.ast.RowNode(index, rowSchemaClause, rowOperator, cells);
                }));
                return FunctionUtil.oneOf(
                        () -> ELLIPSIS_ROW.parse(procedure).map(cell -> new com.github.leeonky.dal.ast.RowNode(index, rowSchemaClause, rowOperator, singletonList(cell))),
                        () -> WILDCARD_ROW.parse(procedure).map(cell -> new com.github.leeonky.dal.ast.RowNode(index, rowSchemaClause, rowOperator, singletonList(cell))),
                        () -> COLUMN_SPLITTER.before(cell(rowOperator, headers).sequence(byTableRow(), cells -> new com.github.leeonky.dal.ast.RowNode(index,
                                rowSchemaClause, rowOperator, checkCellSize(procedure, headers, cells)))).parse(procedure)
                                .map(com.github.leeonky.dal.ast.RowNode.class::cast));
            });
        }

        private List<DALNode> checkCellSize(DALProcedure procedure, List<HeaderNodeBk> headers, List<DALNode> cellClauses) {
            if (cellClauses.size() != headers.size())
                throw procedure.getSourceCode().syntaxError("Different cell size", 0);
            return cellClauses;
        }
    }

    private DALNode getRowCell(DALProcedure dalProcedure, Optional<DALOperator> rowOperator, HeaderNodeBk headerNode) {
        int cellPosition = dalProcedure.getSourceCode().nextPosition();
        return oneOf(ELEMENT_ELLIPSIS, EMPTY_CELL).or(ROW_WILDCARD.or(
                shortJudgementClause(oneOf(JUDGEMENT_OPERATORS, headerNode.headerOperator(), procedure -> rowOperator)
                        .or(DEFAULT_JUDGEMENT_OPERATOR)).input(headerNode.getProperty()))).parse(dalProcedure)
                .setPositionBegin(cellPosition);
    }

    public class TransposedTable implements NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> {
        private TransposedTableParser transposedTableParser = new TransposedTableParser();

        @Override
        public Optional<DALNode> parse(DALProcedure procedure) {
            if (NewTransposed)
                return transposedTableParser.parse(procedure);
            return procedure.getSourceCode().popWord(TRANSPOSE_MARK).map(x -> {
                List<HeaderNodeBk> headerNodes = new ArrayList<>();
                return new TableNodeBk(headerNodes, getRowNodes(procedure, headerNodes), TableNodeBk.Type.TRANSPOSED);
            });
        }

        private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> cell(
                HeaderNodeBk headerNode) {
            return procedure -> getRowCell(procedure, empty(), headerNode);
        }

        private List<com.github.leeonky.dal.ast.RowNode> getRowNodes(DALProcedure procedure, List<HeaderNodeBk> headerNodes) {
            return transpose(allOptional(() -> TRANSPOSED_TABLE_HEADER.parse(procedure).map(HeaderNodeBk.class::cast)
                    .map(headerNode -> {
                        headerNodes.add(headerNode);
                        return COLUMN_SPLITTER.before(cell(headerNode).sequence(byTableRow()
                                , CellCollection::new)).parse(procedure).map(n -> ((CellCollection) n).getCells())
//                                TODO need test
                                .orElseThrow(() -> procedure.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream().map(row -> new com.github.leeonky.dal.ast.RowNode(empty(), empty(), empty(), row)).collect(toList());
        }
    }


    private Optional<Integer> getRowIndex(DALProcedure procedure) {
        return INTEGER.parse(procedure).map(node -> (Integer) ((ConstNode) node).getValue());
    }

    public class TransposedTableWithRowOperator implements NodeParser<DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALProcedure> {

        private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> indexCell(
                List<Optional<Integer>> rowIndexes, List<Optional<Clause<DALRuntimeContext, DALNode>>> rowSchemaClauses,
                List<Optional<DALOperator>> rowOperators) {
            return procedure -> {
                rowIndexes.add(getRowIndex(procedure));
                rowSchemaClauses.add(SCHEMA_CLAUSE.parse(procedure));
                rowOperators.add(JUDGEMENT_OPERATORS.parse(procedure));
                return new EmptyCellNode();
            };
        }

        @Override
        public Optional<DALNode> parse(DALProcedure procedure) {
            return procedure.getSourceCode().tryFetch(() -> when(procedure.getSourceCode().popWord(COLUMN_SPLITTER).isPresent()
                    && procedure.getSourceCode().popWord(TRANSPOSE_MARK).isPresent()).optional(() -> {
                List<Optional<Integer>> rowIndexes = new ArrayList<>();
                List<Optional<Clause<DALRuntimeContext, DALNode>>> rowSchemaClauses = new ArrayList<>();
                List<Optional<DALOperator>> rowOperators = new ArrayList<>();
                COLUMN_SPLITTER.before(indexCell(rowIndexes, rowSchemaClauses, rowOperators).sequence(byTableRow(), CellCollection::new)).parse(procedure);
                List<HeaderNodeBk> headerNodes = new ArrayList<>();
                return new TableNodeBk(headerNodes, getRowNodes(procedure, headerNodes, rowSchemaClauses, rowOperators,
                        rowIndexes), TableNodeBk.Type.TRANSPOSED);
            }));
        }

        private List<com.github.leeonky.dal.ast.RowNode> getRowNodes(DALProcedure dalProcedure, List<HeaderNodeBk> headerNodes,
                                                                     List<Optional<Clause<DALRuntimeContext, DALNode>>> rowSchemaClauses,
                                                                     List<Optional<DALOperator>> rowOperators, List<Optional<Integer>> rowIndexes) {
            return FunctionUtil.mapWithIndex(getCells(dalProcedure, headerNodes, rowOperators), (i, row) ->
                    new com.github.leeonky.dal.ast.RowNode(rowIndexes.get(i), rowSchemaClauses.get(i), rowOperators.get(i), row)).collect(toList());
        }

        private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> cell(
                List<Optional<DALOperator>> rowOperators, HeaderNodeBk headerNode) {
            return procedure -> getRowCell(procedure, rowOperators.get(procedure.getIndex()), headerNode);
        }

        private Stream<List<DALNode>> getCells(DALProcedure dalProcedure, List<HeaderNodeBk> headerNodes,
                                               List<Optional<DALOperator>> rowOperators) {
            return transpose(allOptional(() -> TRANSPOSED_TABLE_HEADER.parse(dalProcedure).map(HeaderNodeBk.class::cast)
                    .map(headerNode -> {
                        headerNodes.add(headerNode);
                        return COLUMN_SPLITTER.before(cell(rowOperators, headerNode).sequence(byTableRow()
                                , CellCollection::new)).parse(dalProcedure).map(n -> ((CellCollection) n).getCells())
//                                TODO need test
                                .orElseThrow(() -> dalProcedure.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream();
        }
    }

    private Optional<DALNode> compileUserDefinedLiteral(DALProcedure dalProcedure) {
        return dalProcedure.getSourceCode().tryFetch(() -> Tokens.SYMBOL.scan(dalProcedure.getSourceCode())
                .flatMap(token -> dalProcedure.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }

    public class TransposedTableParser implements NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> {

        private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> TABLE_HEADER = procedure -> {
            DALNode property = PROPERTY_CHAIN.parse(procedure);
            return new HeaderNode(property, empty());
        };

        @Override
        public Optional<DALNode> parse(DALProcedure procedure) {
            return procedure.getSourceCode().popWord(TRANSPOSE_MARK).map(x -> {
                Optional<DALNode> parse = COLUMN_SPLITTER.before(TABLE_HEADER).closeBy(endWith(COLUMN_SPLITTER)).parse(procedure);

                return new TransposedTableNode(new ArrayList<DALNode>() {{
                    add(new TransposedRowNode(parse.get()));
                }});
            });
        }
    }

    public class TableParser implements NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> {

        private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
                TABLE_HEADER = procedure -> {
            DALNode property = PROPERTY_CHAIN.parse(procedure);
            Optional<DALOperator> operator = JUDGEMENT_OPERATORS.parse(procedure);

            return new HeaderNode(property, operator);
        };

        private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> cell(
                List<DALNode> headers, Optional<DALOperator> rowOperator) {
            return procedure -> {
                HeaderNode headerNode = (HeaderNode) headers.get(procedure.getIndex());
                return shortJudgementClause(oneOf(JUDGEMENT_OPERATORS, headerNode.headerOperator(), p -> rowOperator)
                        .or(DEFAULT_JUDGEMENT_OPERATOR)).input(headerNode.getProperty()).parse(procedure);
            };
        }


        @Override
        public Optional<DALNode> parse(DALProcedure procedure) {
            return COLUMN_SPLITTER.before(TABLE_HEADER.sequence(byTableRow(), headers -> new TableNode(headers,
                    allOptional(() -> {
                        Optional<DALOperator> rowOperator = JUDGEMENT_OPERATORS.parse(procedure);
                        return COLUMN_SPLITTER.before(cell(headers, rowOperator).sequence(byTableRow(), cells ->
                                new RowNode(rowOperator, cells))).parse(procedure);
                    })))).parse(procedure);
        }
    }

    private static Sequence<Procedure<?, ?, ?, ?, ?>> byTableRow() {
        return severalTimes().mandatoryTailSplitBy(COLUMN_SPLITTER).endWithLine();
    }
}
