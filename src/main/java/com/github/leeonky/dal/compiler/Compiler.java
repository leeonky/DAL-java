package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.DALNode.constNode;
import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.dal.compiler.DALProcedure.enableCommaAnd;
import static com.github.leeonky.dal.compiler.Notations.*;
import static com.github.leeonky.interpreter.ClauseParser.oneOf;
import static com.github.leeonky.interpreter.FunctionUtil.*;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.NodeParser.lazy;
import static com.github.leeonky.interpreter.NodeParser.oneOf;
import static com.github.leeonky.interpreter.OperatorParser.oneOf;
import static com.github.leeonky.interpreter.Parser.endWith;
import static com.github.leeonky.interpreter.Sequence.atLeast;
import static com.github.leeonky.interpreter.Sequence.severalTimes;
import static java.util.Collections.emptyList;
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
            SINGLE_QUOTED_STRING = SINGLE_QUOTED.next(charNode(SINGLE_QUOTED_ESCAPES).sequence(severalTimes().endWith(
                    SINGLE_QUOTED.getLabel()), DALNode::constString)),
            DOUBLE_QUOTED_STRING = DOUBLE_QUOTED.next(charNode(DOUBLE_QUOTED_ESCAPES).sequence(severalTimes().endWith(
                    DOUBLE_QUOTED.getLabel()), DALNode::constString)),
            CONST_TRUE = Keywords.TRUE.nodeParser(DALNode::constTrue),
            CONST_FALSE = Keywords.FALSE.nodeParser(DALNode::constFalse),
            CONST_NULL = Keywords.NULL.nodeParser(DALNode::constNull),
            CONST_USER_DEFINED_LITERAL = this::compileUserDefinedLiteral,
            REGEX = REGEX_NOTATION.next(charNode(REGEX_ESCAPES).sequence(severalTimes().endWith(REGEX_NOTATION),
                    DALNode::regex)),
            IMPLICIT_PROPERTY = PROPERTY_IMPLICIT.clause(Tokens.SYMBOL.nodeParser(DALNode::symbolNode))
                    .defaultInputNode(InputNode.INSTANCE),
            WILDCARD = Notations.Operators.WILDCARD.nodeParser(DALNode::wildcardNode),
            ROW_WILDCARD = Notations.Operators.ROW_WILDCARD.nodeParser(DALNode::wildcardNode),
            CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
                    CONST_USER_DEFINED_LITERAL),
            ELEMENT_ELLIPSIS = Operators.ELEMENT_ELLIPSIS.nodeParser(token -> new ListEllipsisNode()),
            EMPTY_CELL = procedure -> when(procedure.emptyCell()).optional(EmptyCellNode::new),
            TABLE = oneOf(new TransposedTableWithRowOperator(), new TableParser(), new TransposedTable()),
            SCHEMA = Tokens.SCHEMA.nodeParser(DALNode::schema),
            INTEGER_OR_STRING = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING);

    public NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SCHEMA_COMPOSE = OPENING_BRACKET.next(SCHEMA.mandatory("expect a schema").sequence(atLeast(1,
            "expect at least one schema").splitBy(SCHEMA_AND).endWith(CLOSING_BRACKET), DALNode::elementSchemas))
            .or(SCHEMA.mandatory("expect a schema").sequence(severalTimes().splitBy(SCHEMA_AND), DALNode::schemas)),
            PROPERTY_CHAIN, OPERAND, EXPRESSION, SHORT_JUDGEMENT_OPERAND;

    public NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SYMBOL = Tokens.SYMBOL.nodeParser(DALNode::symbolNode);

    public ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            ARITHMETIC_CLAUSE, JUDGEMENT_CLAUSE, BINARY_OPERATOR_EXPRESSION,
            SCHEMA_CLAUSE = IS.clause(SCHEMA_COMPOSE),
            WHICH_CLAUSE = ClauseParser.lazy(() -> WHICH.clause(EXPRESSION)),

    EXPLICIT_PROPERTY = oneOf(PROPERTY_DOT.clause(Tokens.DOT_SYMBOL.nodeParser(DALNode::symbolNode).mandatory(
            "expect a symbol")), PROPERTY_IMPLICIT.clause(OPENING_BRACKET.next(INTEGER_OR_STRING.mandatory(
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
        PARENTHESES = lazy(() -> enableCommaAnd(OPENING_PARENTHESES.next(
                EXPRESSION.closeBy(endWith(CLOSING_PARENTHESES)).map(DALNode::parenthesesNode))));
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(InputNode.INSTANCE), IMPLICIT_PROPERTY);
        PROPERTY_CHAIN = PROPERTY.mandatory("expect a object property").recursive(EXPLICIT_PROPERTY);
        OBJECT = DALProcedure.disableCommaAnd(OPENING_BRACES.next(PROPERTY_CHAIN.expression(shortJudgementClause(
                JUDGEMENT_OPERATORS.mandatory("expect operator `:` or `=`"))).sequence(severalTimes().
                optionalSplitBy(Notations.COMMA).endWith(CLOSING_BRACES), ObjectScopeNode::new)));
        LIST = DALProcedure.disableCommaAnd(OPENING_BRACKET.next(ELEMENT_ELLIPSIS.ignoreInput().or(shortJudgementClause(
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

//        TODO remove
        BINARY_OPERATOR_EXPRESSION = oneOf(ARITHMETIC_CLAUSE, JUDGEMENT_CLAUSE, EXPLICIT_PROPERTY,
                WHICH_CLAUSE, SCHEMA_CLAUSE);
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

    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SEQUENCE = procedure -> FunctionUtil.oneOf(
            procedure.sequenceOf(SEQUENCE_AZ, SequenceNode.Type.AZ),
            procedure.sequenceOf(SEQUENCE_ZA, SequenceNode.Type.ZA),
            procedure.sequenceOf(SEQUENCE_AZ_2, SequenceNode.Type.AZ),
            procedure.sequenceOf(SEQUENCE_ZA_2, SequenceNode.Type.ZA))
            .orElse(SequenceNode.noSequence());

    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> TABLE_HEADER = procedure -> {
        SequenceNode sequence = (SequenceNode) SEQUENCE.parse(procedure);
        DALNode property = PROPERTY_CHAIN.parse(procedure);
        return new HeaderNode(sequence, SCHEMA_CLAUSE.parse(procedure)
                .map(expressionClause -> expressionClause.makeExpression(property)).orElse(property),
                JUDGEMENT_OPERATORS.parse(procedure));
    };

    public class TableParser implements NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> {
        @Override
        public Optional<DALNode> parse(DALProcedure procedure) {
            try {
                return procedure.fetchRow(columnIndex -> (HeaderNode) TABLE_HEADER.parse(procedure))
                        .map(headers -> new TableNode(headers, getRowNodes(procedure, headers)));
            } catch (IndexOutOfBoundsException ignore) {
                throw procedure.getSourceCode().syntaxError("Different cell size", 0);
            }
        }

        protected List<RowNode> getRowNodes(DALProcedure procedure, List<HeaderNode> headers) {
            return allOptional(() -> {
                Optional<Integer> index = getRowIndex(procedure);
                Optional<Clause<DALRuntimeContext, DALNode>> rowSchemaClause = SCHEMA_CLAUSE.parse(procedure);
                Optional<DALOperator> rowOperator = JUDGEMENT_OPERATORS.parse(procedure);
                return FunctionUtil.oneOf(
                        () -> procedure.fetchNodeBetween("|", "|", ELEMENT_ELLIPSIS).map(Collections::singletonList),
                        () -> procedure.fetchNodeBetween("|", "|", ROW_WILDCARD).map(Collections::singletonList),
                        () -> procedure.fetchRow(column -> getRowCell(procedure, rowOperator, headers.get(column)))
                                .map(cellClauses -> checkCellSize(procedure, headers, cellClauses))
                ).map(nodes -> new RowNode(index, rowSchemaClause, rowOperator, nodes));
            });
        }

        private List<DALNode> checkCellSize(DALProcedure procedure, List<HeaderNode> headers, List<DALNode> cellClauses) {
            if (cellClauses.size() != headers.size())
                throw procedure.getSourceCode().syntaxError("Different cell size", 0);
            return cellClauses;
        }
    }

    private DALNode getRowCell(DALProcedure dalProcedure, Optional<DALOperator> rowOperator, HeaderNode headerNode) {
        int cellPosition = dalProcedure.getSourceCode().nextPosition();
        return oneOf(ELEMENT_ELLIPSIS, EMPTY_CELL).or(ROW_WILDCARD.or(
                shortJudgementClause(oneOf(JUDGEMENT_OPERATORS, headerNode.headerOperator(), procedure -> rowOperator)
                        .or(DEFAULT_JUDGEMENT_OPERATOR)).input(headerNode.getProperty()))).parse(dalProcedure)
                .setPositionBegin(cellPosition);
    }

    public class TransposedTable implements NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> {
        @Override
        public Optional<DALNode> parse(DALProcedure procedure) {
            return procedure.getSourceCode().popWord(">>").map(x -> {
                List<HeaderNode> headerNodes = new ArrayList<>();
                return new TableNode(headerNodes, getRowNodes(procedure, headerNodes), TableNode.Type.TRANSPOSED);
            });
        }

        private List<RowNode> getRowNodes(DALProcedure procedure, List<HeaderNode> headerNodes) {
            return transpose(allOptional(() -> procedure.fetchNodeAfter("|", TABLE_HEADER)
                    .map(HeaderNode.class::cast).map(headerNode -> {
                        headerNodes.add(headerNode);
                        return procedure.fetchRow(row -> getRowCell(procedure, empty(), headerNode))
                                .orElseThrow(() -> procedure.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream().map(row -> new RowNode(empty(), empty(), empty(), row)).collect(toList());
        }
    }

    private Optional<Integer> getRowIndex(DALProcedure procedure) {
        return INTEGER.parse(procedure).map(node -> (Integer) ((ConstNode) node).getValue());
    }

    public class TransposedTableWithRowOperator implements NodeParser<DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALProcedure> {

        @Override
        public Optional<DALNode> parse(DALProcedure procedure) {
            return procedure.getSourceCode().tryFetch(() -> when(procedure.getSourceCode().popWord("|").isPresent()
                    && procedure.getSourceCode().popWord(">>").isPresent()).optional(() -> {
                List<Optional<Integer>> rowIndexes = new ArrayList<>();
                List<Optional<Clause<DALRuntimeContext, DALNode>>> rowSchemaClauses = new ArrayList<>();
                List<Optional<DALOperator>> rowOperators = procedure.fetchRow(row -> {
                    rowIndexes.add(getRowIndex(procedure));
                    rowSchemaClauses.add(SCHEMA_CLAUSE.parse(procedure));
                    return JUDGEMENT_OPERATORS.parse(procedure);
                }).orElse(emptyList());
                List<HeaderNode> headerNodes = new ArrayList<>();
                return new TableNode(headerNodes, getRowNodes(procedure, headerNodes, rowSchemaClauses, rowOperators,
                        rowIndexes), TableNode.Type.TRANSPOSED);
            }));
        }

        private List<RowNode> getRowNodes(DALProcedure dalProcedure, List<HeaderNode> headerNodes,
                                          List<Optional<Clause<DALRuntimeContext, DALNode>>> rowSchemaClauses,
                                          List<Optional<DALOperator>> rowOperators, List<Optional<Integer>> rowIndexes) {
            return FunctionUtil.mapWithIndex(getCells(dalProcedure, headerNodes, rowOperators), (i, row) ->
                    new RowNode(rowIndexes.get(i), rowSchemaClauses.get(i), rowOperators.get(i), row)).collect(toList());
        }

        private Stream<List<DALNode>> getCells(DALProcedure dalProcedure, List<HeaderNode> headerNodes,
                                               List<Optional<DALOperator>> rowOperators) {
            return transpose(allOptional(() -> dalProcedure.fetchNodeAfter("|", TABLE_HEADER).map(HeaderNode.class::cast)
                    .map(headerNode -> {
                        headerNodes.add(headerNode);
                        return dalProcedure.fetchRow(row -> getRowCell(dalProcedure, rowOperators.get(row), headerNode))
                                .orElseThrow(() -> dalProcedure.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream();
        }
    }

    private Optional<DALNode> compileUserDefinedLiteral(DALProcedure dalProcedure) {
        return dalProcedure.getSourceCode().tryFetch(() -> Tokens.SYMBOL.scan(dalProcedure.getSourceCode())
                .flatMap(token -> dalProcedure.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }
}
