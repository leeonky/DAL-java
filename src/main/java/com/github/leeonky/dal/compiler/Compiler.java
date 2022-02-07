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

import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.dal.compiler.DALProcedure.enableCommaAnd;
import static com.github.leeonky.dal.compiler.Notations.Operators;
import static com.github.leeonky.interpreter.ClauseParser.Mandatory.after;
import static com.github.leeonky.interpreter.ClauseParser.oneOf;
import static com.github.leeonky.interpreter.ComplexNode.multiple;
import static com.github.leeonky.interpreter.ComplexNode.single;
import static com.github.leeonky.interpreter.FunctionUtil.*;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.NodeParser.lazy;
import static com.github.leeonky.interpreter.NodeParser.oneOf;
import static com.github.leeonky.interpreter.OperatorParser.oneOf;
import static java.util.Collections.emptyList;
import static java.util.Optional.empty;
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

    public static final OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALProcedure>
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
            INPUT = procedure -> when(procedure.isCodeBeginning()).optional(() -> InputNode.INSTANCE),
            NUMBER = Tokens.NUMBER.nodeParser(token -> new ConstNode(token.getNumber())),
            INTEGER = Tokens.INTEGER.nodeParser(token -> new ConstNode(token.getInteger())),
            SINGLE_QUOTED_STRING = procedure -> procedure.fetchString('\'', '\'', ConstNode::new, SINGLE_QUOTED_ESCAPES),
            DOUBLE_QUOTED_STRING = procedure -> procedure.fetchString('"', '"', ConstNode::new, DOUBLE_QUOTED_ESCAPES),
            CONST_TRUE = Keywords.TRUE.nodeMatcher(token -> new ConstNode(true)),
            CONST_FALSE = Keywords.FALSE.nodeMatcher(token -> new ConstNode(false)),
            CONST_NULL = Keywords.NULL.nodeMatcher(token -> new ConstNode(null)),
            CONST_USER_DEFINED_LITERAL = this::compileUserDefinedLiteral,
            REGEX = procedure -> procedure.fetchString('/', '/', RegexNode::new, REGEX_ESCAPES),
            IMPLICIT_PROPERTY,
            WILDCARD = Notations.Operators.WILDCARD.nodeMatcher(token -> new WildcardNode(token.getContent())),
            ROW_WILDCARD = Notations.Operators.ROW_WILDCARD.nodeMatcher(token -> new WildcardNode(token.getContent())),
            PROPERTY, OBJECT, LIST, CONST, PARENTHESES, JUDGEMENT, SCHEMA_JUDGEMENT_CLAUSE,
            ELEMENT_ELLIPSIS, UNARY_OPERATOR_EXPRESSION,
            EMPTY_CELL = procedure -> when(procedure.getSourceCode().startsWith("|")).optional(EmptyCellNode::new),
            TABLE = oneOf(new TransposedTableWithRowOperator(), new TableParser(), new TransposedTable()),
            SCHEMA = Tokens.SCHEMA.nodeParser(DALNode::schema);

    public NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            LIST_INDEX_OR_MAP_KEY = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING)
            .mandatory("should given one property or array index in `[]`"),
            PROPERTY_CHAIN, OPERAND, EXPRESSION, ARITHMETIC_EXPRESSION, JUDGEMENT_EXPRESSION_OPERAND,
            SCHEMA_COMPOSE = multiple(SCHEMA.mandatory("expect a schema")).between('[', ']').splitBy("/").nodeParser(DALNode::elementSchemas)
                    .or(multiple(SCHEMA.mandatory("expect a schema")).splitBy("/").mandatory(DALNode::schemas));

    ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            EXPLICIT_PROPERTY,
            BINARY_ARITHMETIC_EXPRESSION,
            BINARY_JUDGEMENT_EXPRESSION,
            BINARY_OPERATOR_EXPRESSION,
            SCHEMA_EXPRESSION;

    public NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SYMBOL = Tokens.IDENTITY_PROPERTY.nodeParser(DALNode::symbolNode),
            BRACKET_SYMBOL = single(LIST_INDEX_OR_MAP_KEY, "should given one property or array index in `[]`")
                    .between('[', ']').nodeParser(DALNode::bracketSymbolNode);

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALProcedure> judgementClause(OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALProcedure> operatorMandatory) {
        return after(Notations.IS_s, SCHEMA_CLAUSE).concat(JUDGEMENT_OPERATORS.clause(JUDGEMENT_EXPRESSION_OPERAND))
                .or(operatorMandatory.mandatoryClause(JUDGEMENT_EXPRESSION_OPERAND));
    }

    private static final ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SCHEMA_CLAUSE = new SchemaClauseMandatory();

    private ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> implicitProperty(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeParser) {
        return procedure -> nodeParser.parse(procedure).map(node -> previous -> new DALExpression(previous,
                new DALOperator.PropertyImplicit().setPosition(node.getOperandPosition()), node));
    }

    public Compiler() {
        EXPLICIT_PROPERTY = oneOf(PROPERTY_DOT.clause(SYMBOL.mandatory("expect a symbol")),
                implicitProperty(BRACKET_SYMBOL));
        IMPLICIT_PROPERTY = implicitProperty(SYMBOL).defaultInputNode(InputNode.INSTANCE);
        CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
                CONST_USER_DEFINED_LITERAL);
        PARENTHESES = lazy(() -> enableCommaAnd(single(EXPRESSION, "expect a value or expression").between('(', ')')
                .nodeParser(DALNode::parenthesesNode)));
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(InputNode.INSTANCE), IMPLICIT_PROPERTY);
        PROPERTY_CHAIN = procedure -> PROPERTY.mandatory("expect a object property").recursive(EXPLICIT_PROPERTY).parse(procedure);
        OBJECT = lazy(() -> DALProcedure.disableCommaAnd(multiple(PROPERTY_CHAIN.mandatoryNode(
                judgementClause(JUDGEMENT_OPERATORS.or("expect operator `:` or `=`"))))
                .between('{', '}').nodeParser(ObjectNode::new)));
        ELEMENT_ELLIPSIS = Notations.Operators.ELEMENT_ELLIPSIS.nodeMatcher(token -> new ListEllipsisNode());
        LIST = lazy(() -> DALProcedure.disableCommaAnd(multiple(ELEMENT_ELLIPSIS.castToClause().or(
                judgementClause(JUDGEMENT_OPERATORS.or(DEFAULT_JUDGEMENT_OPERATOR))))
                .between('[', ']').nodeParser(ListNode::new)));

        JUDGEMENT = oneOf(REGEX, OBJECT, LIST, WILDCARD, TABLE);
        UNARY_OPERATOR_EXPRESSION = procedure -> procedure.fetchExpression(null, UNARY_OPERATORS, OPERAND);
        OPERAND = UNARY_OPERATOR_EXPRESSION.or(oneOf(CONST, PROPERTY, PARENTHESES, INPUT)
                .mandatory("expect a value or expression").map(DALNode::avoidListMapping));
        BINARY_ARITHMETIC_EXPRESSION = BINARY_ARITHMETIC_OPERATORS.clause(OPERAND);
        BINARY_JUDGEMENT_EXPRESSION = JUDGEMENT_OPERATORS.clause(JUDGEMENT.or(OPERAND));
        BINARY_OPERATOR_EXPRESSION = oneOf(BINARY_ARITHMETIC_EXPRESSION, BINARY_JUDGEMENT_EXPRESSION, EXPLICIT_PROPERTY);
        ARITHMETIC_EXPRESSION = OPERAND.recursive(oneOf(BINARY_ARITHMETIC_EXPRESSION, /*need test*/EXPLICIT_PROPERTY));
        JUDGEMENT_EXPRESSION_OPERAND = JUDGEMENT.or(ARITHMETIC_EXPRESSION);
        SCHEMA_JUDGEMENT_CLAUSE = procedure -> procedure.fetchExpression(
                InputNode.INSTANCE, JUDGEMENT_OPERATORS, JUDGEMENT_EXPRESSION_OPERAND);
        SCHEMA_EXPRESSION = procedure -> after(Notations.IS_s, SCHEMA_CLAUSE).concat(omitWhich(SCHEMA_JUDGEMENT_CLAUSE),
                after(Notations.WHICH_S, which(SCHEMA_JUDGEMENT_CLAUSE.or(EXPRESSION)))).parse(procedure);
        EXPRESSION = OPERAND.recursive(oneOf(BINARY_OPERATOR_EXPRESSION, SCHEMA_EXPRESSION));
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> which(
            NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeFactory) {
        return procedure -> previous -> ((SchemaExpression) previous).which(nodeFactory.parse(procedure));
    }

    private ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> omitWhich(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeParser) {
        return procedure -> nodeParser.parse(procedure).map(node -> previous ->
                ((SchemaExpression) previous).omitWhich(node));
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
        return new HeaderNode(sequence, procedure.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE)
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
                Optional<Clause<DALRuntimeContext, DALNode>> rowSchemaClause = procedure.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE);
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
                judgementClause(oneOf(JUDGEMENT_OPERATORS, headerNode.headerOperator(), procedure -> rowOperator)
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
            return transpose(allOptional(() -> procedure.fetchNodeAfter2("|", TABLE_HEADER)
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
                    rowSchemaClauses.add(procedure.fetchClauseAfter(Notations.IS_s, SCHEMA_CLAUSE));
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
            return transpose(allOptional(() -> dalProcedure.fetchNodeAfter2("|", TABLE_HEADER).map(HeaderNode.class::cast)
                    .map(headerNode -> {
                        headerNodes.add(headerNode);
                        return dalProcedure.fetchRow(row -> getRowCell(dalProcedure, rowOperators.get(row), headerNode))
                                .orElseThrow(() -> dalProcedure.getSourceCode().syntaxError("should end with `|`", 0));
                    }))).stream();
        }
    }

    private Optional<DALNode> compileUserDefinedLiteral(DALProcedure dalProcedure) {
        return dalProcedure.getSourceCode().tryFetch(() -> Tokens.IDENTITY_PROPERTY.scan(dalProcedure.getSourceCode())
                .flatMap(token -> dalProcedure.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }
}
