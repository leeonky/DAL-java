package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.ast.table.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import static com.github.leeonky.dal.ast.DALNode.constNode;
import static com.github.leeonky.dal.compiler.Constants.PROPERTY_DELIMITER_STRING;
import static com.github.leeonky.dal.compiler.DALProcedure.*;
import static com.github.leeonky.dal.compiler.Notations.*;
import static com.github.leeonky.interpreter.ClauseParser.Mandatory.clause;
import static com.github.leeonky.interpreter.FunctionUtil.not;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.NodeParser.positionNode;
import static com.github.leeonky.interpreter.Notation.notation;
import static com.github.leeonky.interpreter.Parser.*;
import static com.github.leeonky.interpreter.Syntax.Rules.*;
import static com.github.leeonky.interpreter.Syntax.many;
import static com.github.leeonky.interpreter.Syntax.single;
import static java.util.Optional.empty;
import static java.util.Optional.of;

public class Compiler {

    private static final OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            DEFAULT_OPERATOR = Procedure::currentOperator,
            IS = Operators.IS.operator(DALOperator.Is::new),
            WHICH = Operators.WHICH.operator(DALOperator.Which::new),
            PROPERTY_DOT = Operators.DOT.operator(DALOperator.PropertyDot::new,
                    not(DALProcedure::mayBeElementEllipsis)),
            MAYBE_PROPERTY_SLASH = Operators.SLASH.operator(DALOperator.PropertySlash::new),
            PROPERTY_SLASH = procedure -> procedure.isEnableSlashProperty() ? MAYBE_PROPERTY_SLASH.parse(procedure) : empty(),
            PROPERTY_IMPLICIT = procedure -> of(new DALOperator.PropertyImplicit()),
            BINARY_ARITHMETIC_OPERATORS = oneOf(
                    Operators.AND.operator(DALOperator::operatorAnd),
                    Operators.OR.operator(DALOperator::operatorOr),
                    Keywords.AND.keywordOperator(DALOperator::keywordAnd, PROPERTY_DELIMITER_STRING),
                    Operators.COMMA.operator(DALOperator::commaAnd, DALProcedure::isEnableCommaAnd),
                    Operators.NOT_EQUAL.operator(DALOperator.NotEqual::new),
                    Keywords.OR.keywordOperator(DALOperator::keywordOr, PROPERTY_DELIMITER_STRING),
                    Operators.GREATER_OR_EQUAL.operator(DALOperator.GreaterOrEqual::new),
                    Operators.LESS_OR_EQUAL.operator(DALOperator.LessOrEqual::new),
                    Operators.GREATER.operator(DALOperator.Greater::new),
                    Operators.LESS.operator(DALOperator.Less::new, not(DALProcedure::mayBeOpeningGroup)),
                    Operators.PLUS.operator(DALOperator.Plus::new),
                    Operators.SUBTRACTION.operator(DALOperator.Subtraction::new),
                    Operators.MULTIPLICATION.operator(DALOperator.Multiplication::new),
                    Operators.DIVISION.operator(DALOperator.Division::new)),
            UNARY_OPERATORS = oneOf(
                    Operators.MINUS.operator(DALOperator.Minus::new, not(DALProcedure::isCodeBeginning)),
                    Operators.PLUS.operator(DALOperator.Positive::new, not(DALProcedure::isCodeBeginning)),
                    Operators.NOT.operator(DALOperator.Not::new, not(DALProcedure::mayBeUnEqual))),
            VERIFICATION_OPERATORS = oneOf(
                    Operators.MATCHER.<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
                            operator(DALOperator.Matcher::new),
                    Operators.EQUAL.operator(DALOperator.Equal::new));

    private static final OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            DEFAULT_VERIFICATION_OPERATOR = DEFAULT_OPERATOR.mandatory("");

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

    //    TODO private
    NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            PROPERTY, OBJECT, SORTED_LIST, LIST, PARENTHESES, VERIFICATION_SPECIAL_OPERAND, VERIFICATION_VALUE_OPERAND,
            TABLE, SHORT_VERIFICATION_OPERAND, CELL_VERIFICATION_OPERAND, GROUP_PROPERTY, OPTIONAL_PROPERTY_CHAIN,
            INPUT = procedure -> when(procedure.isCodeBeginning()).optional(() -> InputNode.INSTANCE),
            NUMBER = Tokens.NUMBER.nodeParser(constNode(Token::getNumber)),
            INTEGER = Tokens.INTEGER.nodeParser(constNode(Token::getInteger)),
            SINGLE_QUOTED_STRING = SINGLE_QUOTED.with(many(charNode(SINGLE_QUOTED_ESCAPES))
                    .and(endWith(SINGLE_QUOTED.getLabel())).as(DALNode::constString)),
            DOUBLE_QUOTED_STRING = DOUBLE_QUOTED.with(many(charNode(DOUBLE_QUOTED_ESCAPES))
                    .and(endWith(DOUBLE_QUOTED.getLabel())).as(DALNode::constString)),
            CONST_TRUE = Keywords.TRUE.wordNode(DALNode::constTrue, PROPERTY_DELIMITER_STRING),
            CONST_FALSE = Keywords.FALSE.wordNode(DALNode::constFalse, PROPERTY_DELIMITER_STRING),
            CONST_NULL = Keywords.NULL.wordNode(DALNode::constNull, PROPERTY_DELIMITER_STRING),
            CONST_USER_DEFINED_LITERAL = this::compileUserDefinedLiteral,
            REGEX = OPEN_REGEX.with(many(charNode(REGEX_ESCAPES)).and(endWith(CLOSE_REGEX.getLabel())).as(DALNode::regex)),
            WILDCARD = Notations.Operators.WILDCARD.node(WildcardNode::new),
            ROW_WILDCARD = Notations.Operators.ROW_WILDCARD.node(WildcardNode::new),
            CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
                    CONST_USER_DEFINED_LITERAL),
            ELEMENT_ELLIPSIS = Operators.ELEMENT_ELLIPSIS.node(token -> new ListEllipsisNode()),
            SCHEMA = Tokens.SCHEMA.nodeParser(DALNode::schema),
            INTEGER_OR_STRING = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING),
            STRING_PROPERTY = procedure -> procedure.isEnableRelaxProperty() ? single(oneOf(SINGLE_QUOTED_STRING,
                    DOUBLE_QUOTED_STRING)).as(DALNode::stringSymbol).parse(procedure) : empty(),
            SYMBOL = procedure -> (procedure.isEnableRelaxProperty() ? Tokens.RELAX_SYMBOL : Tokens.SYMBOL).nodeParser(
                    DALNode::symbolNode).parse(procedure),
            DOT_SYMBOL = procedure -> (procedure.isEnableRelaxProperty() ? Tokens.RELAX_DOT_SYMBOL : Tokens.DOT_SYMBOL)
                    .nodeParser(DALNode::symbolNode).parse(procedure),
            PROPERTY_PATTERN = this::propertyPattern,
            OPTIONAL_VERIFICATION_PROPERTY = lazyNode(() -> enableSlashProperty(enableRelaxProperty(OPTIONAL_PROPERTY_CHAIN)));

    private Optional<DALNode> propertyPattern(DALProcedure dalProcedure) {
        ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> patternClause =
                notation("{}").clause((token, symbol) -> new PropertyPattern(symbol));
        return oneOf(notation("{}").node(n -> new PropertyThis()), SYMBOL.with(patternClause)).parse(dalProcedure);
    }

    public NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            PROPERTY_CHAIN, OPERAND, EXPRESSION, VERIFICATION_PROPERTY,
            DEFAULT_INPUT = procedure -> InputNode.INSTANCE,
            SCHEMA_COMPOSE = OPENING_BRACKET.with(single(many(SCHEMA.mandatory("Expect a schema"))
                            .and(Syntax.Rules.splitBy(SCHEMA_AND)).as(DALNode::elementSchemas)).and(endWith(CLOSING_BRACKET)).as())
                    .or(many(SCHEMA.mandatory("Expect a schema")).and(Syntax.Rules.splitBy(SCHEMA_AND)).as(DALNode::schemas)),
            EXPRESSION_RELAX_STRING = Tokens.EXPRESSION_RELAX_STRING.nodeParser(DALNode::relaxString),
            OBJECT_SCOPE_RELAX_STRING = Tokens.OBJECT_SCOPE_RELAX_STRING.nodeParser(DALNode::relaxString),
            LIST_SCOPE_RELAX_STRING = Tokens.LIST_SCOPE_RELAX_STRING.nodeParser(DALNode::relaxString),
            TABLE_CELL_RELAX_STRING = Tokens.TABLE_CELL_RELAX_STRING.nodeParser(DALNode::relaxString);

    public ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            ARITHMETIC_CLAUSE, VERIFICATION_CLAUSE,
            SCHEMA_CLAUSE = IS.clause(SCHEMA_COMPOSE),
            WHICH_CLAUSE = lazyClause(() -> WHICH.clause(EXPRESSION)),
            ELEMENT_ELLIPSIS_CLAUSE = Operators.ELEMENT_ELLIPSIS.clause((token, input) -> new ListEllipsisNode()),
            LIST_MAPPING_CLAUSE = Notations.LIST_MAPPING.clause((token, symbolNode) -> new ListMappingNode(symbolNode)),
            IMPLICIT_PROPERTY_CLAUSE = PROPERTY_IMPLICIT.clause(oneOf(PROPERTY_PATTERN, oneOf(STRING_PROPERTY, SYMBOL).concat(LIST_MAPPING_CLAUSE))),
            EXPLICIT_PROPERTY_CLAUSE = oneOf(PROPERTY_DOT.clause(PROPERTY_PATTERN.or(oneOf(STRING_PROPERTY, DOT_SYMBOL).concat(LIST_MAPPING_CLAUSE).mandatory(
                    "Expect a symbol"))), PROPERTY_SLASH.clause(oneOf(STRING_PROPERTY, DOT_SYMBOL).concat(LIST_MAPPING_CLAUSE).mandatory(
                    "Expect a symbol")), PROPERTY_IMPLICIT.clause(OPENING_BRACKET.with(single(INTEGER_OR_STRING.mandatory(
                    "Should given one property or array index in `[]`")).and(endWith(CLOSING_BRACKET))
                    .as(DALNode::bracketSymbolNode).concat(LIST_MAPPING_CLAUSE))));

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> shortVerificationClause(
            OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> operatorMandatory,
            NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> operand) {
        return SCHEMA_CLAUSE.concat(VERIFICATION_OPERATORS.clause(operand)).or(operatorMandatory.clause(operand));
    }

    private ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> ARITHMETIC_CLAUSE_CHAIN,
            VERIFICATION_CLAUSE_CHAIN, EXPLICIT_PROPERTY_CHAIN, WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN, EXPRESSION_CLAUSE;

    public Compiler() {
        PARENTHESES = lazyNode(() -> enableCommaAnd(OPENING_PARENTHESES.with(single(EXPRESSION).and(endWith(CLOSING_PARENTHESES))
                .as(DALNode::parenthesesNode))));
        PROPERTY = lazyNode(() -> oneOf(GROUP_PROPERTY, DEFAULT_INPUT.with(oneOf(EXPLICIT_PROPERTY_CLAUSE, IMPLICIT_PROPERTY_CLAUSE))));
        OPTIONAL_PROPERTY_CHAIN = PROPERTY.concatAll(EXPLICIT_PROPERTY_CLAUSE);
        PROPERTY_CHAIN = OPTIONAL_PROPERTY_CHAIN.mandatory("Expect a object property");
        VERIFICATION_PROPERTY = enableRelaxProperty(enableSlashProperty(PROPERTY_CHAIN));
        OBJECT = lazyNode(() -> disableCommaAnd(OPENING_BRACES.with(single(ELEMENT_ELLIPSIS).and(endWith(CLOSING_BRACES))
                .as(ObjectScopeNode::new).or(many(VERIFICATION_PROPERTY.concat(shortVerificationClause(VERIFICATION_OPERATORS
                        .mandatory("Expect operator `:` or `=`"), SHORT_VERIFICATION_OPERAND.or(OBJECT_SCOPE_RELAX_STRING))))
                        .and(optionalSplitBy(COMMA)).and(endWith(CLOSING_BRACES)).as(ObjectScopeNode::new)))));
        SORTED_LIST = oneOf(Operators.PLUS.before(pureList(ListScopeNode.NatureOrder::new)),
                Operators.SUBTRACTION.before(pureList(ListScopeNode.ReverseOrder::new)));
        LIST = oneOf(pureList(ListScopeNode::new), SORTED_LIST);
        TABLE = oneOf(TRANSPOSE_MARK.with(EMPTY_TRANSPOSED_HEAD.with(transposeTable())),
                positionNode(COLUMN_SPLITTER.before(TRANSPOSE_MARK.before(COLUMN_SPLITTER.before(tableLine(ROW_PREFIX)
                        .as(TransposedTableHead::new))))).concat(transposeTable()),
                positionNode(COLUMN_SPLITTER.before(tableLine(TABLE_HEADER).as(TableHeadRow::new))).concat(TABLE_BODY_CLAUSE));
        VERIFICATION_SPECIAL_OPERAND = oneOf(REGEX, OBJECT, LIST, WILDCARD, TABLE);
        OPERAND = lazyNode(() -> oneOf(UNARY_OPERATORS.unary(OPERAND), CONST, PROPERTY, PARENTHESES, INPUT))
                .mandatory("Expect a value or expression");
        VERIFICATION_VALUE_OPERAND = oneOf(UNARY_OPERATORS.unary(OPERAND), CONST, DEFAULT_INPUT.with(EXPLICIT_PROPERTY_CLAUSE), PARENTHESES);
        ARITHMETIC_CLAUSE = BINARY_ARITHMETIC_OPERATORS.clause(OPERAND);
        VERIFICATION_CLAUSE = VERIFICATION_OPERATORS.clause(oneOf(VERIFICATION_SPECIAL_OPERAND,
                VERIFICATION_VALUE_OPERAND).or(EXPRESSION_RELAX_STRING));
        ARITHMETIC_CLAUSE_CHAIN = lazyClause(() -> ARITHMETIC_CLAUSE.concat(EXPRESSION_CLAUSE));
        VERIFICATION_CLAUSE_CHAIN = lazyClause(() -> VERIFICATION_CLAUSE.concat(EXPRESSION_CLAUSE));
        EXPLICIT_PROPERTY_CHAIN = lazyClause(() -> EXPLICIT_PROPERTY_CLAUSE.concat(EXPRESSION_CLAUSE));
        WHICH_CLAUSE_CHAIN = lazyClause(() -> WHICH_CLAUSE.concat(EXPRESSION_CLAUSE));
        SCHEMA_CLAUSE_CHAIN = lazyClause(() -> SCHEMA_CLAUSE.concat(oneOf(VERIFICATION_CLAUSE_CHAIN,
                WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN)));
        EXPRESSION_CLAUSE = oneOf(ARITHMETIC_CLAUSE_CHAIN, VERIFICATION_CLAUSE_CHAIN, EXPLICIT_PROPERTY_CHAIN,
                WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN);
        EXPRESSION = OPERAND.concat(EXPRESSION_CLAUSE);
        SHORT_VERIFICATION_OPERAND = oneOf(VERIFICATION_SPECIAL_OPERAND, VERIFICATION_VALUE_OPERAND.concatAll(oneOf(ARITHMETIC_CLAUSE)));
        CELL_VERIFICATION_OPERAND = single(oneOf(oneOf(REGEX, OBJECT, LIST, WILDCARD), VERIFICATION_VALUE_OPERAND
                .concatAll(oneOf(ARITHMETIC_CLAUSE)))).and(enabledBefore(COLUMN_SPLITTER)).as();
        GROUP_PROPERTY = disableCommaAnd(OPENING_GROUP.with(many(PROPERTY_CHAIN).and(optionalSplitBy(COMMA))
                .and(endWith(CLOSING_GROUP)).as(GroupNode::new)));
    }

    private NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> pureList(
            Function<List<Clause<DALRuntimeContext, DALNode>>, DALNode> factory) {
        return lazyNode(() -> disableCommaAnd(OPENING_BRACKET.with(many(ELEMENT_ELLIPSIS_CLAUSE.or(
                shortVerificationClause(VERIFICATION_OPERATORS.or(DEFAULT_VERIFICATION_OPERATOR),
                        SHORT_VERIFICATION_OPERAND.or(LIST_SCOPE_RELAX_STRING)))).and(optionalSplitBy(COMMA))
                .and(endWith(CLOSING_BRACKET)).as(factory))));
    }

    public List<DALNode> compile(SourceCode sourceCode, DALRuntimeContext DALRuntimeContext) {
        return new ArrayList<DALNode>() {{
            DALProcedure dalParser = new DALProcedure(sourceCode, DALRuntimeContext, DALExpression::new);
            add(EXPRESSION.parse(dalParser));
            if (sourceCode.isBeginning() && sourceCode.hasCode())
                throw sourceCode.syntaxError("Unexpected token", 0);
            while (sourceCode.hasCode())
                add(EXPRESSION.parse(dalParser));
        }};
    }

    private static NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> charNode(
            EscapeChars escapeChars) {
        return procedure -> new ConstNode(procedure.getSourceCode().popChar(escapeChars));
    }

    public List<Object> toChainNodes(String sourceCode) {
        return PROPERTY_CHAIN.parse(new DALProcedure(SourceCode.createSourceCode(sourceCode, LINE_COMMENTS),
                null, DALExpression::new)).propertyChain();
    }

    private final NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SEQUENCE_AZ = Notations.SEQUENCE_AZ.node(SortSymbolNode::new),
            SEQUENCE_ZA = Notations.SEQUENCE_ZA.node(SortSymbolNode::new),
            SEQUENCE_AZ_2 = Notations.SEQUENCE_AZ_2.node(SortSymbolNode::new),
            SEQUENCE_ZA_2 = Notations.SEQUENCE_ZA_2.node(SortSymbolNode::new),
            ROW_KEY = oneOf(INTEGER, OPTIONAL_VERIFICATION_PROPERTY);

    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SEQUENCE = oneOf(
            many(SEQUENCE_AZ).and(atLeast(1)).as(SortGroupNode::new),
            many(SEQUENCE_AZ_2).and(atLeast(1)).as(SortGroupNode::new),
            many(SEQUENCE_ZA).and(atLeast(1)).as(SortGroupNode::new),
            many(SEQUENCE_ZA_2).and(atLeast(1)).as(SortGroupNode::new)).or(procedure -> SortGroupNode.noSequence()),
            EMPTY_TRANSPOSED_HEAD = procedure -> new EmptyTransposedTableHead();

    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            ROW_PREFIX = procedure -> new TableRowPrefixNode(ROW_KEY.parse(procedure), SCHEMA_CLAUSE.parse(procedure),
            VERIFICATION_OPERATORS.parse(procedure)),
            TABLE_HEADER = procedure -> new HeaderNode((SortGroupNode) SEQUENCE.parse(procedure),
                    VERIFICATION_PROPERTY.concat(SCHEMA_CLAUSE).parse(procedure), VERIFICATION_OPERATORS.parse(procedure));

    private final ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            TABLE_BODY_CLAUSE = procedure -> head -> new TableNode((TableHeadRow) head, (TableBody) many(ROW_PREFIX.with(oneOf(
            COLUMN_SPLITTER.before(singleCellRow(ELEMENT_ELLIPSIS)), COLUMN_SPLITTER.before(singleCellRow(ROW_WILDCARD)),
            COLUMN_SPLITTER.before(tableRow((TableHeadRow) head))))).and(endWithOptionalLine()).as(TableBody::new).parse(procedure));

    private ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> singleCellRow(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeParser) {
        return single(single(nodeParser).and(endWith(COLUMN_SPLITTER)).as()).and(endWithLine()).as()
                .clause(TableRowNode::new);
    }

    private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> tableCell(
            DALNode rowPrefix, TableHeadRow head) {
        return procedure -> positionNode(cellVerificationExpression((TableRowPrefixNode) rowPrefix,
                head.getHeader(procedure.getIndex()))).parse(procedure);
    }

    private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALProcedure> cellVerificationExpression(TableRowPrefixNode rowPrefix, HeaderNode header) {
        return header.property().with(shortVerificationClause(oneOf(VERIFICATION_OPERATORS, header.operator(),
                rowPrefix.operator()).or(DEFAULT_VERIFICATION_OPERATOR), CELL_VERIFICATION_OPERAND
                .or(TABLE_CELL_RELAX_STRING)));
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> tableRow(
            TableHeadRow headRow) {
        return clause(rowPrefix -> tableLine(tableCell(rowPrefix, headRow)).as(cells -> new TableRowNode(rowPrefix, cells)));
    }

    private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> transposeTableCell(
            DALNode head, DALNode transposedTableHead) {
        return procedure -> positionNode(oneOf(ELEMENT_ELLIPSIS, ROW_WILDCARD).or(cellVerificationExpression(
                ((TransposedTableHead) transposedTableHead).getPrefix(procedure.getIndex()), (HeaderNode) head))).parse(procedure);
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> transposeTable() {
        return procedure -> prefixHead -> new TransposedTableNode(prefixHead, many(positionNode(COLUMN_SPLITTER.before(
                single(TABLE_HEADER).and(endWith(COLUMN_SPLITTER)).as())).concat(clause(header -> tableLine(
                transposeTableCell(header, prefixHead)).as(cells -> new TransposedRowNode(header, cells))))).and(atLeast(1))
                .and(endWithOptionalLine()).as(TransposedTableBody::new).mandatory("Expecting a table").parse(procedure));
    }

    private static Syntax<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure, NodeParser<DALRuntimeContext,
            DALNode, DALExpression, DALOperator, DALProcedure>, NodeParser.Mandatory<DALRuntimeContext, DALNode,
            DALExpression, DALOperator, DALProcedure>, DALNode, NodeParser.Mandatory<DALRuntimeContext, DALNode,
            DALExpression, DALOperator, DALProcedure>, List<DALNode>> tableLine(NodeParser.Mandatory<DALRuntimeContext,
            DALNode, DALExpression, DALOperator, DALProcedure> mandatory) {
        return many(mandatory).and(Syntax.Rules.mandatorySplitBy(COLUMN_SPLITTER)).and(Syntax.endOfRow(COLUMN_SPLITTER));
    }

    private Optional<DALNode> compileUserDefinedLiteral(DALProcedure dalProcedure) {
        return dalProcedure.getSourceCode().tryFetch(() -> Tokens.SYMBOL.scan(dalProcedure.getSourceCode())
                .flatMap(token -> dalProcedure.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }
}
