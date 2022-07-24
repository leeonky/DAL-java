package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.ast.table.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import static com.github.leeonky.dal.ast.InputNode.INPUT_NODE;
import static com.github.leeonky.dal.compiler.Constants.PROPERTY_DELIMITER_STRING;
import static com.github.leeonky.dal.compiler.DALProcedure.*;
import static com.github.leeonky.interpreter.ClauseParser.Mandatory.clause;
import static com.github.leeonky.interpreter.ClauseParser.positionClause;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.NodeParser.positionNode;
import static com.github.leeonky.interpreter.Notation.notation;
import static com.github.leeonky.interpreter.Parser.*;
import static com.github.leeonky.interpreter.Syntax.Rules.*;
import static com.github.leeonky.interpreter.Syntax.many;
import static com.github.leeonky.interpreter.Syntax.single;
import static java.util.Optional.empty;

public class Compiler {
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
            INPUT = procedure -> when(procedure.isCodeBeginning()).optional(() -> INPUT_NODE),
            NUMBER = Tokens.NUMBER.nodeParser(DALNode::constNumber),
            INTEGER = Tokens.INTEGER.nodeParser(DALNode::constInteger),
            SINGLE_QUOTED_STRING = Notations.SINGLE_QUOTED.with(many(charNode(SINGLE_QUOTED_ESCAPES))
                    .and(endWith(Notations.SINGLE_QUOTED.getLabel())).as(DALNode::constString)),
            DOUBLE_QUOTED_STRING = Notations.DOUBLE_QUOTED.with(many(charNode(DOUBLE_QUOTED_ESCAPES))
                    .and(endWith(Notations.DOUBLE_QUOTED.getLabel())).as(DALNode::constString)),
            CONST_TRUE = Notations.Keywords.TRUE.wordNode(DALNode::constTrue, PROPERTY_DELIMITER_STRING),
            CONST_FALSE = Notations.Keywords.FALSE.wordNode(DALNode::constFalse, PROPERTY_DELIMITER_STRING),
            CONST_NULL = Notations.Keywords.NULL.wordNode(DALNode::constNull, PROPERTY_DELIMITER_STRING),
            CONST_USER_DEFINED_LITERAL = this::compileUserDefinedLiteral,
            REGEX = Notations.OPEN_REGEX.with(many(charNode(REGEX_ESCAPES)).and(endWith(Notations.CLOSE_REGEX.getLabel())).as(DALNode::regex)),
            WILDCARD = Notations.Operators.WILDCARD.node(WildcardNode::new),
            ROW_WILDCARD = Notations.Operators.ROW_WILDCARD.node(WildcardNode::new),
            CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
                    CONST_USER_DEFINED_LITERAL),
            ELEMENT_ELLIPSIS = Notations.Operators.ELEMENT_ELLIPSIS.node(token -> new ListEllipsisNode()),
            SCHEMA = Tokens.SCHEMA.nodeParser(DALNode::schema),
            INTEGER_OR_STRING = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING),
            STRING_PROPERTY = procedure -> procedure.isEnableRelaxProperty() ? single(oneOf(SINGLE_QUOTED_STRING,
                    DOUBLE_QUOTED_STRING)).as(DALNode::stringSymbol).parse(procedure) : empty(),
            NUMBER_PROPERTY = procedure -> procedure.isEnableNumberProperty() ? single(NUMBER).as(DALNode::numberSymbol)
                    .parse(procedure) : empty(),
            SYMBOL = procedure -> (procedure.isEnableRelaxProperty() ? Tokens.RELAX_SYMBOL : Tokens.SYMBOL).nodeParser(
                    DALNode::symbolNode).parse(procedure),
            DOT_SYMBOL = procedure -> (procedure.isEnableRelaxProperty() ? Tokens.RELAX_DOT_SYMBOL : Tokens.DOT_SYMBOL)
                    .nodeParser(DALNode::symbolNode).parse(procedure),
            META_SYMBOL = Tokens.DOT_SYMBOL.nodeParser(DALNode::metaSymbolNode),
            PROPERTY_PATTERN = this::propertyPattern,
            OPTIONAL_VERIFICATION_PROPERTY = lazyNode(() -> enableSlashProperty(enableRelaxProperty(OPTIONAL_PROPERTY_CHAIN)));

    private Optional<DALNode> propertyPattern(DALProcedure dalProcedure) {
        ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> patternClause =
                notation("{}").clause((token, symbol) -> new PropertyPattern(symbol));
        return oneOf(notation("{}").node(n -> new PropertyThis()), SYMBOL.with(patternClause)).parse(dalProcedure);
    }

    public NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            PROPERTY_CHAIN, OPERAND, EXPRESSION, VERIFICATION_PROPERTY,
            DEFAULT_INPUT = procedure -> INPUT_NODE,
            SCHEMA_COMPOSE = Notations.OPENING_BRACKET.with(single(many(SCHEMA.mandatory("Expect a schema"))
                    .and(Syntax.Rules.splitBy(Notations.SCHEMA_AND)).as(DALNode::elementSchemas))
                    .and(endWith(Notations.CLOSING_BRACKET)).as()).or(many(SCHEMA.mandatory("Expect a schema"))
                    .and(Syntax.Rules.splitBy(Notations.SCHEMA_AND)).as(DALNode::schemas)),
            EXPRESSION_RELAX_STRING = Tokens.EXPRESSION_RELAX_STRING.nodeParser(DALNode::relaxString),
            OBJECT_SCOPE_RELAX_STRING = Tokens.OBJECT_SCOPE_RELAX_STRING.nodeParser(DALNode::relaxString),
            LIST_SCOPE_RELAX_STRING = Tokens.LIST_SCOPE_RELAX_STRING.nodeParser(DALNode::relaxString),
            TABLE_CELL_RELAX_STRING = Tokens.TABLE_CELL_RELAX_STRING.nodeParser(DALNode::relaxString),
            DEFAULT_INDEX_HEADER = procedure -> new TableDefaultIndexHeadRow();

    public ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            ARITHMETIC_CLAUSE, VERIFICATION_CLAUSE,
            SCHEMA_CLAUSE = Operators.IS.clause(SCHEMA_COMPOSE),
            WHICH_CLAUSE = lazyClause(() -> Operators.WHICH.clause(EXPRESSION)),
            ELEMENT_ELLIPSIS_CLAUSE = Notations.Operators.ELEMENT_ELLIPSIS.clause((token, input) -> new ListEllipsisNode()),
            ROW_WILDCARD_CLAUSE = Notations.Operators.ROW_WILDCARD.clause((token, input) -> new WildcardNode(token.getContent())),
            LIST_MAPPING_CLAUSE = Notations.LIST_MAPPING.clause((token, symbolNode) -> new ListMappingNode(symbolNode)),
            META_LIST_MAPPING_CLAUSE = Notations.LIST_MAPPING.clause((token, symbolNode) -> new ListMappingNodeMeta(symbolNode)),
            IMPLICIT_PROPERTY_CLAUSE = Operators.PROPERTY_IMPLICIT.clause(oneOf(PROPERTY_PATTERN,
                    oneOf(STRING_PROPERTY, NUMBER_PROPERTY, SYMBOL).concat(LIST_MAPPING_CLAUSE))),
            EXPLICIT_PROPERTY_CLAUSE = oneOf(Operators.PROPERTY_DOT.clause(PROPERTY_PATTERN
                            .or(symbolClause(oneOf(STRING_PROPERTY, DOT_SYMBOL).concat(LIST_MAPPING_CLAUSE)))),
                    Operators.PROPERTY_SLASH.clause(symbolClause(oneOf(STRING_PROPERTY, DOT_SYMBOL)
                            .concat(LIST_MAPPING_CLAUSE))),
                    Operators.PROPERTY_IMPLICIT.clause(Notations.OPENING_BRACKET.with(single(INTEGER_OR_STRING.mandatory(
                            "Should given one property or array index in `[]`")).and(endWith(Notations.CLOSING_BRACKET))
                            .as(DALNode::bracketSymbolNode).concat(LIST_MAPPING_CLAUSE))),
                    Operators.PROPERTY_META.clause(symbolClause(META_SYMBOL.concat(META_LIST_MAPPING_CLAUSE))));

    private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> symbolClause(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeParser) {
        return nodeParser.mandatory("Expect a symbol");
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> shortVerificationClause(
            OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> operatorMandatory,
            NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> operand) {
        return SCHEMA_CLAUSE.concat(Operators.VERIFICATION_OPERATORS.clause(operand)).or(operatorMandatory.clause(operand));
    }

    private ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> ARITHMETIC_CLAUSE_CHAIN,
            VERIFICATION_CLAUSE_CHAIN, EXPLICIT_PROPERTY_CHAIN, WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN, EXPRESSION_CLAUSE;

    public Compiler() {
        PARENTHESES = lazyNode(() -> enableCommaAnd(Notations.OPENING_PARENTHESES.with(single(EXPRESSION).and(endWith(Notations.CLOSING_PARENTHESES))
                .as(DALNode::parenthesesNode))));
        PROPERTY = lazyNode(() -> oneOf(GROUP_PROPERTY, DEFAULT_INPUT.with(oneOf(EXPLICIT_PROPERTY_CLAUSE, IMPLICIT_PROPERTY_CLAUSE))));
        OPTIONAL_PROPERTY_CHAIN = PROPERTY.concatAll(EXPLICIT_PROPERTY_CLAUSE);
        PROPERTY_CHAIN = OPTIONAL_PROPERTY_CHAIN.mandatory("Expect a object property");
        VERIFICATION_PROPERTY = enableRelaxProperty(enableSlashProperty(PROPERTY_CHAIN));
        OBJECT = lazyNode(() -> disableCommaAnd(Notations.OPENING_BRACES.with(single(ELEMENT_ELLIPSIS).and(endWith(Notations.CLOSING_BRACES))
                .as(ObjectScopeNode::new).or(many(enableNumberProperty(VERIFICATION_PROPERTY).concat(shortVerificationClause(Operators.VERIFICATION_OPERATORS
                        .mandatory("Expect operator `:` or `=`"), SHORT_VERIFICATION_OPERAND.or(OBJECT_SCOPE_RELAX_STRING))))
                        .and(optionalSplitBy(Notations.COMMA)).and(endWith(Notations.CLOSING_BRACES)).as(ObjectScopeNode::new)))));
        SORTED_LIST = oneOf(Notations.Operators.PLUS.before(pureList(ListScopeNode.NatureOrder::new)),
                Notations.Operators.SUBTRACTION.before(pureList(ListScopeNode.ReverseOrder::new)));
        LIST = oneOf(pureList(ListScopeNode::new), SORTED_LIST);
        TABLE = oneOf(Notations.TRANSPOSE_MARK.with(EMPTY_TRANSPOSED_HEAD.with(transposeTable())),
                positionNode(Notations.COLUMN_SPLITTER.before(Notations.TRANSPOSE_MARK.before(Notations.COLUMN_SPLITTER.before(tableLine(ROW_PREFIX)
                        .as(TransposedTableHead::new))))).concat(transposeTable()),
                positionNode(Notations.COLUMN_SPLITTER.before(tableLine(TABLE_HEADER).as(TableHeadRow::new))).concat(TABLE_BODY_CLAUSE),
                positionNode(Notations.MATRIX_COLUMN_SPLITTER.before(DEFAULT_INDEX_HEADER).concat(TABLE_BODY_CLAUSE)));
        VERIFICATION_SPECIAL_OPERAND = oneOf(REGEX, OBJECT, LIST, WILDCARD, TABLE);
        OPERAND = lazyNode(() -> oneOf(Operators.UNARY_OPERATORS.unary(OPERAND), CONST, PROPERTY, PARENTHESES, INPUT))
                .mandatory("Expect a value or expression");
        VERIFICATION_VALUE_OPERAND = oneOf(Operators.UNARY_OPERATORS.unary(OPERAND), CONST, DEFAULT_INPUT.with(EXPLICIT_PROPERTY_CLAUSE), PARENTHESES);
        ARITHMETIC_CLAUSE = Operators.BINARY_ARITHMETIC_OPERATORS.clause(OPERAND);
        VERIFICATION_CLAUSE = Operators.VERIFICATION_OPERATORS.clause(oneOf(VERIFICATION_SPECIAL_OPERAND,
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
                .concatAll(oneOf(ARITHMETIC_CLAUSE)))).and(enabledBefore(Notations.COLUMN_SPLITTER)).as();
        GROUP_PROPERTY = disableCommaAnd(Notations.OPENING_GROUP.with(many(PROPERTY_CHAIN).and(optionalSplitBy(Notations.COMMA))
                .and(endWith(Notations.CLOSING_GROUP)).as(GroupNode::new)));
    }

    private NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> pureList(
            Function<List<Clause<DALRuntimeContext, DALNode>>, DALNode> factory) {
        return lazyNode(() -> disableCommaAnd(Notations.OPENING_BRACKET.with(many(ELEMENT_ELLIPSIS_CLAUSE.or(
                shortVerificationClause(Operators.VERIFICATION_OPERATORS.or(Operators.DEFAULT_VERIFICATION_OPERATOR),
                        SHORT_VERIFICATION_OPERAND.or(LIST_SCOPE_RELAX_STRING)))).and(optionalSplitBy(Notations.COMMA))
                .and(endWith(Notations.CLOSING_BRACKET)).as(factory))));
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
        return PROPERTY_CHAIN.parse(new DALProcedure(SourceCode.createSourceCode(sourceCode, Notations.LINE_COMMENTS),
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
            Operators.VERIFICATION_OPERATORS.parse(procedure)),
            TABLE_HEADER = procedure -> new HeaderNode((SortGroupNode) SEQUENCE.parse(procedure),
                    enableNumberProperty(VERIFICATION_PROPERTY).concat(SCHEMA_CLAUSE).parse(procedure),
                    Operators.VERIFICATION_OPERATORS.parse(procedure));

    private final ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            TABLE_BODY_CLAUSE = procedure -> head -> new TableNode((TableHeadRow) head, (TableBody) many(ROW_PREFIX.with(oneOf(
            Notations.COLUMN_SPLITTER.before(singleCellRow(ELEMENT_ELLIPSIS, (TableHeadRow) head)),
            Notations.COLUMN_SPLITTER.before(singleCellRow(ROW_WILDCARD, (TableHeadRow) head)),
            Notations.COLUMN_SPLITTER.before(tableRow((TableHeadRow) head))))).and(endWithOptionalLine()).as(TableBody::new).parse(procedure));

    private ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> singleCellRow(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeParser, TableHeadRow head) {
        return single(single(nodeParser).and(endWith(Notations.COLUMN_SPLITTER)).as()).and(endWithLine()).as()
                .clause((prefix, cell) -> new TableRowNode(prefix, cell, head));
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> tableCell(
            DALNode rowPrefix, TableHeadRow head) {
        return positionClause(ClauseParser.<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
                columnMandatory(column -> shortVerificationClause(oneOf(Operators.VERIFICATION_OPERATORS,
                head.getHeader(column).operator(), ((TableRowPrefixNode) rowPrefix).operator()).or(
                Operators.DEFAULT_VERIFICATION_OPERATOR), CELL_VERIFICATION_OPERAND.or(TABLE_CELL_RELAX_STRING))));
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> tableRow(
            TableHeadRow headRow) {
        return clause(rowPrefix -> tableLine(tableCell(rowPrefix, headRow)).as(cells -> new TableRowNode(rowPrefix, cells, headRow)));
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> transposeTableCell(
            DALNode prefix, DALNode header) {
        return positionClause(ClauseParser.<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
                columnMandatory(column -> oneOf(ELEMENT_ELLIPSIS_CLAUSE, ROW_WILDCARD_CLAUSE)
                .or(shortVerificationClause(oneOf(Operators.VERIFICATION_OPERATORS, ((HeaderNode) prefix).operator(),
                        ((TransposedTableHead) header).getPrefix(column).operator())
                        .or(Operators.DEFAULT_VERIFICATION_OPERATOR), CELL_VERIFICATION_OPERAND.or(TABLE_CELL_RELAX_STRING)))));
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> transposeTable() {
        return procedure -> header -> new TransposedTableNode(header, many(positionNode(Notations.COLUMN_SPLITTER.before(
                single(TABLE_HEADER).and(endWith(Notations.COLUMN_SPLITTER)).as())).concat(clause(prefix -> tableLine(
                transposeTableCell(prefix, header)).as(cells -> new TransposedRowNode(prefix, cells))))).and(atLeast(1))
                .and(endWithOptionalLine()).as(TransposedTableBody::new).mandatory("Expecting a table").parse(procedure));
    }

    private static Syntax<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure, NodeParser<DALRuntimeContext,
            DALNode, DALExpression, DALOperator, DALProcedure>, NodeParser.Mandatory<DALRuntimeContext, DALNode,
            DALExpression, DALOperator, DALProcedure>, DALNode, NodeParser.Mandatory<DALRuntimeContext, DALNode,
            DALExpression, DALOperator, DALProcedure>, List<DALNode>> tableLine(NodeParser.Mandatory<DALRuntimeContext,
            DALNode, DALExpression, DALOperator, DALProcedure> mandatory) {
        return many(mandatory).and(mandatorySplitBy(Notations.COLUMN_SPLITTER)).and(endOfRow(Notations.COLUMN_SPLITTER));
    }

    private static Syntax<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure, ClauseParser<DALRuntimeContext,
            DALNode, DALExpression, DALOperator, DALProcedure>, ClauseParser.Mandatory<DALRuntimeContext, DALNode,
            DALExpression, DALOperator, DALProcedure>, Clause<DALRuntimeContext, DALNode>, NodeParser.Mandatory<DALRuntimeContext,
            DALNode, DALExpression, DALOperator, DALProcedure>, List<Clause<DALRuntimeContext, DALNode>>> tableLine(
            ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> mandatory) {
        return many(mandatory).and(mandatorySplitBy(Notations.COLUMN_SPLITTER)).and(endOfRow(Notations.COLUMN_SPLITTER));
    }

    private Optional<DALNode> compileUserDefinedLiteral(DALProcedure dalProcedure) {
        return dalProcedure.getSourceCode().tryFetch(() -> Tokens.SYMBOL.scan(dalProcedure.getSourceCode())
                .flatMap(token -> dalProcedure.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }
}
