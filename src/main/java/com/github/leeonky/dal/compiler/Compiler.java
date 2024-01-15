package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.node.*;
import com.github.leeonky.dal.ast.node.table.*;
import com.github.leeonky.dal.ast.node.text.*;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import static com.github.leeonky.dal.ast.node.DALExpression.expression;
import static com.github.leeonky.dal.ast.node.InputNode.INPUT_NODE;
import static com.github.leeonky.dal.ast.opt.Factory.exclamation;
import static com.github.leeonky.dal.compiler.Constants.PROPERTY_DELIMITER_STRING;
import static com.github.leeonky.dal.compiler.DALProcedure.*;
import static com.github.leeonky.interpreter.ClauseParser.Mandatory.clause;
import static com.github.leeonky.interpreter.ClauseParser.positionClause;
import static com.github.leeonky.interpreter.NodeParser.positionNode;
import static com.github.leeonky.interpreter.Parser.*;
import static com.github.leeonky.interpreter.Rules.*;
import static com.github.leeonky.interpreter.Syntax.many;
import static com.github.leeonky.interpreter.Syntax.single;
import static com.github.leeonky.util.function.When.when;
import static java.util.Optional.empty;

//TODO splite to sub classes
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
    NodeParser<DALNode, DALProcedure>
            PROPERTY, OBJECT, SORTED_LIST, LIST, PARENTHESES, VERIFICATION_SPECIAL_OPERAND, VERIFICATION_VALUE_OPERAND,
            TABLE, SHORT_VERIFICATION_OPERAND, CELL_VERIFICATION_OPERAND, GROUP_PROPERTY, OPTIONAL_PROPERTY_CHAIN,
            OBJECT_VERIFICATION_PROPERTY,
            ROOT_INPUT = procedure -> when(procedure.isCodeBeginning()).optional(() -> INPUT_NODE),
            NUMBER = Tokens.NUMBER.nodeParser(NodeFactory::constNumber),
            INTEGER = Tokens.INTEGER.nodeParser(NodeFactory::constInteger),
            SINGLE_QUOTED_STRING = Notations.SINGLE_QUOTED.with(many(charNode(SINGLE_QUOTED_ESCAPES))
                    .and(endWith(Notations.SINGLE_QUOTED.getLabel())).as(NodeFactory::constString)),
            DOUBLE_QUOTED_STRING = Notations.DOUBLE_QUOTED.with(many(charNode(DOUBLE_QUOTED_ESCAPES))
                    .and(endWith(Notations.DOUBLE_QUOTED.getLabel())).as(NodeFactory::constString)),
            TEXT_BLOCK = positionNode(many(Notations.TEXT_BLOCK).and(atLeast(3)).as(TextBlockNotationNode::new))
                    .concat(clause(this::textAttribute)).concat(clause(node -> many(charNode(new EscapeChars()))
                            .and(endWithPosition(((NotationAttributeNode) node).endNotation()))
                            .as(ls -> new TextBlockNode((NotationAttributeNode) node, ls)))),
            STRING = oneOf(TEXT_BLOCK, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING),
            CONST_TRUE = Notations.Keywords.TRUE.wordNode(NodeFactory::constTrue, PROPERTY_DELIMITER_STRING),
            CONST_FALSE = Notations.Keywords.FALSE.wordNode(NodeFactory::constFalse, PROPERTY_DELIMITER_STRING),
            CONST_NULL = Notations.Keywords.NULL.wordNode(NodeFactory::constNull, PROPERTY_DELIMITER_STRING),
            REGEX = Notations.OPEN_REGEX.with(many(charNode(REGEX_ESCAPES))
                    .and(endWith(Notations.CLOSE_REGEX.getLabel())).as(NodeFactory::regex)),
            WILDCARD = Notations.Operators.WILDCARD.node(WildcardNode::new),
            ROW_WILDCARD = Notations.Operators.ROW_WILDCARD.node(WildcardNode::new),
            CONST = oneOf(STRING, CONST_TRUE, CONST_FALSE, CONST_NULL, this::compileUserDefinedLiteral, NUMBER),
            ELEMENT_ELLIPSIS = Notations.Operators.ELEMENT_ELLIPSIS.node(token -> new ListEllipsisNode()),
            SCHEMA = Tokens.SCHEMA.nodeParser(NodeFactory::schema),
            INTEGER_OR_STRING = oneOf(INTEGER, STRING),
            STRING_PROPERTY = procedure -> procedure.isEnableRelaxProperty() ? single(STRING)
                    .as(NodeFactory::stringSymbol).parse(procedure) : empty(),
            NUMBER_PROPERTY = procedure -> procedure.isEnableNumberProperty() ? single(NUMBER)
                    .as(NodeFactory::numberSymbol).parse(procedure) : empty(),
            SYMBOL = procedure -> (procedure.isEnableRelaxProperty() ? Tokens.RELAX_SYMBOL : Tokens.SYMBOL)
                    .nodeParser(NodeFactory::symbolNode).parse(procedure),
            TEXT_ATTRIBUTE = Tokens.RELAX_SYMBOL.nodeParser(t -> new TextBlockAttributeNode(t.getContent())),
            DOT_SYMBOL = procedure -> (procedure.isEnableRelaxProperty() ? Tokens.RELAX_DOT_SYMBOL : Tokens.DOT_SYMBOL)
                    .nodeParser(NodeFactory::symbolNode).parse(procedure),
            META_SYMBOL = Tokens.DOT_SYMBOL.nodeParser(NodeFactory::metaSymbolNode),
            PROPERTY_PATTERN = this::propertyPattern,
            OPTIONAL_VERIFICATION_PROPERTY = lazyNode(() -> enableSlashProperty(enableRelaxProperty(OPTIONAL_PROPERTY_CHAIN))),
            CONST_REMARK;

    private NodeParser.Mandatory<DALNode, DALProcedure> textAttribute(DALNode notationNode) {
        return many(TEXT_ATTRIBUTE).and(endWithLine()).as(attributes ->
                new NotationAttributeNode(notationNode, new TextBlockAttributeListNode(attributes)));
    }

    private Optional<DALNode> propertyPattern(DALProcedure dalProcedure) {
        ClauseParser<DALNode, DALProcedure> patternClause =
                Notations.THIS.clause((token, symbol) -> new PropertyPattern(symbol));
        return oneOf(Notations.THIS.node(n -> new PropertyThis()), SYMBOL.with(patternClause)).parse(dalProcedure);
    }

    public NodeParser.Mandatory<DALNode, DALProcedure>
            PROPERTY_CHAIN, OPERAND, EXPRESSION, VERIFICATION_PROPERTY,
            DEFAULT_INPUT = procedure -> INPUT_NODE,
            SCHEMA_COMPOSE = Notations.OPENING_BRACKET.with(single(many(SCHEMA.mandatory("Expect a schema"))
                    .and(splitBy(Notations.SCHEMA_AND)).as(NodeFactory::elementSchemas))
                    .and(endWith(Notations.CLOSING_BRACKET)).as()).or(many(SCHEMA.mandatory("Expect a schema"))
                    .and(splitBy(Notations.SCHEMA_AND)).as(NodeFactory::schemas)),
            EXPRESSION_RELAX_STRING = Tokens.EXPRESSION_RELAX_STRING.nodeParser(NodeFactory::relaxString),
            OBJECT_SCOPE_RELAX_STRING = Tokens.OBJECT_SCOPE_RELAX_STRING.nodeParser(NodeFactory::relaxString),
            LIST_SCOPE_RELAX_STRING = Tokens.LIST_SCOPE_RELAX_STRING.nodeParser(NodeFactory::relaxString),
            TABLE_CELL_RELAX_STRING = Tokens.TABLE_CELL_RELAX_STRING.nodeParser(NodeFactory::relaxString),
            BRACKET_RELAX_STRING = Tokens.BRACKET_RELAX_STRING.nodeParser(NodeFactory::relaxString),
            DEFAULT_INDEX_HEADER = procedure -> new DefaultIndexColumnHeaderRow(),
            DATA_REMARK = positionNode(many(charNode(new EscapeChars())).and(endWith(Notations.CLOSING_PARENTHESES.getLabel())).
                    as(NodeFactory::dataRemarkNode));

    public ClauseParser<DALNode, DALProcedure>
            ARITHMETIC_CLAUSE, VERIFICATION_CLAUSE,
            SCHEMA_CLAUSE = Operators.IS.clause(SCHEMA_COMPOSE),
            WHICH_CLAUSE = lazyClause(() -> Operators.WHICH.clause(EXPRESSION)),
            ELEMENT_ELLIPSIS_CLAUSE = Notations.Operators.ELEMENT_ELLIPSIS.clause((token, input) -> new ListEllipsisNode()),
            ROW_WILDCARD_CLAUSE = Notations.Operators.ROW_WILDCARD.clause((token, input) -> new WildcardNode(token.getContent())),
            LIST_MAPPING_CLAUSE = Notations.LIST_MAPPING.clause((token, symbolNode) -> new ListMappingNode(symbolNode)),
            EXCLAMATION_CLAUSE = positionNode(many(Notations.Operators.EXCLAMATION).and(atLeast(1)).as(ExclamationNode::new)
                    .notStartWith(Notations.Operators.NOT_EQUAL)).clause((n1, n2) -> expression(n1, exclamation(), n2)),
            DATA_REMARK_CLAUSE = Operators.DATA_REMARK.clause(DATA_REMARK),
            PROPERTY_POSTFIX = oneOf(EXCLAMATION_CLAUSE, DATA_REMARK_CLAUSE),
            META_LIST_MAPPING_CLAUSE = Notations.LIST_MAPPING.clause((token, symbolNode) -> new ListMappingNodeMeta(symbolNode)),
            IMPLICIT_PROPERTY_CLAUSE = Operators.PROPERTY_IMPLICIT.clause(oneOf(PROPERTY_PATTERN,
                    oneOf(STRING_PROPERTY, NUMBER_PROPERTY, SYMBOL).concat(LIST_MAPPING_CLAUSE))).concatAll(PROPERTY_POSTFIX),
            EXPLICIT_PROPERTY_CLAUSE = oneOf(Operators.PROPERTY_DOT.clause(PROPERTY_PATTERN.or(propertyChainNode())),
                    Operators.PROPERTY_SLASH.clause(propertyChainNode()),
                    Operators.PROPERTY_META.clause(symbolClause(META_SYMBOL.concat(META_LIST_MAPPING_CLAUSE))),
                    Operators.PROPERTY_IMPLICIT.clause(Notations.OPENING_BRACKET.with(single(INTEGER_OR_STRING.or(BRACKET_RELAX_STRING))
                            .and(endWith(Notations.CLOSING_BRACKET))
                            .as(NodeFactory::bracketSymbolNode).concat(LIST_MAPPING_CLAUSE))),
                    Operators.PROPERTY_IMPLICIT.clause(lazyNode(() -> GROUP_PROPERTY))).concatAll(PROPERTY_POSTFIX);

    private NodeParser.Mandatory<DALNode, DALProcedure> propertyChainNode() {
        return symbolClause(oneOf(STRING_PROPERTY, DOT_SYMBOL, NUMBER_PROPERTY).concat(LIST_MAPPING_CLAUSE));
    }

    private NodeParser.Mandatory<DALNode, DALProcedure> symbolClause(
            NodeParser<DALNode, DALProcedure> nodeParser) {
        return nodeParser.mandatory("Expect a symbol");
    }

    private ClauseParser.Mandatory<DALNode, DALProcedure> shortVerificationClause(
            OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression> operatorMandatory,
            NodeParser.Mandatory<DALNode, DALProcedure> operand) {
        return SCHEMA_CLAUSE.concat(Operators.VERIFICATION_OPERATORS.clause(operand)).or(operatorMandatory.clause(operand));
    }

    private ClauseParser<DALNode, DALProcedure> objectVerificationClauseChain(
            OperatorParser<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression> operator,
            NodeParser.Mandatory<DALNode, DALProcedure> operand) {
        return oneOf(SCHEMA_CLAUSE.concat(Operators.VERIFICATION_OPERATORS.clause(operand)), operator.clause(operand));
    }

    private ClauseParser<DALNode, DALProcedure> ARITHMETIC_CLAUSE_CHAIN, VERIFICATION_CLAUSE_CHAIN,
            EXPLICIT_PROPERTY_CHAIN, WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN, EXPRESSION_CLAUSE;

    public Compiler() {
        PARENTHESES = lazyNode(() -> enableCommaAnd(Notations.OPENING_PARENTHESES.with(single(EXPRESSION).and(endWith(Notations.CLOSING_PARENTHESES))
                .as(NodeFactory::parenthesesNode))));
        PROPERTY = DEFAULT_INPUT.with(oneOf(EXPLICIT_PROPERTY_CLAUSE, IMPLICIT_PROPERTY_CLAUSE));
        OPTIONAL_PROPERTY_CHAIN = PROPERTY.concatAll(EXPLICIT_PROPERTY_CLAUSE);
        PROPERTY_CHAIN = OPTIONAL_PROPERTY_CHAIN.mandatory("Expect a object property");
        VERIFICATION_PROPERTY = enableNumberProperty(enableRelaxProperty(enableSlashProperty(PROPERTY_CHAIN)));
        OBJECT_VERIFICATION_PROPERTY = many(VERIFICATION_PROPERTY).and(atLeast(1)).and(splitBy(Notations.COMMA))
//                TODO miss test for error message
                .and(endWith(this::verificationNotations, () -> "Expect a verification operator")).as(NodeFactory::createVerificationGroup);
        OBJECT = lazyNode(() -> disableCommaAnd(Notations.OPENING_BRACES.with(single(ELEMENT_ELLIPSIS).and(endWith(Notations.CLOSING_BRACES))
                .as(ObjectScopeNode::new).or(many(OBJECT_VERIFICATION_PROPERTY.concat(shortVerificationClause(Operators.VERIFICATION_OPERATORS
                                .mandatory("Expect operator `:` or `=`"), SHORT_VERIFICATION_OPERAND.or(OBJECT_SCOPE_RELAX_STRING)))
                        .concatAll(objectVerificationClauseChain(Operators.VERIFICATION_OPERATORS, SHORT_VERIFICATION_OPERAND.or(OBJECT_SCOPE_RELAX_STRING))))
                        .and(optionalSplitBy(Notations.COMMA)).and(endWith(Notations.CLOSING_BRACES)).as(ObjectScopeNode::new)))));
        SORTED_LIST = oneOf(Notations.Operators.PLUS.before(pureList(ListScopeNode.NatureOrder::new)),
                Notations.Operators.SUBTRACTION.before(pureList(ListScopeNode.ReverseOrder::new)));
        LIST = oneOf(pureList(ListScopeNode::new), SORTED_LIST);
        TABLE = oneOf(Notations.TRANSPOSE_MARK.with(EMPTY_TRANSPOSED_HEAD.with(transposeTable())),
                positionNode(Notations.COLUMN_SPLITTER.before(Notations.TRANSPOSE_MARK.before(Notations.COLUMN_SPLITTER.before(tableLine(ROW_PREFIX)
                        .as(TransposedRowHeaderRow::new))))).concat(transposeTable()),
                positionNode(Notations.COLUMN_SPLITTER.before(tableLine(TABLE_HEADER).as(ColumnHeaderRow::new))).concat(TABLE_BODY_CLAUSE),
                positionNode(Notations.MATRIX_COLUMN_SPLITTER.before(DEFAULT_INDEX_HEADER).concat(TABLE_BODY_CLAUSE)));
        VERIFICATION_SPECIAL_OPERAND = oneOf(REGEX, OBJECT, LIST, WILDCARD, TABLE);
        CONST_REMARK = CONST.concat(PARENTHESES.clause(NodeFactory::constRemarkNode));
        OPERAND = lazyNode(() -> oneOf(CONST_REMARK, Operators.UNARY_OPERATORS.unary(OPERAND), PROPERTY, PARENTHESES, ROOT_INPUT))
                .mandatory("Expect a value or expression");
        VERIFICATION_VALUE_OPERAND = oneOf(CONST_REMARK, Operators.UNARY_OPERATORS.unary(OPERAND), DEFAULT_INPUT.with(EXPLICIT_PROPERTY_CLAUSE), PARENTHESES);
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
        SHORT_VERIFICATION_OPERAND = oneOf(VERIFICATION_SPECIAL_OPERAND, VERIFICATION_VALUE_OPERAND
                .concatAll(oneOf(ARITHMETIC_CLAUSE, EXPLICIT_PROPERTY_CLAUSE)));
        CELL_VERIFICATION_OPERAND = single(oneOf(oneOf(REGEX, OBJECT, LIST, WILDCARD), VERIFICATION_VALUE_OPERAND
                .concatAll(oneOf(ARITHMETIC_CLAUSE, EXPLICIT_PROPERTY_CLAUSE)))).and(enabledBefore(Notations.COLUMN_SPLITTER)).as();
        GROUP_PROPERTY = disableCommaAnd(Notations.OPENING_GROUP.with(many(VERIFICATION_PROPERTY)
                .and(optionalSplitBy(Notations.COMMA)).and(endWith(Notations.CLOSING_GROUP)).as(GroupExpression::new)));
    }

    private boolean verificationNotations(DALProcedure procedure) {
        SourceCode sourceCode = procedure.getSourceCode();
        return sourceCode.startsWith(Notations.Operators.EQUAL)
                || sourceCode.startsWith(Notations.Operators.MATCHER, Notations.Operators.META.getLabel())
                || Notations.Operators.IS.postfix(Constants.PROPERTY_DELIMITER).stream().anyMatch(sourceCode::startsWith);
    }

    private NodeParser<DALNode, DALProcedure> pureList(Function<List<Clause<DALNode>>, DALNode> factory) {
        return lazyNode(() -> disableCommaAnd(Notations.OPENING_BRACKET.with(many(ELEMENT_ELLIPSIS_CLAUSE.or(
                shortVerificationClause(Operators.VERIFICATION_OPERATORS.or(Operators.DEFAULT_VERIFICATION_OPERATOR),
                        SHORT_VERIFICATION_OPERAND.or(LIST_SCOPE_RELAX_STRING)))).and(optionalSplitBy(Notations.COMMA))
                .and(endWith(Notations.CLOSING_BRACKET)).as(factory))));
    }

    public List<DALNode> compile(SourceCode sourceCode, DALRuntimeContext DALRuntimeContext) {
        return new ArrayList<DALNode>() {{
            DALProcedure dalParser = new DALProcedure(sourceCode, DALRuntimeContext);
            add(EXPRESSION.parse(dalParser));
            if (sourceCode.isBeginning() && sourceCode.hasCode())
                throw sourceCode.syntaxError("Unexpected token", 0);
            while (sourceCode.hasCode())
                add(EXPRESSION.parse(dalParser));
        }};
    }

    public List<Object> toChainNodes(String sourceCode) {
        return PROPERTY_CHAIN.parse(new DALProcedure(new SourceCode(sourceCode, Notations.LINE_COMMENTS),
                null)).propertyChain();
    }

    private final NodeParser<DALNode, DALProcedure>
            SEQUENCE_AZ = Notations.SEQUENCE_AZ.node(SortSymbolNode::new),
            SEQUENCE_ZA = Notations.SEQUENCE_ZA.node(SortSymbolNode::new),
            SEQUENCE_AZ_2 = Notations.SEQUENCE_AZ_2.node(SortSymbolNode::new),
            SEQUENCE_ZA_2 = Notations.SEQUENCE_ZA_2.node(SortSymbolNode::new),
            ROW_KEY = oneOf(INTEGER, OPTIONAL_VERIFICATION_PROPERTY);

    private final NodeParser.Mandatory<DALNode, DALProcedure>
            SEQUENCE = oneOf(
            many(SEQUENCE_AZ).and(atLeast(1)).as(SortGroupNode::new),
            many(SEQUENCE_AZ_2).and(atLeast(1)).as(SortGroupNode::new),
            many(SEQUENCE_ZA).and(atLeast(1)).as(SortGroupNode::new),
            many(SEQUENCE_ZA_2).and(atLeast(1)).as(SortGroupNode::new)).or(procedure -> SortGroupNode.noSequence()),
            EMPTY_TRANSPOSED_HEAD = procedure -> new EmptyTransposedRowHeaderRow();

    public ClauseParser<DALNode, DALProcedure> ROW_HEADER_CLAUSE = oneOf(DATA_REMARK_CLAUSE, SCHEMA_CLAUSE, EXCLAMATION_CLAUSE)
            .concatAll(oneOf(DATA_REMARK_CLAUSE, SCHEMA_CLAUSE, EXCLAMATION_CLAUSE));

    private final NodeParser.Mandatory<DALNode, DALProcedure>
            ROW_PREFIX = procedure -> new RowHeader(ROW_KEY.parse(procedure), ROW_HEADER_CLAUSE.parse(procedure),
            Operators.VERIFICATION_OPERATORS.parse(procedure)),
            TABLE_HEADER = procedure -> new ColumnHeader((SortGroupNode) SEQUENCE.parse(procedure),
                    VERIFICATION_PROPERTY.concat(SCHEMA_CLAUSE).parse(procedure),
                    Operators.VERIFICATION_OPERATORS.parse(procedure));

    private final ClauseParser.Mandatory<DALNode, DALProcedure>
            TABLE_BODY_CLAUSE = procedure -> head -> new TableNode((ColumnHeaderRow) head, (Body) many(ROW_PREFIX.with(oneOf(
            Notations.COLUMN_SPLITTER.before(singleCellRow(ELEMENT_ELLIPSIS, (ColumnHeaderRow) head)),
            Notations.COLUMN_SPLITTER.before(singleCellRow(ROW_WILDCARD, (ColumnHeaderRow) head)),
            Notations.COLUMN_SPLITTER.before(tableRow((ColumnHeaderRow) head))))).and(endWithOptionalLine())
            .as(Body::new).parse(procedure));

    private ClauseParser<DALNode, DALProcedure> singleCellRow(NodeParser<DALNode, DALProcedure> nodeParser,
                                                              ColumnHeaderRow head) {
        return single(single(nodeParser).and(endWith(Notations.COLUMN_SPLITTER)).as()).and(endWithLine()).as()
                .clause((prefix, cell) -> new Row(prefix, cell, head));
    }

    private ClauseParser.Mandatory<DALNode, DALProcedure> tableCell(DALNode rowPrefix, ColumnHeaderRow head) {
        return positionClause(ClauseParser.<DALNode, DALProcedure>
                columnMandatory(column -> shortVerificationClause(oneOf(Operators.VERIFICATION_OPERATORS,
                head.getHeader(column).operator(), ((RowHeader) rowPrefix).operator()).or(
                Operators.DEFAULT_VERIFICATION_OPERATOR), CELL_VERIFICATION_OPERAND.or(TABLE_CELL_RELAX_STRING))));
    }

    private ClauseParser.Mandatory<DALNode, DALProcedure> tableRow(ColumnHeaderRow columnHeaderRow) {
        return clause(rowPrefix -> tableLine(tableCell(rowPrefix, columnHeaderRow))
                .as(cells -> new Row(rowPrefix, cells, columnHeaderRow)));
    }

    private ClauseParser.Mandatory<DALNode, DALProcedure> transposeTableCell(DALNode prefix, DALNode header) {
        return positionClause(ClauseParser.<DALNode, DALProcedure>
                columnMandatory(column -> oneOf(ELEMENT_ELLIPSIS_CLAUSE, ROW_WILDCARD_CLAUSE)
                .or(shortVerificationClause(oneOf(Operators.VERIFICATION_OPERATORS, ((ColumnHeader) prefix).operator(),
                        ((TransposedRowHeaderRow) header).getRowHeader(column).operator())
                        .or(Operators.DEFAULT_VERIFICATION_OPERATOR), CELL_VERIFICATION_OPERAND.or(TABLE_CELL_RELAX_STRING)))));
    }

    private ClauseParser.Mandatory<DALNode, DALProcedure> transposeTable() {
        return procedure -> header -> new TransposedTableNode(header, many(positionNode(Notations.COLUMN_SPLITTER.before(
                single(TABLE_HEADER).and(endWith(Notations.COLUMN_SPLITTER)).as())).concat(clause(prefix -> tableLine(
                transposeTableCell(prefix, header)).as(cells -> new TransposedRow(prefix, cells))))).and(atLeast(1))
                .and(endWithOptionalLine()).as(TransposedBody::new).mandatory("Expecting a table").parse(procedure));
    }

    private static Syntax<DALNode, DALProcedure, NodeParser<DALNode, DALProcedure>, NodeParser.Mandatory<DALNode,
            DALProcedure>, DALNode, NodeParser.Mandatory<DALNode, DALProcedure>, List<DALNode>> tableLine(
            NodeParser.Mandatory<DALNode, DALProcedure> mandatory) {
        return many(mandatory).and(mandatorySplitBy(Notations.COLUMN_SPLITTER)).and(endOfRow(Notations.COLUMN_SPLITTER));
    }

    private static Syntax<DALNode, DALProcedure, ClauseParser<DALNode, DALProcedure>, ClauseParser.Mandatory<DALNode,
            DALProcedure>, Clause<DALNode>, NodeParser.Mandatory<DALNode, DALProcedure>, List<Clause<DALNode>>> tableLine(
            ClauseParser.Mandatory<DALNode, DALProcedure> mandatory) {
        return many(mandatory).and(mandatorySplitBy(Notations.COLUMN_SPLITTER)).and(endOfRow(Notations.COLUMN_SPLITTER));
    }

    private Optional<DALNode> compileUserDefinedLiteral(DALProcedure dalProcedure) {
        return dalProcedure.getSourceCode().tryFetch(() -> Tokens.USER_LITERAL_SYMBOL.scan(dalProcedure.getSourceCode())
                .flatMap(token -> dalProcedure.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstValueNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }

    private ObjectParser.Mandatory<DALProcedure, Character> charNode(EscapeChars escapeChars) {
        return procedure -> procedure.getSourceCode().popChar(escapeChars);
    }
}
