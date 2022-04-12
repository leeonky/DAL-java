package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.ast.table.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.github.leeonky.dal.ast.DALNode.constNode;
import static com.github.leeonky.dal.compiler.Constants.DELIMITER_STRING;
import static com.github.leeonky.dal.compiler.DALProcedure.enableCommaAnd;
import static com.github.leeonky.dal.compiler.Notations.*;
import static com.github.leeonky.interpreter.FunctionUtil.not;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.NodeParser.Mandatory.clause;
import static com.github.leeonky.interpreter.NodeParser.lazy;
import static com.github.leeonky.interpreter.Parser.oneOf;
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
                    Keywords.AND.operator(DALOperator::keywordAnd),
                    Operators.COMMA.operator(DALOperator::commaAnd, DALProcedure::isEnableCommaAnd),
                    Operators.NOT_EQUAL.operator(DALOperator.NotEqual::new),
                    Keywords.OR.operator(DALOperator::keywordOr),
                    Operators.GREATER_OR_EQUAL.operator(DALOperator.GreaterOrEqual::new),
                    Operators.LESS_OR_EQUAL.operator(DALOperator.LessOrEqual::new),
                    Operators.GREATER.operator(DALOperator.Greater::new),
                    Operators.LESS.operator(DALOperator.Less::new),
                    Operators.PLUS.operator(DALOperator.Plus::new),
                    Operators.SUBTRACTION.operator(DALOperator.Subtraction::new),
                    Operators.MULTIPLICATION.operator(DALOperator.Multiplication::new),
                    Operators.DIVISION.operator(DALOperator.Division::new)),
            UNARY_OPERATORS = oneOf(
                    Operators.MINUS.operator(DALOperator.Minus::new, not(DALProcedure::isCodeBeginning)),
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
            PROPERTY, OBJECT, LIST, PARENTHESES, VERIFICATION_SPECIAL_OPERAND, VERIFICATION_VALUE_OPERAND, TABLE,
            SHORT_VERIFICATION_OPERAND, CELL_VERIFICATION_OPERAND,
            INPUT = procedure -> when(procedure.isCodeBeginning()).optional(() -> InputNode.INSTANCE),
            NUMBER = Tokens.NUMBER.nodeParser(constNode(Token::getNumber)),
            INTEGER = Tokens.INTEGER.nodeParser(constNode(Token::getInteger)),
            SINGLE_QUOTED_STRING = SINGLE_QUOTED.with(many(charNode(SINGLE_QUOTED_ESCAPES))
                    .and(endWith(SINGLE_QUOTED.getLabel())).as(DALNode::constString)),
            DOUBLE_QUOTED_STRING = DOUBLE_QUOTED.with(many(charNode(DOUBLE_QUOTED_ESCAPES))
                    .and(endWith(DOUBLE_QUOTED.getLabel())).as(DALNode::constString)),
            CONST_TRUE = Keywords.TRUE.wordNode(DALNode::constTrue, DELIMITER_STRING),
            CONST_FALSE = Keywords.FALSE.wordNode(DALNode::constFalse, DELIMITER_STRING),
            CONST_NULL = Keywords.NULL.wordNode(DALNode::constNull, DELIMITER_STRING),
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
                    DOUBLE_QUOTED_STRING)).as(DALNode::stringSymbol).parse(procedure) : empty();

    public NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            PROPERTY_CHAIN, OPERAND, EXPRESSION,
            SCHEMA_COMPOSE = OPENING_BRACKET.with(single(many(SCHEMA.mandatory("Expect a schema"))
                            .and(Syntax.Rules.splitBy(SCHEMA_AND)).as(DALNode::elementSchemas)).and(endWith(CLOSING_BRACKET)).as())
                    .or(many(SCHEMA.mandatory("Expect a schema")).and(Syntax.Rules.splitBy(SCHEMA_AND)).as(DALNode::schemas)),
            EXPRESSION_RELAX_STRING = Tokens.EXPRESSION_RELAX_STRING.nodeParser(DALNode::relaxString),
            OBJECT_SCOPE_RELAX_STRING = Tokens.OBJECT_SCOPE_RELAX_STRING.nodeParser(DALNode::relaxString),
            LIST_SCOPE_RELAX_STRING = Tokens.LIST_SCOPE_RELAX_STRING.nodeParser(DALNode::relaxString),
            TABLE_CELL_RELAX_STRING = Tokens.TABLE_CELL_RELAX_STRING.nodeParser(DALNode::relaxString),
            VERIFICATION_PROPERTY = procedure -> procedure.enableRelaxProperty(() ->
                    procedure.enableSlashProperty(() -> PROPERTY_CHAIN.parse(procedure)));

    public NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SYMBOL = Tokens.SYMBOL.nodeParser(DALNode::symbolNode),
            DOT_SYMBOL = Tokens.DOT_SYMBOL.nodeParser(DALNode::symbolNode);

    public ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            ARITHMETIC_CLAUSE, VERIFICATION_CLAUSE,
            SCHEMA_CLAUSE = IS.clause(SCHEMA_COMPOSE),
            WHICH_CLAUSE = ClauseParser.lazy(() -> WHICH.clause(EXPRESSION)),
            LIST_MAPPING = Notations.LIST_MAPPING.clause((token, symbolNode) -> new ListMappingNode(symbolNode)),
            IMPLICIT_PROPERTY = PROPERTY_IMPLICIT.clause(oneOf(STRING_PROPERTY, SYMBOL).concat(LIST_MAPPING)),
            EXPLICIT_PROPERTY = oneOf(PROPERTY_DOT.clause(oneOf(STRING_PROPERTY, DOT_SYMBOL).concat(LIST_MAPPING).mandatory(
                    "Expect a symbol")), PROPERTY_SLASH.clause(oneOf(STRING_PROPERTY, DOT_SYMBOL).concat(LIST_MAPPING).mandatory(
                    "Expect a symbol")), PROPERTY_IMPLICIT.clause(OPENING_BRACKET.with(single(INTEGER_OR_STRING.mandatory(
                    "Should given one property or array index in `[]`")).and(endWith(CLOSING_BRACKET))
                    .as(DALNode::bracketSymbolNode).concat(LIST_MAPPING))));

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> shortVerificationClause(
            OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> operatorMandatory,
            NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> operand) {
        return procedure -> SCHEMA_CLAUSE.concat(VERIFICATION_OPERATORS.clause(operand))
                .or(operatorMandatory.clause(operand)).parse(procedure);
    }

    private ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> ARITHMETIC_CLAUSE_CHAIN,
            VERIFICATION_CLAUSE_CHAIN, EXPLICIT_PROPERTY_CHAIN, WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN, EXPRESSION_CLAUSE;

    public Compiler() {
        PARENTHESES = lazy(() -> enableCommaAnd(OPENING_PARENTHESES.with(single(EXPRESSION).and(endWith(CLOSING_PARENTHESES))
                .as(DALNode::parenthesesNode))));
        PROPERTY = oneOf(EXPLICIT_PROPERTY, IMPLICIT_PROPERTY).defaultInputNode(InputNode.INSTANCE);
        PROPERTY_CHAIN = PROPERTY.mandatory("Expect a object property").recursive(EXPLICIT_PROPERTY);
        OBJECT = lazy(() -> DALProcedure.disableCommaAnd(OPENING_BRACES.with(single(ELEMENT_ELLIPSIS).and(endWith(CLOSING_BRACES))
                .as(ObjectScopeNode::new).or(many(VERIFICATION_PROPERTY.expression(shortVerificationClause(VERIFICATION_OPERATORS
                        .mandatory("Expect operator `:` or `=`"), SHORT_VERIFICATION_OPERAND.or(OBJECT_SCOPE_RELAX_STRING))))
                        .and(optionalSplitBy(COMMA)).and(endWith(CLOSING_BRACES)).as(ObjectScopeNode::new)))));
        LIST = lazy(() -> DALProcedure.disableCommaAnd(OPENING_BRACKET.with(many(ELEMENT_ELLIPSIS.ignoreInput().or(
                shortVerificationClause(VERIFICATION_OPERATORS.or(DEFAULT_VERIFICATION_OPERATOR),
                        SHORT_VERIFICATION_OPERAND.or(LIST_SCOPE_RELAX_STRING)))).and(optionalSplitBy(COMMA))
                .and(endWith(CLOSING_BRACKET)).as(ListScopeNode::new))));
        TABLE = oneOf(TRANSPOSE_MARK.with(transposeTable().input(new EmptyTransposedTableHead())),
                COLUMN_SPLITTER.before(TRANSPOSE_MARK.before(COLUMN_SPLITTER.before(
                        tableLine(ROW_PREFIX).as(TransposedTableHead::new).expression(transposeTable())))),
                COLUMN_SPLITTER.before(tableLine(TABLE_HEADER).as(TableHead::new)).expression(TABLE_BODY_CLAUSE));
        VERIFICATION_SPECIAL_OPERAND = oneOf(REGEX, OBJECT, LIST, WILDCARD, TABLE);
        OPERAND = lazy(() -> oneOf(UNARY_OPERATORS.unary(OPERAND), CONST, PROPERTY, PARENTHESES, INPUT))
                .mandatory("Expect a value or expression");
        VERIFICATION_VALUE_OPERAND = oneOf(UNARY_OPERATORS.unary(OPERAND), CONST, EXPLICIT_PROPERTY.defaultInputNode(
                InputNode.INSTANCE), PARENTHESES);
        ARITHMETIC_CLAUSE = BINARY_ARITHMETIC_OPERATORS.clause(OPERAND);
        VERIFICATION_CLAUSE = VERIFICATION_OPERATORS.clause(oneOf(VERIFICATION_SPECIAL_OPERAND,
                VERIFICATION_VALUE_OPERAND).or(EXPRESSION_RELAX_STRING));
        ARITHMETIC_CLAUSE_CHAIN = ClauseParser.lazy(() -> ARITHMETIC_CLAUSE.concat(EXPRESSION_CLAUSE));
        VERIFICATION_CLAUSE_CHAIN = ClauseParser.lazy(() -> VERIFICATION_CLAUSE.concat(EXPRESSION_CLAUSE));
        EXPLICIT_PROPERTY_CHAIN = ClauseParser.lazy(() -> EXPLICIT_PROPERTY.concat(EXPRESSION_CLAUSE));
        WHICH_CLAUSE_CHAIN = ClauseParser.lazy(() -> WHICH_CLAUSE.concat(EXPRESSION_CLAUSE));
        SCHEMA_CLAUSE_CHAIN = ClauseParser.lazy(() -> SCHEMA_CLAUSE.concat(oneOf(VERIFICATION_CLAUSE_CHAIN,
                WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN)));
        EXPRESSION_CLAUSE = oneOf(ARITHMETIC_CLAUSE_CHAIN, VERIFICATION_CLAUSE_CHAIN, EXPLICIT_PROPERTY_CHAIN,
                WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN);
        EXPRESSION = OPERAND.concat(EXPRESSION_CLAUSE);
        SHORT_VERIFICATION_OPERAND = oneOf(VERIFICATION_SPECIAL_OPERAND, VERIFICATION_VALUE_OPERAND.recursive(oneOf(ARITHMETIC_CLAUSE)));
        CELL_VERIFICATION_OPERAND = single(oneOf(oneOf(REGEX, OBJECT, LIST, WILDCARD), VERIFICATION_VALUE_OPERAND
                .recursive(oneOf(ARITHMETIC_CLAUSE)))).and(enabledBefore(COLUMN_SPLITTER)).as();
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
            SEQUENCE_AZ = Notations.SEQUENCE_AZ.node(SortNode::new),
            SEQUENCE_ZA = Notations.SEQUENCE_ZA.node(SortNode::new),
            SEQUENCE_AZ_2 = Notations.SEQUENCE_AZ_2.node(SortNode::new),
            SEQUENCE_ZA_2 = Notations.SEQUENCE_ZA_2.node(SortNode::new);

    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SEQUENCE = oneOf(
            many(SEQUENCE_AZ).and(atLeast(1)).as(SortSequenceNode::new),
            many(SEQUENCE_AZ_2).and(atLeast(1)).as(SortSequenceNode::new),
            many(SEQUENCE_ZA).and(atLeast(1)).as(SortSequenceNode::new),
            many(SEQUENCE_ZA_2).and(atLeast(1)).as(SortSequenceNode::new)).or(procedure -> SortSequenceNode.noSequence());

    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            ROW_PREFIX = procedure -> new RowPrefixNode(INTEGER.parse(procedure).map(node -> (Integer)
            ((ConstNode) node).getValue()), SCHEMA_CLAUSE.parse(procedure), VERIFICATION_OPERATORS.parse(procedure)),
            TABLE_HEADER = procedure -> new HeaderNode((SortSequenceNode) SEQUENCE.parse(procedure),
                    VERIFICATION_PROPERTY.concat(SCHEMA_CLAUSE).parse(procedure), VERIFICATION_OPERATORS.parse(procedure));

    private final ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            TABLE_BODY_CLAUSE = procedure -> head -> new TableNode((TableHead) head, (TableBody) many(ROW_PREFIX.combine(oneOf(
            COLUMN_SPLITTER.before(singleCellRow(ELEMENT_ELLIPSIS)), COLUMN_SPLITTER.before(singleCellRow(ROW_WILDCARD)),
            COLUMN_SPLITTER.before(tableRow((TableHead) head))))).and(endWithOptionalLine()).as(TableBody::new).parse(procedure));

    private ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> singleCellRow(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> element_ellipsis) {
        return single(single(element_ellipsis).and(endWith(COLUMN_SPLITTER)).as()).and(endWithLine()).as()
                .clauseParser(RowNode::new);
    }

    private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> tableCell(
            DALNode rowPrefix, TableHead head) {
        return procedure -> procedure.positionOf(cellPosition -> shortVerificationClause(oneOf(VERIFICATION_OPERATORS,
                head.getHeader(procedure).headerOperator(), ((RowPrefixNode) rowPrefix).rowOperator())
                .or(DEFAULT_VERIFICATION_OPERATOR), CELL_VERIFICATION_OPERAND.or(TABLE_CELL_RELAX_STRING))
                .input(head.getHeader(procedure).getProperty()).parse(procedure).setPositionBegin(cellPosition));
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> tableRow(
            TableHead tableHead) {
        return clause(rowPrefix -> tableLine(tableCell(rowPrefix, tableHead)).as(cells -> new RowNode(rowPrefix, cells)));
    }

    private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> transposeTableCell(
            DALNode head, DALNode transposedTableHead) {
        return procedure -> procedure.positionOf(cellPosition -> oneOf(ELEMENT_ELLIPSIS, ROW_WILDCARD)
                .or(shortVerificationClause(oneOf(VERIFICATION_OPERATORS, ((HeaderNode) head).headerOperator(),
                        ((TransposedTableHead) transposedTableHead).getPrefix(procedure.getIndex()).rowOperator())
                        .or(DEFAULT_VERIFICATION_OPERATOR), CELL_VERIFICATION_OPERAND.or(TABLE_CELL_RELAX_STRING)).
                        input(((HeaderNode) head).getProperty())).parse(procedure).setPositionBegin(cellPosition));
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> transposeTable() {
        return procedure -> prefixHead -> new TransposedTableNode(prefixHead, many(COLUMN_SPLITTER.before(
                single(TABLE_HEADER).and(endWith(COLUMN_SPLITTER)).as()).expression(clause(header -> tableLine(
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
