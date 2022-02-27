package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.ast.table.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.github.leeonky.dal.ast.DALNode.constNode;
import static com.github.leeonky.dal.compiler.DALProcedure.enableCommaAnd;
import static com.github.leeonky.dal.compiler.Notations.*;
import static com.github.leeonky.interpreter.ClauseParser.oneOf;
import static com.github.leeonky.interpreter.FunctionUtil.not;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.NodeParser.lazy;
import static com.github.leeonky.interpreter.NodeParser.oneOf;
import static com.github.leeonky.interpreter.OperatorParser.oneOf;
import static com.github.leeonky.interpreter.Syntax.many;
import static com.github.leeonky.interpreter.Syntax.single;
import static java.util.Optional.of;

public class Compiler {

    private static final OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            DEFAULT_OPERATOR = Procedure::currentOperator,
            IS = Operators.IS.operatorParser(DALOperator.Is::new),
            WHICH = Operators.WHICH.operatorParser(DALOperator.Which::new),
            PROPERTY_DOT = Operators.DOT.operatorParser(DALOperator.PropertyDot::new, not(DALProcedure::mayBeElementEllipsis)),
            PROPERTY_IMPLICIT = procedure -> of(new DALOperator.PropertyImplicit()),
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
                    Operators.DIVISION.operatorParser(DALOperator.Division::new)),
            UNARY_OPERATORS = oneOf(
                    Operators.MINUS.operatorParser(DALOperator.Minus::new, not(DALProcedure::isCodeBeginning)),
                    Operators.NOT.operatorParser(DALOperator.Not::new, not(DALProcedure::mayBeUnEqual))),
            JUDGEMENT_OPERATORS = oneOf(
                    Operators.MATCHER.<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
                            operatorParser(DALOperator.Matcher::new),
                    Operators.EQUAL.operatorParser(DALOperator.Equal::new));

    private static final OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            DEFAULT_JUDGEMENT_OPERATOR = DEFAULT_OPERATOR.mandatory("");

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
            PROPERTY, OBJECT, LIST, PARENTHESES, JUDGEMENT,
            INPUT = procedure -> when(procedure.isCodeBeginning()).optional(() -> InputNode.INSTANCE),
            NUMBER = Tokens.NUMBER.nodeParser(constNode(Token::getNumber)),
            INTEGER = Tokens.INTEGER.nodeParser(constNode(Token::getInteger)),
            SINGLE_QUOTED_STRING = SINGLE_QUOTED.and(many(charNode(SINGLE_QUOTED_ESCAPES))
                    .endWith(SINGLE_QUOTED.getLabel()).as(DALNode::constString)),
            DOUBLE_QUOTED_STRING = DOUBLE_QUOTED.and(many(charNode(DOUBLE_QUOTED_ESCAPES))
                    .endWith(DOUBLE_QUOTED.getLabel()).as(DALNode::constString)),
            CONST_TRUE = Keywords.TRUE.nodeParser(DALNode::constTrue),
            CONST_FALSE = Keywords.FALSE.nodeParser(DALNode::constFalse),
            CONST_NULL = Keywords.NULL.nodeParser(DALNode::constNull),
            CONST_USER_DEFINED_LITERAL = this::compileUserDefinedLiteral,
            REGEX = OPEN_REGEX.and(many(charNode(REGEX_ESCAPES)).endWith(CLOSE_REGEX.getLabel()).as(DALNode::regex)),
            IMPLICIT_PROPERTY = PROPERTY_IMPLICIT.clause(Tokens.SYMBOL.nodeParser(DALNode::symbolNode))
                    .defaultInputNode(InputNode.INSTANCE),
            WILDCARD = Notations.Operators.WILDCARD.nodeParser(WildcardNode::new),
            ROW_WILDCARD = Notations.Operators.ROW_WILDCARD.nodeParser(WildcardNode::new),
            CONST = oneOf(NUMBER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL,
                    CONST_USER_DEFINED_LITERAL),
            ELEMENT_ELLIPSIS = Operators.ELEMENT_ELLIPSIS.nodeParser(token -> new ListEllipsisNode()),
            EMPTY_CELL = procedure -> when(procedure.emptyCell()).optional(EmptyCellNode::new),
            TABLE = lazy(() -> oneOf(transposeTableIndex(), table(),
                    TRANSPOSE_MARK.and(transposeTable().input(new EmptyTransposedTableHead())))),
            SCHEMA = Tokens.SCHEMA.nodeParser(DALNode::schema),
            INTEGER_OR_STRING = oneOf(INTEGER, SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING);

    public NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SCHEMA_COMPOSE = OPENING_BRACKET.and(single(many(SCHEMA.mandatory("expect a schema")).splitBy(SCHEMA_AND)
            .as(DALNode::elementSchemas)).endWith(CLOSING_BRACKET).as()).or(many(SCHEMA.mandatory("expect a schema"))
            .splitBy(SCHEMA_AND).as(DALNode::schemas)),
            PROPERTY_CHAIN, OPERAND, EXPRESSION, SHORT_JUDGEMENT_OPERAND;

    public NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            SYMBOL = Tokens.SYMBOL.nodeParser(DALNode::symbolNode);

    public ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            ARITHMETIC_CLAUSE, JUDGEMENT_CLAUSE,
            SCHEMA_CLAUSE = IS.clause(SCHEMA_COMPOSE),
            WHICH_CLAUSE = ClauseParser.lazy(() -> WHICH.clause(EXPRESSION)),

    EXPLICIT_PROPERTY = oneOf(PROPERTY_DOT.clause(Tokens.DOT_SYMBOL.nodeParser(DALNode::symbolNode).mandatory(
            "expect a symbol")), PROPERTY_IMPLICIT.clause(OPENING_BRACKET.and(single(INTEGER_OR_STRING.mandatory(
            "should given one property or array index in `[]`")).endWith(CLOSING_BRACKET).as(DALNode::bracketSymbolNode))));

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALProcedure> shortJudgementClause(OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALProcedure> operatorMandatory) {
        return procedure -> SCHEMA_CLAUSE.concat(JUDGEMENT_OPERATORS.clause(SHORT_JUDGEMENT_OPERAND))
                .or(operatorMandatory.clause(SHORT_JUDGEMENT_OPERAND)).parse(procedure);
    }

    private ClauseParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> ARITHMETIC_CLAUSE_CHAIN,
            JUDGEMENT_CLAUSE_CHAIN, EXPLICIT_PROPERTY_CHAIN, WHICH_CLAUSE_CHAIN, SCHEMA_CLAUSE_CHAIN, EXPRESSION_CLAUSE;

    public Compiler() {
        PARENTHESES = lazy(() -> enableCommaAnd(OPENING_PARENTHESES.and(single(EXPRESSION).endWith(CLOSING_PARENTHESES)
                .as(DALNode::parenthesesNode))));
        PROPERTY = oneOf(EXPLICIT_PROPERTY.defaultInputNode(InputNode.INSTANCE), IMPLICIT_PROPERTY);
        PROPERTY_CHAIN = PROPERTY.mandatory("expect a object property").recursive(EXPLICIT_PROPERTY);
        OBJECT = DALProcedure.disableCommaAnd(OPENING_BRACES.and(many(PROPERTY_CHAIN.expression(shortJudgementClause(
                JUDGEMENT_OPERATORS.mandatory("expect operator `:` or `=`")))).optionalSplitBy(Notations.COMMA)
                .endWith(CLOSING_BRACES).as(ObjectScopeNode::new)));
        LIST = DALProcedure.disableCommaAnd(OPENING_BRACKET.and(many(ELEMENT_ELLIPSIS.ignoreInput().or(
                shortJudgementClause(JUDGEMENT_OPERATORS.or(DEFAULT_JUDGEMENT_OPERATOR)))).optionalSplitBy(COMMA)
                .endWith(CLOSING_BRACKET).as(ListScopeNode::new)));
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
            SEQUENCE = oneOf(
            many(SEQUENCE_AZ).atLeast(1).as(SortSequenceNode::new), many(SEQUENCE_AZ_2).atLeast(1).as(SortSequenceNode::new),
            many(SEQUENCE_ZA).atLeast(1).as(SortSequenceNode::new), many(SEQUENCE_ZA_2).atLeast(1).as(SortSequenceNode::new))
            .or(procedure -> SortSequenceNode.noSequence());


    private final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            ROW_PREFIX = procedure -> new RowPrefixNode(INTEGER.parse(procedure).map(node -> (Integer)
            ((ConstNode) node).getValue()), SCHEMA_CLAUSE.parse(procedure), JUDGEMENT_OPERATORS.parse(procedure)),
            TABLE_HEADER = procedure -> new HeaderNode((SortSequenceNode) SEQUENCE.parse(procedure),
                    PROPERTY_CHAIN.concat(SCHEMA_CLAUSE).parse(procedure), JUDGEMENT_OPERATORS.parse(procedure)),
            PREFIX_ROW = many(ROW_PREFIX).splitBy(COLUMN_SPLITTER).endWithLine().as(TransposedTableHead::new);

    private final ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            TABLE_BODY_CLAUSE = procedure -> head -> new TableNode((TableHead) head, (TableBody) many(ROW_PREFIX.combine(oneOf(
            COLUMN_SPLITTER.before(single(ELEMENT_ELLIPSIS).endWith(COLUMN_SPLITTER).as()).clauseParser(RowNode::new),
            COLUMN_SPLITTER.before(single(ROW_WILDCARD).endWith(COLUMN_SPLITTER).as()).clauseParser(RowNode::new),
            COLUMN_SPLITTER.before(tableRow((TableHead) head))))).as(TableBody::new).parse(procedure));

    private NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> table() {
        return COLUMN_SPLITTER.before(tableLine(TABLE_HEADER).as(TableHead::new)).expression(TABLE_BODY_CLAUSE);
    }

    private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> tableCell(
            DALNode rowPrefix, TableHead head) {
        return procedure -> procedure.positionOf(cellPosition -> shortJudgementClause(oneOf(JUDGEMENT_OPERATORS,
                head.getHeader(procedure).headerOperator(), ((RowPrefixNode) rowPrefix).rowOperator())
                .or(DEFAULT_JUDGEMENT_OPERATOR)).input(head.getHeader(procedure).getProperty()).parse(procedure)
                .setPositionBegin(cellPosition));
    }

    private NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> tableCell(
            HeaderNode head, TransposedTableHead transposedTableHead) {
        return procedure -> procedure.positionOf(cellPosition -> oneOf(ELEMENT_ELLIPSIS, EMPTY_CELL, ROW_WILDCARD)
                .or(shortJudgementClause(oneOf(JUDGEMENT_OPERATORS, head.headerOperator(), transposedTableHead.getPrefix(
                        procedure.getIndex()).rowOperator()).or(DEFAULT_JUDGEMENT_OPERATOR)).input(head.getProperty()))
                .parse(procedure).setPositionBegin(cellPosition));
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> tableRow(
            TableHead tableHead) {
//            TODO method for input->nodeparser => clauseparser
        return procedure -> rowPrefix -> tableLine(tableCell(rowPrefix, tableHead)).as(cells -> {
            if (cells.size() != tableHead.size())
                throw procedure.getSourceCode().syntaxError("Different cell size", 0);
            return new RowNode(rowPrefix, cells);
        }).parse(procedure);
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> tableRow(
            TransposedTableHead prefix) {
        return procedure -> header -> tableLine(tableCell((HeaderNode) header, prefix))
                .as(cells -> new TransposedRowNode(header, cells)).parse(procedure);
    }

    private ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> transposeTable() {
        return procedure -> prefixHead -> {
            //        TODO test not allow empty table >> with out rows
            return many(COLUMN_SPLITTER.before(single(TABLE_HEADER).endWith(COLUMN_SPLITTER).as()).expression(tableRow(
                    (TransposedTableHead) prefixHead))).as(dalNodes -> new TransposedTableNode(prefixHead, dalNodes))
                    .parse(procedure);
        };
    }
    //TODO should test integer or something after table
//: | a   | b   |
//0 | 'a' | 'b' |
//is a
//        Row prefix ok but no cell

    private NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> transposeTableIndex() {
        return COLUMN_SPLITTER.before(TRANSPOSE_MARK.before(COLUMN_SPLITTER.before(PREFIX_ROW.expression(transposeTable()))));
    }

    private static Syntax<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure, NodeParser<DALRuntimeContext,
            DALNode, DALExpression, DALOperator, DALProcedure>, NodeParser.Mandatory<DALRuntimeContext, DALNode,
            DALExpression, DALOperator, DALProcedure>, DALNode, NodeParser.Mandatory<DALRuntimeContext, DALNode,
            DALExpression, DALOperator, DALProcedure>, List<DALNode>> tableLine(NodeParser.Mandatory<DALRuntimeContext,
            DALNode, DALExpression, DALOperator, DALProcedure> mandatory) {
        return many(mandatory).mandatoryTailSplitBy(COLUMN_SPLITTER).endWithLine();
    }

    private Optional<DALNode> compileUserDefinedLiteral(DALProcedure dalProcedure) {
        return dalProcedure.getSourceCode().tryFetch(() -> Tokens.SYMBOL.scan(dalProcedure.getSourceCode())
                .flatMap(token -> dalProcedure.getRuntimeContext().takeUserDefinedLiteral(token.getContent())
                        .map(result -> new ConstNode(result.getValue()).setPositionBegin(token.getPosition()))));
    }
}
