package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.ast.opt.Factory;
import com.github.leeonky.dal.compiler.Notations.Keywords;
import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.OperatorParser;
import com.github.leeonky.interpreter.Procedure;

import static com.github.leeonky.dal.ast.opt.Factory.ExpressionContextData.adapt;
import static com.github.leeonky.dal.ast.opt.Factory.*;
import static com.github.leeonky.dal.compiler.Constants.PROPERTY_DELIMITER_STRING;
import static com.github.leeonky.dal.compiler.Notations.COMMA;
import static com.github.leeonky.dal.compiler.Notations.Operators.*;
import static com.github.leeonky.interpreter.Parser.oneOf;
import static com.github.leeonky.util.function.Extension.not;
import static java.util.Optional.empty;
import static java.util.Optional.of;

public class Operators {
    private static final OperatorParser<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression>
            DEFAULT_OPERATOR = Procedure::currentOperator,
            MAYBE_PROPERTY_SLASH = SLASH.operator(() -> Factory.executable(SLASH));

    static final OperatorParser<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression>
            IS = Notations.Operators.IS.keywordOperator(Factory::is, PROPERTY_DELIMITER_STRING),
            WHICH = Notations.Operators.WHICH.operator(Factory::which),
            PROPERTY_DOT = DOT.operator(() -> Factory.executable(DOT), not(DALProcedure::mayBeElementEllipsis)),
            PROPERTY_SLASH = procedure -> procedure.isEnableSlashProperty() ? MAYBE_PROPERTY_SLASH.parse(procedure) : empty(),
            PROPERTY_IMPLICIT = procedure -> of(Factory.executable(Notations.EMPTY)),
            PROPERTY_META = META.operator(() -> Factory.executable(META)),
            BINARY_ARITHMETIC_OPERATORS = oneOf(
                    AND.operator(() -> logical(AND, Calculator::and)),
                    OR.operator(() -> logical(OR, Calculator::or)),
                    Keywords.AND.keywordOperator(() -> logical(Keywords.AND, Calculator::and), PROPERTY_DELIMITER_STRING),
                    COMMA.operator(() -> logical(COMMA, Calculator::and), DALProcedure::isEnableCommaAnd),
                    NOT_EQUAL.operator(() -> comparator(NOT_EQUAL, adapt(Calculator::notEqual))),
                    Keywords.OR.keywordOperator(() -> logical(Keywords.OR, Calculator::or), PROPERTY_DELIMITER_STRING),
                    GREATER_OR_EQUAL.operator(() -> comparator(GREATER_OR_EQUAL, adapt(Calculator::greaterOrEqual))),
                    LESS_OR_EQUAL.operator(() -> comparator(LESS_OR_EQUAL, adapt(Calculator::lessOrEqual))),
                    GREATER.operator(() -> comparator(GREATER, adapt(Calculator::greater))),
                    LESS.operator(() -> comparator(LESS, adapt(Calculator::less)), not(DALProcedure::mayBeOpeningGroup)),
                    PLUS.operator(() -> plusSub(PLUS, Calculator::plus, com.github.leeonky.dal.runtime.Operators.PLUS)),
                    SUBTRACTION.operator(() -> plusSub(SUBTRACTION, Calculator::subtract, com.github.leeonky.dal.runtime.Operators.SUB)),
                    MULTIPLICATION.operator(() -> mulDiv(MULTIPLICATION, Calculator::multiply, com.github.leeonky.dal.runtime.Operators.MUL)),
                    DIVISION.operator(() -> mulDiv(DIVISION, Calculator::divide, com.github.leeonky.dal.runtime.Operators.DIV))),
            UNARY_OPERATORS = oneOf(MINUS.operator(() -> unary(MINUS, adapt(Calculator::negate)), not(DALProcedure::isCodeBeginning)),
                    PLUS.operator(() -> unary(PLUS, adapt(Calculator::positive)), not(DALProcedure::isCodeBeginning)),
                    NOT.operator(() -> unary(NOT, adapt(Calculator::not)), not(DALProcedure::mayBeUnEqual))),
            VERIFICATION_OPERATORS = oneOf(MATCHER.operator(Factory::match, not(DALProcedure::mayBeMetaProperty)),
                    EQUAL.operator(Factory::equal)),
            DATA_REMARK = Notations.Operators.DATA_REMARK.operator(Factory::dataRemark);

    static final OperatorParser.Mandatory<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression>
            DEFAULT_VERIFICATION_OPERATOR = DEFAULT_OPERATOR.mandatory("");
}
