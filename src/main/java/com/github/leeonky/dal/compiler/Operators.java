package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.compiler.Notations.Keywords;
import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.OperatorParser;
import com.github.leeonky.interpreter.Procedure;

import static com.github.leeonky.dal.ast.Operators.*;
import static com.github.leeonky.dal.compiler.Constants.PROPERTY_DELIMITER_STRING;
import static com.github.leeonky.dal.compiler.Notations.COMMA;
import static com.github.leeonky.dal.compiler.Notations.Operators.*;
import static com.github.leeonky.interpreter.FunctionUtil.not;
import static com.github.leeonky.interpreter.Parser.oneOf;
import static java.util.Optional.empty;
import static java.util.Optional.of;

public class Operators {
    private static final OperatorParser<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            DEFAULT_OPERATOR = Procedure::currentOperator,
            MAYBE_PROPERTY_SLASH = Notations.Operators.SLASH.operator(DALOperator.PropertySlash::new);

    static final OperatorParser<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            IS = Notations.Operators.IS.operator(DALOperator.Is::new),
            WHICH = Notations.Operators.WHICH.operator(DALOperator.Which::new),
            PROPERTY_DOT = Notations.Operators.DOT.operator(DALOperator.PropertyDot::new, not(DALProcedure::mayBeElementEllipsis)),
            PROPERTY_SLASH = procedure -> procedure.isEnableSlashProperty() ? MAYBE_PROPERTY_SLASH.parse(procedure) : empty(),
            PROPERTY_IMPLICIT = procedure -> of(new DALOperator.PropertyImplicit()),
            PROPERTY_META = Notations.Operators.META.operator(DALOperator.PropertyMeta::new),
            BINARY_ARITHMETIC_OPERATORS = oneOf(
                    AND.operator(() -> logical(AND, Calculator::and)),
                    OR.operator(() -> logical(OR, Calculator::or)),
                    Keywords.AND.keywordOperator(() -> logical(Keywords.AND, Calculator::and), PROPERTY_DELIMITER_STRING),
                    COMMA.operator(() -> logical(COMMA, Calculator::and), DALProcedure::isEnableCommaAnd),
                    Notations.Operators.NOT_EQUAL.operator(DALOperator.NotEqual::new),
                    Keywords.OR.keywordOperator(() -> logical(Keywords.OR, Calculator::or), PROPERTY_DELIMITER_STRING),
                    GREATER_OR_EQUAL.operator(() -> comparator(GREATER_OR_EQUAL, adapt(Calculator::greaterOrEqual))),
                    LESS_OR_EQUAL.operator(() -> comparator(LESS_OR_EQUAL, adapt(Calculator::lessOrEqual))),
                    GREATER.operator(() -> comparator(GREATER, adapt(Calculator::greater))),
                    LESS.operator(() -> comparator(LESS, adapt(Calculator::less)), not(DALProcedure::mayBeOpeningGroup)),
                    PLUS.operator(() -> plusSub(PLUS, Calculator::plus)),
                    SUBTRACTION.operator(() -> plusSub(SUBTRACTION, Calculator::subtract)),
                    MULTIPLICATION.operator(() -> mulDiv(MULTIPLICATION, Calculator::multiply)),
                    DIVISION.operator(() -> mulDiv(DIVISION, Calculator::divide))),
            UNARY_OPERATORS = oneOf(MINUS.operator(() -> unaryOpt(MINUS, unary(Calculator::negate)), not(DALProcedure::isCodeBeginning)),
                    PLUS.operator(() -> unaryOpt(PLUS, unary(Calculator::positive)), not(DALProcedure::isCodeBeginning)),
                    NOT.operator(() -> unaryOpt(NOT, unary(Calculator::not)), not(DALProcedure::mayBeUnEqual))),
            VERIFICATION_OPERATORS = oneOf(Notations.Operators.MATCHER.operator(DALOperator.Matcher::new, not(DALProcedure::mayBeMetaProperty)),
                    Notations.Operators.EQUAL.operator(DALOperator.Equal::new));

    static final OperatorParser.Mandatory<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            DEFAULT_VERIFICATION_OPERATOR = DEFAULT_OPERATOR.mandatory("");
}
