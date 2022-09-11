package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
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
                    Notations.Operators.AND.operator(() -> logical(AND, Calculator::and)),
                    Notations.Operators.OR.operator(() -> logical(OR, Calculator::or)),
                    Notations.Keywords.AND.keywordOperator(() -> logical(Notations.Keywords.AND, Calculator::and), PROPERTY_DELIMITER_STRING),
                    Notations.Operators.COMMA.operator(() -> logical(COMMA, Calculator::and), DALProcedure::isEnableCommaAnd),
                    Notations.Operators.NOT_EQUAL.operator(DALOperator.NotEqual::new),
                    Notations.Keywords.OR.keywordOperator(() -> logical(Notations.Keywords.OR, Calculator::or), PROPERTY_DELIMITER_STRING),
                    Notations.Operators.GREATER_OR_EQUAL.operator(() -> comparator(GREATER_OR_EQUAL, Calculator::greaterOrEqual)),
                    Notations.Operators.LESS_OR_EQUAL.operator(() -> comparator(LESS_OR_EQUAL, Calculator::lessOrEqual)),
                    Notations.Operators.GREATER.operator(() -> comparator(GREATER, Calculator::greater)),
                    Notations.Operators.LESS.operator(() -> comparator(LESS, Calculator::less), not(DALProcedure::mayBeOpeningGroup)),
                    Notations.Operators.PLUS.operator(() -> plusSub(PLUS, Calculator::plus)),
                    Notations.Operators.SUBTRACTION.operator(() -> plusSub(SUBTRACTION, Calculator::subtract)),
                    Notations.Operators.MULTIPLICATION.operator(() -> mulDiv(MULTIPLICATION, Calculator::multiply)),
                    Notations.Operators.DIVISION.operator(() -> mulDiv(DIVISION, Calculator::divide))),
            UNARY_OPERATORS = oneOf(Notations.Operators.MINUS.operator(DALOperator.Minus::new, not(DALProcedure::isCodeBeginning)),
                    Notations.Operators.PLUS.operator(DALOperator.Positive::new, not(DALProcedure::isCodeBeginning)),
                    Notations.Operators.NOT.operator(DALOperator.Not::new, not(DALProcedure::mayBeUnEqual))),
            VERIFICATION_OPERATORS = oneOf(Notations.Operators.MATCHER.<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
                            operator(DALOperator.Matcher::new, not(DALProcedure::mayBeMetaProperty)),
                    Notations.Operators.EQUAL.operator(DALOperator.Equal::new));

    static final OperatorParser.Mandatory<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            DEFAULT_VERIFICATION_OPERATOR = DEFAULT_OPERATOR.mandatory("");
}
