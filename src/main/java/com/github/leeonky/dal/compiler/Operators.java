package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.OperatorParser;
import com.github.leeonky.interpreter.Procedure;

import static com.github.leeonky.dal.compiler.Constants.PROPERTY_DELIMITER_STRING;
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
                    Notations.Operators.AND.operator(DALOperator::operatorAnd),
                    Notations.Operators.OR.operator(DALOperator::operatorOr),
                    Notations.Keywords.AND.keywordOperator(DALOperator::keywordAnd, PROPERTY_DELIMITER_STRING),
                    Notations.Operators.COMMA.operator(DALOperator::commaAnd, DALProcedure::isEnableCommaAnd),
                    Notations.Operators.NOT_EQUAL.operator(DALOperator.NotEqual::new),
                    Notations.Keywords.OR.keywordOperator(DALOperator::keywordOr, PROPERTY_DELIMITER_STRING),
                    Notations.Operators.GREATER_OR_EQUAL.operator(DALOperator.GreaterOrEqual::new),
                    Notations.Operators.LESS_OR_EQUAL.operator(DALOperator.LessOrEqual::new),
                    Notations.Operators.GREATER.operator(DALOperator.Greater::new),
                    Notations.Operators.LESS.operator(DALOperator.Less::new, not(DALProcedure::mayBeOpeningGroup)),
                    Notations.Operators.PLUS.operator(DALOperator.Plus::new),
                    Notations.Operators.SUBTRACTION.operator(DALOperator.Subtraction::new),
                    Notations.Operators.MULTIPLICATION.operator(DALOperator.Multiplication::new),
                    Notations.Operators.DIVISION.operator(DALOperator.Division::new)),
            UNARY_OPERATORS = oneOf(Notations.Operators.MINUS.operator(DALOperator.Minus::new, not(DALProcedure::isCodeBeginning)),
                    Notations.Operators.PLUS.operator(DALOperator.Positive::new, not(DALProcedure::isCodeBeginning)),
                    Notations.Operators.NOT.operator(DALOperator.Not::new, not(DALProcedure::mayBeUnEqual))),
            VERIFICATION_OPERATORS = oneOf(Notations.Operators.MATCHER.<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
                            operator(DALOperator.Matcher::new, not(DALProcedure::mayBeMetaProperty)),
                    Notations.Operators.EQUAL.operator(DALOperator.Equal::new));

    static final OperatorParser.Mandatory<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            DEFAULT_VERIFICATION_OPERATOR = DEFAULT_OPERATOR.mandatory("");
}
