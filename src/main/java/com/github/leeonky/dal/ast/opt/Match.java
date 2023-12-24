package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.WildcardNode;
import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.interpreter.InterpreterException;

public class Match extends DALOperator {
    public Match() {
        super(Precedence.VERIFICATION, Notations.Operators.MATCHER.getLabel(), true, Operators.MATCH);
    }

    @Override
    public Data calculateData(DALExpression expression, RuntimeContextBuilder.DALRuntimeContext context) {
        if (expression.right() instanceof WildcardNode)
            return expression.right().verify(expression.left(), this, context);
        try {
            return context.calculate(expression.left().evaluateData(context),
                    expression.operator(), expression.right().evaluateData(context));
        } catch (InterpreterException | IllegalOperationException | ExpressionException e) {
            throw e;
        } catch (AssertionError error) {
            throw new AssertionFailure(error.getMessage(), expression.right().getPositionBegin());
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), expression.operator().getPosition());
        }
    }

    @Override
    public String inspect(String node1, String node2) {
        return String.format("%s%s %s", node1, label, node2);
    }
}
