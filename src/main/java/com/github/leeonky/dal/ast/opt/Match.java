package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.ast.node.*;
import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.interpreter.InterpreterException;

public class Match extends DALOperator {
    public Match() {
        super(Precedence.VERIFICATION, Notations.Operators.MATCHER.getLabel(), true);
    }

    @Override
    public Data calculateData(DALExpression expression, RuntimeContextBuilder.DALRuntimeContext context) {
        if (expression.right() instanceof TableNode
                || expression.right() instanceof TransposedTableNode
                || expression.right() instanceof ListScopeNode
                || expression.right() instanceof ObjectScopeNode
                || expression.right() instanceof WildcardNode)
            return expression.right().verify(expression.left(), this, context);
        try {
            return context.calculate(expression.left().evaluateData(context), Operators.MATCH,
                    expression.right().evaluateData(context));
        } catch (InterpreterException | IllegalOperationException e) {
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
