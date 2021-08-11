package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;

public class TypeWhichExpression extends Node {
    private final TypeExpression typeExpression;
    private final Node clause;

    public TypeWhichExpression(TypeExpression typeExpression, Node clause) {
        this.typeExpression = typeExpression;
        this.clause = clause;
    }

    @Override
    public String inspect() {
        return typeExpression.inspect() + " which " + clause.inspect();
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        try {
            if ((Boolean) typeExpression.evaluate(context))
                return context.wrapInputValueAndEvaluate(typeExpression.getTypeInstance(), clause, typeExpression.getSchemaName());
            return false;
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }
}
