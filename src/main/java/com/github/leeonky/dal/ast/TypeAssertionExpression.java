package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.token.IllegalTypeException;

import java.util.Objects;
import java.util.function.Function;

public class TypeAssertionExpression extends Node {
    private final Node instance;
    private final TypeNode typeNode;
    private final Node assertion;

    public TypeAssertionExpression(Node instance, TypeNode typeNode, Node assertion) {
        this.instance = instance;
        this.typeNode = typeNode;
        this.assertion = assertion;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Object evaluate(CompilingContext context) {
        try {
            Object value = ((Function) typeNode.evaluate(context)).apply(instance.evaluate(context));
            return context.wrapInputValueAndEvaluate(value, assertion);
        } catch (IllegalTypeException ignore) {
            return false;
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), typeNode.getPositionBegin());
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof TypeAssertionExpression
                && Objects.equals(instance, ((TypeAssertionExpression) obj).instance)
                && Objects.equals(typeNode, ((TypeAssertionExpression) obj).typeNode)
                && Objects.equals(assertion, ((TypeAssertionExpression) obj).assertion);
    }
}
