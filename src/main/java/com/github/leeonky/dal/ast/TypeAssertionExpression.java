package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

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
    public Object evaluate(CompilingContext context) {
        Object instance = this.instance.evaluate(context);
        Function function = (Function) typeNode.evaluate(context);
        return new Expression(new ConstNode(function.apply(instance)), new Operator.And(), assertion).evaluate(context);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof TypeAssertionExpression
                && Objects.equals(instance, ((TypeAssertionExpression) obj).instance)
                && Objects.equals(typeNode, ((TypeAssertionExpression) obj).typeNode)
                && Objects.equals(assertion, ((TypeAssertionExpression) obj).assertion);
    }
}
