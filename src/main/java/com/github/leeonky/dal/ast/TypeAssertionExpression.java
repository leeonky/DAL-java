package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

import java.util.Objects;

public class TypeAssertionExpression implements Node {
    private final Node instance;
    private final String type;
    private final Node assertion;

    public TypeAssertionExpression(Node instance, String type, Node assertion) {
        this.instance = instance;
        this.type = type;
        this.assertion = assertion;
    }

    @Override
    public Object evaluate(CompilingContext context) {
        throw new IllegalStateException();
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof TypeAssertionExpression
                && Objects.equals(instance, ((TypeAssertionExpression) obj).instance)
                && Objects.equals(type, ((TypeAssertionExpression) obj).type)
                && Objects.equals(assertion, ((TypeAssertionExpression) obj).assertion);
    }
}
