package com.github.leeonky.dal.ast;

import java.util.Objects;

//TODO need clean code
public class SchemaAssertionExpression extends ExpressionIsSchema {
    private final Node assertion;

    public SchemaAssertionExpression(Node instance, SchemaNode schemaNode, Node assertion) {
        super(instance, schemaNode);
        this.assertion = assertion;
    }

    @Override
    public Node getAssertion() {
        return assertion;
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o) && Objects.equals(assertion, ((SchemaAssertionExpression) o).assertion);
    }
}
