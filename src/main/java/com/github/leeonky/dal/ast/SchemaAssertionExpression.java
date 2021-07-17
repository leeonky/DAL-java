package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.Constructor;
import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.token.IllegalTypeException;

import java.util.Objects;

public class SchemaAssertionExpression extends Node {
    private final Node instance;
    private final SchemaNode schemaNode;
    private final Node assertion;

    public SchemaAssertionExpression(Node instance, SchemaNode schemaNode, Node assertion) {
        this.instance = instance;
        this.schemaNode = schemaNode;
        this.assertion = assertion;
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        try {
            Object value = ((Constructor) schemaNode.evaluate(context)).apply(instance.evaluate(context), context);
            return context.wrapInputValueAndEvaluate(value, assertion);
        } catch (IllegalTypeException ignore) {
            System.err.println("Warning: Type assertion `" + inspect() + "` got false.");
            return false;
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), schemaNode.getPositionBegin());
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof SchemaAssertionExpression
                && Objects.equals(instance, ((SchemaAssertionExpression) obj).instance)
                && Objects.equals(schemaNode, ((SchemaAssertionExpression) obj).schemaNode)
                && Objects.equals(assertion, ((SchemaAssertionExpression) obj).assertion);
    }

    @Override
    public String inspect() {
        return String.format("%s is %s which %s", instance.inspect(), schemaNode.inspect(), assertion.inspect());
    }
}
