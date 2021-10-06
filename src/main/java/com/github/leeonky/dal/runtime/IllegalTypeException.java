package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.SchemaNode;

import static java.lang.String.format;

public class IllegalTypeException extends java.lang.RuntimeException {
    public IllegalTypeException() {
    }

    public IllegalTypeException(String message) {
        super(message);
    }

    public String assertionFailureMessage(SchemaNode schemaNode) {
        if (getMessage() == null)
            return format("expect matches schema `%s` but was not", schemaNode.getSchema());
        return format("expect matches schema `%s` but was not\n    %s", schemaNode.getSchema(), getMessage());
    }
}
