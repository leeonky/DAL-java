package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.SchemaNode;

import static java.lang.String.format;

public class IllegalTypeException extends java.lang.RuntimeException {
    public IllegalTypeException() {
    }

    public IllegalTypeException(String message) {
        super(message);
    }

    public String assertionFailureMessage(String input, SchemaNode schemaNode) {
        if (getMessage() == null)
            return format("Expecting " + input + "to match schema `%s` but was not", schemaNode.inspect());
        return format("Expecting " + input + "to match schema `%s` but was not\n    %s", schemaNode.inspect(), getMessage());
    }
}
