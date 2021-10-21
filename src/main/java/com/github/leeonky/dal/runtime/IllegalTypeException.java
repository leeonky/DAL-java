package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.SchemaNode;

import static java.lang.String.format;

public class IllegalTypeException extends java.lang.RuntimeException {
    public IllegalTypeException() {
    }

    public IllegalTypeException(String message) {
        super(message);
    }

    //    TODO fix incorrect words: expect
    public String assertionFailureMessage(SchemaNode schemaNode) {
        if (getMessage() == null)
            return format("expect matches schema `%s` but was not", schemaNode.getSchema());
        return format("expect matches schema `%s` but was not\n    %s", schemaNode.getSchema(), getMessage());
    }

    //TODO duplicated string
    public String assertionFailureMessage(SchemaNode schemaNode, int i) {
        if (getMessage() == null)
            return format("expect element[%d] matches schema `%s` but was not", i, schemaNode.getSchema());
        return format("expect element[%d] matches schema `%s` but was not\n    %s", i, schemaNode.getSchema(), getMessage());
    }
}
