package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.SchemaNodeBak;

import static java.lang.String.format;

public class IllegalTypeException extends java.lang.RuntimeException {
    public IllegalTypeException() {
    }

    public IllegalTypeException(String message) {
        super(message);
    }

    public String assertionFailureMessage(String input, SchemaNodeBak schemaNodeBak) {
        if (getMessage() == null)
            return format("Expecting " + input + "to match schema `%s` but was not", schemaNodeBak.getSchema());
        return format("Expecting " + input + "to match schema `%s` but was not\n    %s", schemaNodeBak.getSchema(), getMessage());
    }
}
