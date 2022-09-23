package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.SchemaNode;

import java.util.stream.Collectors;

import static java.lang.String.format;
import static java.util.Arrays.stream;

public class IllegalTypeException extends java.lang.RuntimeException {
    public IllegalTypeException() {
    }

    public IllegalTypeException(String message) {
        super(message);
    }

    public String assertionFailureMessage(String input, SchemaNode schemaNode) {
        String message = format("Expected " + input + "to match schema `%s` but was not", schemaNode.inspect());
        return getMessage() == null ? message
                : message + "\n" + stream(getMessage().split("\n")).collect(Collectors.joining("\n    ", "    ", ""));
    }
}
