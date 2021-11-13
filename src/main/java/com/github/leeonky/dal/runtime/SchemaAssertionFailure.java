package com.github.leeonky.dal.runtime;

public class SchemaAssertionFailure extends Exception {
    public SchemaAssertionFailure(String message) {
        super(message);
    }
}
