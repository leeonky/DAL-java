package com.github.leeonky.dal.runtime;

public interface Schema {
    void verify(Data data) throws SchemaAssertionFailure;
}
