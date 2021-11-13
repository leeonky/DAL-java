package com.github.leeonky.dal.runtime;

public interface Schema {
    void verify(DataObject data) throws SchemaAssertionFailure;
}
