package com.github.leeonky.dal.type;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.SchemaAssertionFailure;

public interface Schema {
    default void verify(Data data) throws SchemaAssertionFailure {
    }
}
