package com.github.leeonky.dal.type;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.SchemaAssertionFailure;

public interface Schema {
    void verify(Data data) throws SchemaAssertionFailure;
}
