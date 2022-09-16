package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public interface Expectation {
    boolean verify(RuntimeContextBuilder.DALRuntimeContext runtimeContext);
}
