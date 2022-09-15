package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public interface Expectation {
    boolean verify(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext);
}
