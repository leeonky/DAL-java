package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

@Deprecated
public interface Expectation_BK {
    boolean verify(Data actual, RuntimeContextBuilder.DALRuntimeContext runtimeContext);
}
