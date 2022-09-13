package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public interface FieldSchema {
    boolean verify(RuntimeContextBuilder.DALRuntimeContext runtimeContext);
}
