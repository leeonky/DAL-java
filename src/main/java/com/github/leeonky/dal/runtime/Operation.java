package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public interface Operation {
    boolean match(Data v1, Data v2, DALRuntimeContext context);

    Object operate(Data v1, Data v2, DALRuntimeContext context);
}
