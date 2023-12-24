package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public interface Operation {
    boolean match(Data v1, DALOperator operator, Data v2, DALRuntimeContext context);

    Data operate(Data v1, DALOperator operator, Data v2, DALRuntimeContext context);
}
