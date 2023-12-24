package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.opt.DALOperator;

public interface Expectation {

    Data equalTo(DALOperator operator, Data actual);

    Data matches(DALOperator operator, Data actual);
}
