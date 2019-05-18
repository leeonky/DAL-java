package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

public interface Evaluatable {
    Object evaluate(CompilingContext context);
}
