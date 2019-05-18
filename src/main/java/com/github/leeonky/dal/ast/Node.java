package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContext;

public interface Node {
    Object evaluate(CompilingContext context);
}
