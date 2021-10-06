package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Operator;

public interface OperatorFactory {
    Operator fetch(SourceCode sourceCode);
}
