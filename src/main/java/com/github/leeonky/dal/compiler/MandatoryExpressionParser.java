package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

public interface MandatoryExpressionParser {
    Node fetch(SourceCode sourceCode, Node previous);
}
