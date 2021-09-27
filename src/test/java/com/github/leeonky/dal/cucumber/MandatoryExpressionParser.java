package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.Node;

//TODO use generic
public interface MandatoryExpressionParser {
    Node fetch(SourceCode sourceCode, Node previous);

    default MandatoryExpressionParser recursive() {
        return (sourceCode, previous) -> fetch(sourceCode, fetch(sourceCode, previous));
    }
}
