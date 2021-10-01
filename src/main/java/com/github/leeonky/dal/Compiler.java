package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.compiler.MandatoryNodeParser;
import com.github.leeonky.dal.compiler.SourceCode;

public class Compiler {
    public Node compile(SourceCode sourceCode) {
        return MandatoryNodeParser.EXPRESSION.fetch(sourceCode);
    }
}
