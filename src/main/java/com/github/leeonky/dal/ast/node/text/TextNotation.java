package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.interpreter.SourceCode;
import com.github.leeonky.interpreter.Token;

public class TextNotation extends DALNode {
    private final int indent;
    private final SourceCode sourceCode;

    public TextNotation(Token token, SourceCode sourceCode) {
        this.sourceCode = sourceCode;
        setPositionBegin(token.getPosition());
        indent = sourceCode.indent(token.getPosition(), "\n");
    }

    @Override
    public String inspect() {
        return "```";
    }

    public int indent() {
        return indent;
    }

    @Deprecated
    public SourceCode getSourceCode() {
        return sourceCode;
    }

}
