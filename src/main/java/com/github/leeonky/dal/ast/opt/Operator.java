package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.DALCompiler;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.SourceCode;

public abstract class Operator {
    private final String code;
    private final boolean isKeyword;

    public Operator(String code) {
        this.code = code;
        isKeyword = false;
    }

    public Operator(String code, boolean isKeyword) {
        this.code = code;
        this.isKeyword = isKeyword;
    }

    public abstract Object calculate(CompilingContext context, Node node1, Node node2);

    public boolean getFrom(SourceCode sourceCode) {
        return isMatched(sourceCode) && (sourceCode.substring(length()) != null);
    }

    private boolean isMatched(SourceCode sourceCode) {
        return sourceCode.startsWith(code) && (!isKeyword || DALCompiler.isSpliter(sourceCode, length()));
    }

    public int length() {
        return code.length();
    }
}
