package com.github.leeonky.dal.ast.opt;

public abstract class SymbolOperator implements Operator {
    protected final String code;

    public SymbolOperator(String code) {
        this.code = code;
    }

    @Override
    public boolean isMatch(String content) {
        return content.startsWith(code);
    }

    @Override
    public int length() {
        return code.length();
    }
}
