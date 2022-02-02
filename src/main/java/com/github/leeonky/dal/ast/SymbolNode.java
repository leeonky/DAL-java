package com.github.leeonky.dal.ast;

import com.github.leeonky.interpreter.Token;

public class SymbolNode extends DALNode {
    private final String symbol;

    public SymbolNode(String symbol) {
        this.symbol = symbol;
    }

    public static SymbolNode symbolNode(Token token) {
        return new SymbolNode(token.getContent());
    }

    @Override
    public String inspect() {
        return symbol;
    }
}
