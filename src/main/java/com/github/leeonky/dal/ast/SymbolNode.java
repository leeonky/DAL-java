package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Token;

public class SymbolNode extends DALNode {
    private final Object symbol;
    private final Type type;

    public SymbolNode(Object symbol, Type type) {
        this.symbol = symbol;
        this.type = type;
    }

    @Override
    public String inspect() {
        return type.inspect(symbol);
    }

    @Override
    public Data evaluateDataObject(RuntimeContextBuilder.DALRuntimeContext context) {
        return context.getInputValue().getValue(symbol);
    }

    @Override
    public Object evaluate(RuntimeContextBuilder.DALRuntimeContext context) {
        return evaluateDataObject(context).getInstance();
    }

    public enum Type {
        SYMBOL {
            @Override
            public String inspect(Object symbol) {
                return (String) symbol;
            }
        }, BRACKET {
            @Override
            public String inspect(Object symbol) {
                return symbol instanceof String ? String.format("['%s']", symbol) : String.format("[%s]", symbol);
            }
        };

        abstract public String inspect(Object symbol);
    }

    public static SymbolNode symbolNode(Token token) {
        return new SymbolNode(token.getContent(), Type.SYMBOL);
    }

    public static SymbolNode bracketSymbolNode(DALNode node) {
        return new SymbolNode(((ConstNode) node).getValue(), Type.BRACKET);
    }
}
