package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeException;

import java.util.Collections;
import java.util.List;

import static java.lang.String.format;

public class SymbolNode extends DALNode implements ExcuteableNode {
    protected final Object symbol;
    protected final Type type;

    public SymbolNode(Object symbol, Type type) {
        this.symbol = symbol;
        this.type = type;
    }

    @Override
    public String inspect() {
        return type.inspect(symbol);
    }

    @Override
    public Data getPropertyValue(Data data) {
        if (data.isNull())
            throw new RuntimeException("Instance is null", getPositionBegin());
        try {
            return data.getValue(symbol);
        } catch (IndexOutOfBoundsException ex) {
            throw new RuntimeException("Index out of bounds (" + ex.getMessage() + ")", getPositionBegin());
        } catch (Exception e) {
            throw new RuntimeException(format("Get property `%s` failed, property can be:\n" +
                    "  1. public field\n" +
                    "  2. public getter\n" +
                    "  3. public no args method\n" +
                    "  4. Map key value\n" +
                    "  5. customized type getter\n" +
                    "  6. static method extension\n" +
                    e.getMessage(), symbol), getPositionBegin());
        }
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

    @Override
    public List<Object> propertyChain() {
        return Collections.singletonList(symbol);
    }

    @Override
    public Object getRootSymbolName() {
        return symbol;
    }
}
