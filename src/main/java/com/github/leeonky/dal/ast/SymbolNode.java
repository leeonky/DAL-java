package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.Flatten;
import com.github.leeonky.dal.runtime.PropertyAccessException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Collections;
import java.util.List;

import static java.lang.String.format;

public class SymbolNode extends DALNode implements ExcuteableNode {
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
    public Data getPropertyValue(Data data, RuntimeContextBuilder.DALRuntimeContext context) {
        try {
            if (data.getInstance() instanceof Flatten)
                context.appendFlattenProperty(data, symbol);
            Data value = data.getValue(symbol);
            if (value.getInstance() instanceof Flatten)
                context.setFlattenProperty(data, symbol, value);
            return value;
        } catch (PropertyAccessException e) {
            throw e.toDalError("", getPositionBegin());
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
                return symbol instanceof String ? format("['%s']", symbol) : format("[%s]", symbol);
            }
        }, STRING {
            @Override
            public String inspect(Object symbol) {
                return format("'%s'", symbol);
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
