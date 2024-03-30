package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.PartialObject;
import com.github.leeonky.dal.runtime.PropertyAccessException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Collections;
import java.util.List;

import static java.lang.String.format;

public class SymbolNode extends DALNode implements ExecutableNode {
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
    public Data getValue(Data data, RuntimeContextBuilder.DALRuntimeContext context) {
        try {
            if (data.instance() instanceof PartialObject)
                context.appendPartialPropertyReference(data, symbol);
            Data value = data.getValue(symbol);
            if (value.instance() instanceof PartialObject)
                context.initPartialPropertyStack(data, symbol, value);
            return value;
        } catch (PropertyAccessException e) {
            throw e.toDalError("", getPositionBegin());
        }
    }

    @Override
    public List<Object> propertyChain() {
        return Collections.singletonList(symbol);
    }

    @Override
    public Object getRootSymbolName() {
        return symbol;
    }

    public enum Type {
        SYMBOL, NUMBER, BRACKET {
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

        public String inspect(Object symbol) {
            return symbol.toString();
        }
    }

    @Override
    public boolean needPrefixBlankWarningCheck() {
        return true;
    }
}
