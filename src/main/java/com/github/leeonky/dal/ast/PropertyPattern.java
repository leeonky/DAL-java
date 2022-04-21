package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class PropertyPattern extends DALNode implements ExcuteableNode {
    private final DALNode symbol;

    public PropertyPattern(DALNode symbol) {
        this.symbol = symbol;
    }

    @Override
    public String inspect() {
        return symbol.inspect() + "{}";
    }

    @Override
    public Data getPropertyValue(Data data, RuntimeContextBuilder.DALRuntimeContext context) {
        String prefix = symbol.getRootSymbolName().toString();
        return data.filter(prefix);
    }
}
