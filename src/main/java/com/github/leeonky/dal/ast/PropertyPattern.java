package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class PropertyPattern extends DALNode implements ExecutableNode {
    private final DALNode symbol;

    public PropertyPattern(DALNode symbol) {
        this.symbol = symbol;
    }

    @Override
    public String inspect() {
        return symbol.inspect() + "{}";
    }

    @Override
    public Data getValue(Data data, RuntimeContextBuilder.DALRuntimeContext context) {
        String prefix = symbol.getRootSymbolName().toString();
        Data filter = data.filter(prefix);
        context.setFlattenProperty(data, prefix, filter);
        return filter;
    }
}
