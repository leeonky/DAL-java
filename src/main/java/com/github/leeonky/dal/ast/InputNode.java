package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Collections;
import java.util.List;

public class InputNode extends DALNode {
    public static final InputNode INSTANCE = new InputNode();

    @Override
    public Data evaluateData(RuntimeContextBuilder.DALRuntimeContext context) {
        return context.currentStack().getData();
    }

    @Override
    public String inspect() {
        return "";
    }

    @Override
    public List<Object> propertyChain() {
        return Collections.emptyList();
    }
}
