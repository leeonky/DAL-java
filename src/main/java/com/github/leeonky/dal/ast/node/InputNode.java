package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Collections;
import java.util.List;

public class InputNode extends DALNode {
    public static final InputNode INPUT_NODE = new InputNode();

    @Override
    public Data evaluateData(RuntimeContextBuilder.DALRuntimeContext context) {
        return context.getThis();
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
