package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class PropertyThis extends DALNode implements ExcuteableNode {

    @Override
    public String inspect() {
        return "{}";
    }

    @Override
    public Data getPropertyValue(Data data, RuntimeContextBuilder.DALRuntimeContext context) {
        return data;
    }
}
