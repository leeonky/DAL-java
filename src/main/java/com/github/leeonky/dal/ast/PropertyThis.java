package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.ThisObject;

public class PropertyThis extends DALNode implements ExcuteableNode {

    @Override
    public String inspect() {
        return "{}";
    }

    @Override
    public Data getPropertyValue(Data data, RuntimeContextBuilder.DALRuntimeContext context) {
        Data wrap = context.wrap(new ThisObject(data));
        context.setFlattenProperty(data, "", wrap);
        return wrap;
    }
}
