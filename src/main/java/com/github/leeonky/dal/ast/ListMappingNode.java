package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ElementAccessException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class ListMappingNode extends DALNode implements ExecutableNode {
    private final SymbolNode symbolNode;

    public ListMappingNode(DALNode symbolNode) {
        this.symbolNode = (SymbolNode) symbolNode;
        setPositionBegin(symbolNode.getPositionBegin());
    }

    @Override
    public String inspect() {
        return symbolNode.inspect() + "[]";
    }

    @Override
    public Data getValue(Data data, RuntimeContextBuilder.DALRuntimeContext context) {
        try {
            return data.requireList(getPositionBegin()).mapList(symbolNode.getRootSymbolName());
        } catch (ElementAccessException e) {
            throw e.toDalError(symbolNode.getPositionBegin());
        }
    }

    @Override
    public Object getRootSymbolName() {
        return symbolNode.getRootSymbolName();
    }
}
