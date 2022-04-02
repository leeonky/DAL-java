package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;

public class ListMappingNode extends DALNode implements ExcuteableNode {
    private final SymbolNode symbolNode;

    public ListMappingNode(DALNode symbolNode) {
        this.symbolNode = (SymbolNode) symbolNode;
    }

    @Override
    public String inspect() {
        return symbolNode.inspect() + "[]";
    }

    @Override
    public Data getPropertyValue(Data data) {
        return symbolNode.getPropertyValue(data);
    }
}
