package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import static com.github.leeonky.dal.runtime.ExpressionException.opt2;

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
        return opt2(data::list).listMap(symbolNode.getRootSymbolName());
    }

    @Override
    public Object getRootSymbolName() {
        return symbolNode.getRootSymbolName();
    }
}
