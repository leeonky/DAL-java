package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ElementAccessException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeException;

import static com.github.leeonky.util.BeanClass.createFrom;
import static java.lang.String.format;

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
            if (data.isList())
                return data.mapList(symbolNode.getRootSymbolName());
            throw new RuntimeException(format("The instance of '%s' is not a list",
                    createFrom(data.getInstance()).getName()), getPositionBegin());
        } catch (ElementAccessException e) {
            throw e.toDalError(symbolNode.getPositionBegin());
        }
    }
}
