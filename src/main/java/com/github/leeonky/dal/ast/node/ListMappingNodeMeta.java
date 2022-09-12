package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.ElementAccessException;
import com.github.leeonky.dal.runtime.MetaData;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.function.Function;

public class ListMappingNodeMeta extends ListMappingNode {
    public ListMappingNodeMeta(DALNode symbolNode) {
        super(symbolNode);
    }

    @Override
    public Data getValue(DALNode left, RuntimeContextBuilder.DALRuntimeContext context) {
        Function<MetaData, Object> function = context.fetchMetaFunction(this);
        try {
            return context.wrap(left.evaluateData(context).requireList(getPositionBegin()).listMap(item ->
                    function.apply(new MetaData(new ConstNode(item.getInstance()), this, context))));
        } catch (ElementAccessException e) {
            throw e.toDalError(getPositionBegin());
        }
    }
}
