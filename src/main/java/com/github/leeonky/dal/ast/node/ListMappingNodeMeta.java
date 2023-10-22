package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.MetaData;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class ListMappingNodeMeta extends ListMappingNode {
    public ListMappingNodeMeta(DALNode symbolNode) {
        super(symbolNode);
    }

    @Override
    public Data getValue(DALNode left, RuntimeContextBuilder.DALRuntimeContext context) {
        return context.wrap(left.evaluateData(context).requireList(getPositionBegin()).listMap(item -> {
            MetaData metaData = new MetaData(new ConstNode(item.getInstance()), this, context);
            return context.fetchMetaFunction(metaData).apply(metaData);
        }));
    }
}
