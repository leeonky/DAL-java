package com.github.leeonky.dal.runtime;

public class BuildInMetaProperty {

    public static Object size(MetaData metaData) {
        Data data = metaData.evaluateInput();
        if (data.isList())
            return data.getListSize();
        throw new RuntimeException("Input value is not list, only List support `size` meta property",
                metaData.getMetaDataNode().getPositionBegin());
    }

}
