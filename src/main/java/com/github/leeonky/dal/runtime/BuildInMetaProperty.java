package com.github.leeonky.dal.runtime;

import static java.lang.String.format;

public class BuildInMetaProperty {

    public static Object size(MetaData metaData) {
        Data data = metaData.evaluateInput();
        if (data.isList())
            return data.getListSize();
        throw new IllegalStateException(format("Invalid meta property `size` for: %s", data.inspect()));
    }
}
