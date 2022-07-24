package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.InvocationException;

import static java.lang.String.format;

public class BuildInMetaProperty {

    public static Object size(MetaData metaData) {
        Data data = metaData.evaluateInput();
        if (data.isList())
            return data.getListSize();
        throw new IllegalStateException(format("Invalid meta property `size` for: %s", data.inspect()));
    }

    public static Object throw_(MetaData metaData) {
        try {
            metaData.evaluateInput();
            return null;
        } catch (RuntimeException e) {
            if (e.getCause() instanceof InvocationException)
                return e.getCause().getCause();
            throw e;
        }
    }
}
