package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.BuildInMetaProperty;
import com.github.leeonky.dal.runtime.Extension;

public class MetaProperties implements Extension {
    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder()
                .registerMetaProperty("size", BuildInMetaProperty::size)
                .registerMetaProperty("throw", BuildInMetaProperty::throw_);
    }
}
