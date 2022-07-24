package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.BeanClass;

class AutoMappingListPropertyAccessor extends JavaClassPropertyAccessor<AutoMappingList> {
    public AutoMappingListPropertyAccessor(RuntimeContextBuilder runtimeContextBuilder) {
        super(runtimeContextBuilder, BeanClass.create(AutoMappingList.class));
    }

    @Override
    public Object getValueByData(Data data, Object property) {
        return data.listMap(item -> item.getValue(property).getInstance());
    }
}
