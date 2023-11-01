package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.runtime.AutoMappingList;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.JavaClassPropertyAccessor;
import com.github.leeonky.util.BeanClass;

class AutoMappingListPropertyAccessor extends JavaClassPropertyAccessor<AutoMappingList> {
    public AutoMappingListPropertyAccessor() {
        super(BeanClass.create(AutoMappingList.class));
    }

    @Override
    public Object getValueByData(Data data, Object property) {
        return data.list().listMap(item -> item.getValue(property).instance());
    }
}
