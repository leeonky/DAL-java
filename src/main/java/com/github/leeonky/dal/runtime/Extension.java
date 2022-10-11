package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.util.BeanClass;

public interface Extension {
    void extend(DAL dal);

    default int order() {
        return BeanClass.createFrom(this).annotation(Order.class).map(Order::value).orElse(Integer.MAX_VALUE);
    }
}
