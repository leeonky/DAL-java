package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.util.BeanClass;

public class Expect {
    private final BeanClass<Object> type;
    private final Object expect;

    public Expect(BeanClass<Object> type, Object expect) {
        this.type = type;
        this.expect = expect;
    }

    public BeanClass<Object> getType() {
        return type;
    }

    public Object getExpect() {
        return expect;
    }

    public boolean isSchema() {
        return false;
    }
}
