package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.format.Value;
import com.github.leeonky.util.BeanClass;

import java.util.Map;

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

    public boolean isFormatter() {
        return Formatter.class.isAssignableFrom(((BeanClass<?>) getType()).getType());
    }

    public boolean isCollection() {
        return getType().isCollection();
    }

    public boolean isMap() {
        return Map.class.isAssignableFrom(((BeanClass<?>) getType()).getType());
    }

    public boolean isSchemaValue() {
        return Value.class.isAssignableFrom(((BeanClass<?>) getType()).getType());
    }

    public boolean isSchemaType() {
        return Type.class.isAssignableFrom(((BeanClass<?>) getType()).getType());
    }
}
