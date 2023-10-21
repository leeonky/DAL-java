package com.github.leeonky.dal.runtime;

import java.lang.reflect.Array;

public class JavaArrayAccessor implements ArrayAccessor<Object> {
    @Override
    public Object get(Object o, int index) {
        return Array.get(o, index);
    }

    @Override
    public int size(Object o) {
        return Array.getLength(o);
    }
}
