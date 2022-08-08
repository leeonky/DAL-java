package com.github.leeonky.dal.runtime;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

class StaticCurryingMethod extends InstanceCurryingMethod {
    public StaticCurryingMethod(Object instance, Method method) {
        super(instance, method);
    }

    @Override
    protected List<Object> args() {
        return new ArrayList<Object>() {{
            add(instance);
            addAll(StaticCurryingMethod.super.args());
        }};
    }

    @Override
    protected InstanceCurryingMethod clone() {
        return new StaticCurryingMethod(instance, method);
    }
}
