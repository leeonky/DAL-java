package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

public interface InspectorFactory {
    default boolean matches(Data data) {
        return true;
    }

    Inspector create(Data data);
}
