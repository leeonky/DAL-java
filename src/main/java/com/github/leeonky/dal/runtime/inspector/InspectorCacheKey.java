package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

import java.util.Objects;

class InspectorCacheKey {
    private final Data data;

    public InspectorCacheKey(Data data) {
        this.data = data;
    }

    @Override
    public int hashCode() {
        return Objects.hash(data.getInstance());
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof InspectorCacheKey && ((InspectorCacheKey) obj).data.getInstance() == data.getInstance();
    }
}
