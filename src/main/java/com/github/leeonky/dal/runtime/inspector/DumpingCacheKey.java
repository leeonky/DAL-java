package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;

import java.util.Objects;

class DumpingCacheKey {
    private final Data data;

    public DumpingCacheKey(Data data) {
        this.data = data;
    }

    @Override
    public int hashCode() {
        return Objects.hash(data.instance());
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof DumpingCacheKey && ((DumpingCacheKey) obj).data.instance() == data.instance();
    }
}
