package com.github.leeonky.dal.runtime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.function.Function;

@Deprecated
//unlimited list
public class AutoMappingList extends ArrayList<Object> {
    private final int firstIndex;

    public <T> AutoMappingList(int firstIndex, Collection<T> collection, Function<T, Object> mapper) {
        this.firstIndex = firstIndex;
        collection.forEach(obj -> {
            try {
                add(mapper.apply(obj));
            } catch (PropertyAccessException e) {
                throw new ElementAccessException(size() + this.firstIndex, e);
            } catch (Exception e) {
                throw new ElementAccessException(size() + this.firstIndex, new PropertyAccessException(e.getMessage(), e));
            }
        });
    }

    public int firstIndex() {
        return firstIndex;
    }
}
