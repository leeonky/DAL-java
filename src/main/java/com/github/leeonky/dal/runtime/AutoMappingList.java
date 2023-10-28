package com.github.leeonky.dal.runtime;

import java.util.function.Function;

public class AutoMappingList extends DALCollection.Decorated<Object> {
    public AutoMappingList(Function<Data, Object> mapper, DALCollection<Data> list) {
        super(list.map((index, data) -> {
            try {
                return mapper.apply(data);
            } catch (PropertyAccessException e) {
                throw new ListMappingElementAccessException(index, e);
            } catch (Exception e) {
                throw new ListMappingElementAccessException(index, new PropertyAccessException(e.getMessage(), e));
            }
        }));
    }
}
