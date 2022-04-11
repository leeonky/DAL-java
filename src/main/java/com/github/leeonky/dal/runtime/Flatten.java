package com.github.leeonky.dal.runtime;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public interface Flatten {
    default List<String> removeExpectedFields(Set<String> fields, Object symbol, Object property) {
//        TODO refactor rule
        //        TODO expectedFields should not more than one
        return fields.stream().filter(field -> (String.valueOf(symbol) + property)
                .equalsIgnoreCase(field)).collect(Collectors.toList());
    }
}