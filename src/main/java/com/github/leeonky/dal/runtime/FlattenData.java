package com.github.leeonky.dal.runtime;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

//TODO rename to Flatten
public interface FlattenData {
    default void removeExpectedFields(Set<String> fields, Object symbol, Object property) {
        List<String> expectedFields = fields.stream().filter(field -> (String.valueOf(symbol) + property)
                .equalsIgnoreCase(field)).collect(Collectors.toList());
//        TODO expectedFields should not more than one
        expectedFields.forEach(fields::remove);
    }
}