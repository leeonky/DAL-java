package com.github.leeonky.dal;

import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.util.BeanClass;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class SchemaType {
    public final Class<?> schema;

    public SchemaType(Class<?> schema) {
        this.schema = schema;
    }

    private SchemaType getFieldSchema(String field) {
        try {
            return new SchemaType(BeanClass.create(schema).getPropertyChainReader(field).getTypeClass());
        } catch (Exception ignore) {
            return new SchemaType(null);
        }
    }

    private String fetchFieldChain(String name) {
        if (schema != null) {
            FieldAliases fieldAliases = schema.getAnnotation(FieldAliases.class);
            if (fieldAliases != null)
                for (FieldAlias fieldAlias : fieldAliases.value()) {
                    if (fieldAlias.alias().equals(name)) {
                        return fieldAlias.field();
                    }
                }
        }
        return name;
    }

    //TODO nested in List element
    public List<String> transformToFieldChain(LinkedList<String> aliases) {
        String fieldChain = fetchFieldChain(aliases.pop());
        List<String> result = new ArrayList<>();
        result.add(fieldChain);
        if (!aliases.isEmpty())
            result.addAll(getFieldSchema(fieldChain).transformToFieldChain(aliases));
        return result;
    }
}
