package com.github.leeonky.dal;

import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.util.BeanClass;
import com.github.leeonky.util.PropertyReader;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class SchemaType {
    public final Class<?> schema;

    public SchemaType(Class<?> schema) {
        this.schema = schema;
    }

    public SchemaType getFieldSchema(String field) {
        PropertyReader<?> propertyReader = null;
        if (schema != null) {
            //TODO result 0 may be a chain
            propertyReader = BeanClass.create(schema).getPropertyReaders().get(field);
        }
        return propertyReader == null ? new SchemaType(null) : new SchemaType(propertyReader.getTypeClass());
    }

    private String fetchFieldChain(String name) {
        Class<?> schema = this.schema;
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

    //TODO move logic to object
    //TODO nested schema
    //TODO nested in List element
    //TODO schemaType class maybe null
    //TODO maybe no next level property
    public List<String> transformToFieldChain(LinkedList<String> aliases) {
        if (aliases.isEmpty())
            return Collections.emptyList();
        String fieldChain = fetchFieldChain(aliases.pop());
        //TODO fieldChain maybe a chain
        return new ArrayList<String>() {{
            add(fieldChain);
            addAll(getFieldSchema(fieldChain).transformToFieldChain(aliases));
        }};
    }
}
