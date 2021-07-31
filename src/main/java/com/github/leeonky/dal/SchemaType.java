package com.github.leeonky.dal;

import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.util.BeanClass;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import static java.util.Arrays.asList;

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

    //TODO nested is list element
    private String fetchFieldChain(String name) {
        return allAliases().stream().filter(fieldAlias -> fieldAlias.alias().equals(name))
                .map(FieldAlias::field).findFirst().orElse(name);
    }

    private List<FieldAlias> allAliases() {
        List<FieldAlias> aliases = new ArrayList<>();
        FieldAliases fieldAliases = schema.getAnnotation(FieldAliases.class);
        if (fieldAliases != null)
            aliases.addAll(asList(fieldAliases.value()));
        return aliases;
    }

    //TODO nested in List element
    public List<String> transformToFieldChain(LinkedList<String> aliases) {
        if (schema == null)
            return aliases;
        String fieldChain = fetchFieldChain(aliases.pop());
        //TODO field lead to another alias
        //TODO part of field chain is another alias
        List<String> result = new ArrayList<>();
        result.add(fieldChain);
        if (!aliases.isEmpty())
            result.addAll(getFieldSchema(fieldChain).transformToFieldChain(aliases));
        return result;
    }
}
