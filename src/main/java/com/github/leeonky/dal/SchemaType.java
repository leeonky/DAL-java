package com.github.leeonky.dal;

import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.util.BeanClass;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

import static java.util.Arrays.asList;

public class SchemaType {
    public final BeanClass<?> schema;
    private final Object fromProperty;
    private final SchemaType parent;

    public SchemaType(BeanClass<?> schema) {
        this(schema, null, null);
    }

    public SchemaType(BeanClass<?> schema, Object fromProperty, SchemaType parent) {
        this.schema = schema;
        this.fromProperty = fromProperty;
        this.parent = parent;
    }

    private SchemaType getFieldSchema(String field) {
        try {
            return new SchemaType(schema.getPropertyChainReader(field).getType());
        } catch (Exception ignore) {
            return new SchemaType(null);
        }
    }

    private String fetchFieldChain(String name) {
        return allAliases().stream().filter(fieldAlias -> fieldAlias.alias().equals(name))
                .map(FieldAlias::field).findFirst().orElse(name);
    }

    private List<FieldAlias> allAliases() {
        List<FieldAlias> aliases = new ArrayList<>();
        if (schema != null) {
            FieldAliases fieldAliases = schema.getType().getAnnotation(FieldAliases.class);
            if (fieldAliases != null)
                aliases.addAll(asList(fieldAliases.value()));
        }
        return aliases;
    }

    //TODO nested in List element
    //TODO to be removed
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

    public SchemaType access(Object alias) {
        if (alias instanceof Integer)
            return subSchema(alias);
        String property = fetchFieldChain((String) alias);
        if (Objects.equals(property, alias))
            return subSchema(property);
        List<Object> chain = BeanClass.toChainNodes(property);
        return chain.stream().skip(1).reduce(access(chain.get(0)), SchemaType::access, (o1, o2) -> {
            throw new IllegalStateException("Not allow parallel here!");
        });
    }

    private SchemaType subSchema(Object property) {
        try {
            if (property instanceof Integer)
                return new SchemaType(schema.getElementType(), property, this);
            else
                return new SchemaType(schema.getPropertyChainReader((String) property).getType(), property, this);
        } catch (Exception e) {
            return new SchemaType(null, property, this);
        }
    }

    public List<Object> getPropertyChainBefore(SchemaType schemaOrder) {
        if (schemaOrder == this)
            return new ArrayList<>();
        List<Object> chain = parent.getPropertyChainBefore(schemaOrder);
        chain.add(fromProperty);
        return chain;
    }
}
