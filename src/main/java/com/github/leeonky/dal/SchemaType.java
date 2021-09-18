package com.github.leeonky.dal;

import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.dal.util.CodeHelper;
import com.github.leeonky.util.BeanClass;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static java.util.Arrays.asList;

public class SchemaType {
    public final BeanClass<?> schema;
    private final Object fromProperty;
    private final SchemaType parent;

    private SchemaType(BeanClass<?> schema) {
        this(schema, null, null);
    }

    private SchemaType(BeanClass<?> schema, Object fromProperty, SchemaType parent) {
        this.schema = schema;
        this.fromProperty = fromProperty;
        this.parent = parent;
    }

    public static SchemaType create(BeanClass<?> schema) {
        return new SchemaType(schema);
    }

    public static SchemaType createRoot() {
        return new SchemaType(null);
    }

    private String fetchFieldChain(String name) {
        return allAliases().stream().filter(fieldAlias -> fieldAlias.alias().equals(name))
                .map(FieldAlias::field).findFirst().orElse(name);
    }

    private List<FieldAlias> allAliases() {
        return collectAlias(schema == null ? null : schema.getType());
    }

    private List<FieldAlias> collectAlias(Class<?> type) {
        List<FieldAlias> aliases = new ArrayList<>();
        if (type != null) {
            FieldAliases fieldAliases = type.getAnnotation(FieldAliases.class);
            if (fieldAliases != null)
                aliases.addAll(asList(fieldAliases.value()));
            aliases.addAll(collectAlias(type.getSuperclass()));
            for (Class<?> interfaceType : type.getInterfaces())
                aliases.addAll(collectAlias(interfaceType));
        }
        return aliases;
    }

    public SchemaType access(Object alias) {
        if (alias instanceof Integer)
            return subSchema(alias);
        String property = fetchFieldChain((String) alias);
        if (Objects.equals(property, alias))
            return subSchema(property);
        List<Object> chain = CodeHelper.INSTANCE.toChainNodes(property);
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

    public SchemaType mappingAccess(Object property) {
        return new SchemaType(null) {
            @Override
            public SchemaType access(Object alias) {
                return new SchemaType(SchemaType.this.access(alias).access(property).schema, alias, this);
            }
        };
    }

    public Object firstFieldFromAlias(Object alias) {
        return access(alias).getPropertyChainBefore(this).get(0);
    }
}
