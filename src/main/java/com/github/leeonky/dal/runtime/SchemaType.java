package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.util.BeanClass;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static com.github.leeonky.util.function.Extension.notAllowParallelReduce;
import static java.util.Arrays.asList;

public class SchemaType {
    private final BeanClass<?> schema;
    private final Object fromProperty;
    private final SchemaType parent;
    private static final Compiler compiler = new Compiler();

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
        if (isIntegerForListIndex(alias))
            return subSchema(alias);
        String property = fetchFieldChain((String) alias);
        if (Objects.equals(property, alias))
            return subSchema(property);
        List<Object> chain = compiler.toChainNodes(property);
        return chain.stream().skip(1).reduce(access(chain.get(0)), SchemaType::access, notAllowParallelReduce());
    }

    private SchemaType subSchema(Object property) {
        try {
            if (isIntegerForListIndex(property))
                return new SchemaType(schema.getElementType(), property, this);
            else
                return new SchemaType(schema.getPropertyChainReader((String) property).getType(), property, this);
        } catch (Exception e) {
            return new SchemaType(null, property, this);
        }
    }

    private boolean isIntegerForListIndex(Object property) {
        return property instanceof Integer || property instanceof Long;
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
