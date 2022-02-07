package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.ConstructorViaSchema;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeException;

public class SchemaNode extends DALNode {
    private final String schema;

    public SchemaNode(String schema) {
        this.schema = schema;
    }

    public ConstructorViaSchema getConstructorViaSchema(RuntimeContextBuilder.DALRuntimeContext context) {
        return context.searchConstructor(schema).orElseThrow(() ->
                new RuntimeException("Schema '" + schema + "' not registered", getPositionBegin()));
    }

    @Override
    public String inspect() {
        return schema;
    }
}
