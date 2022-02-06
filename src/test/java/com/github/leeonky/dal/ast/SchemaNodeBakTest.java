package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeException;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SchemaNodeBakTest {
    SchemaNodeBak schemaNodeBak = new SchemaNodeBak("Type");

    @Test
    void no_type_register() {
        schemaNodeBak.setPositionBegin(10);
        final RuntimeException runtimeException = assertThrows(RuntimeException.class,
                () -> schemaNodeBak.getConstructorViaSchema(new RuntimeContextBuilder().build(null)));

        assertThat(runtimeException).hasMessage("Schema 'Type' not registered")
                .hasFieldOrPropertyWithValue("position", 10);
    }
}