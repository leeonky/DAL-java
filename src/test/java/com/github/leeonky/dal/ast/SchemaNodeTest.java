package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContextBuilder;
import com.github.leeonky.dal.RuntimeException;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SchemaNodeTest {

    @Test
    void no_type_register() {
        SchemaNode schemaNode = new SchemaNode("Type");
        schemaNode.setPositionBegin(10);
        final RuntimeException runtimeException = assertThrows(RuntimeException.class,
                () -> schemaNode.evaluate(new RuntimeContextBuilder().build(null)));

        assertThat(runtimeException).hasMessage("Schema 'Type' not registered")
                .hasFieldOrPropertyWithValue("position", 10);
    }
}