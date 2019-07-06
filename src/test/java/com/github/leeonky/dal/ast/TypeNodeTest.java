package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContextBuilder;
import com.github.leeonky.dal.RuntimeException;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TypeNodeTest {

    @Test
    void no_type_register() {
        TypeNode typeNode = new TypeNode("Type");
        typeNode.setPositionBegin(10);
        final RuntimeException runtimeException = assertThrows(RuntimeException.class,
                () -> typeNode.evaluate(new CompilingContextBuilder().build(null)));

        assertThat(runtimeException).hasMessage("Schema 'Type' not registered")
                .hasFieldOrPropertyWithValue("position", 10);
    }
}