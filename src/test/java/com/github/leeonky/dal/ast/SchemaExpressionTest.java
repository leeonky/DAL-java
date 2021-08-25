package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContextBuilder;
import com.github.leeonky.dal.RuntimeException;
import org.junit.jupiter.api.Test;

import java.time.Instant;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SchemaExpressionTest {
    RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

    @Test
    void unexpected_data_type_should_return_false() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode(1), new SchemaNode("String"));

        assertThat(schemaExpression.evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(false);
    }

    @Test
    void should_return_true_when_type_matches() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode(1), new SchemaNode("Integer"));

        assertThat(schemaExpression.evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(true);
    }

    @Test
    void support_return_value_as_schema_type() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode("2000-10-10T00:00:00Z"), new SchemaNode("Instant"));

        schemaExpression.evaluate(runtimeContextBuilder.build(null));

        assertThat(schemaExpression.getTypeInstance())
                .isEqualTo(Instant.parse("2000-10-10T00:00:00Z"));
    }

    @Test
    void support_return_schema_name() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode(1), new SchemaNode("Integer"));

        assertThat(schemaExpression.getSchemaName()).isEqualTo("Integer");
    }

    @Test
    void record_schema_position_in_parsing() {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () ->
                new SchemaExpression(new ConstNode(1),
                        (SchemaNode) new SchemaNode("Unknown").setPositionBegin(5))
                        .evaluate(new RuntimeContextBuilder().build(null)));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", 5);
    }
}