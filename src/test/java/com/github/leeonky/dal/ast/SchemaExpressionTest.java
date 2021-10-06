package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import java.time.Instant;

import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SchemaExpressionTest {
    RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

    @Test
    void unexpected_data_type_should_raise_error() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode(1), singletonList((SchemaNode) new SchemaNode("String").setPositionBegin(100)));

        assertThat(assertThrows(AssertionFailure.class, () ->
                schemaExpression.evaluate(runtimeContextBuilder.build(null))))
                .hasMessage("expect matches schema `String` but was not")
                .hasFieldOrPropertyWithValue("position", 100)
        ;
    }

    @Test
    void should_return_true_when_type_matches() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode(1), singletonList(new SchemaNode("Integer")));

        assertThat(schemaExpression.evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(true);
    }

    @Test
    void support_return_value_as_schema_type() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode("2000-10-10T00:00:00Z"), singletonList(new SchemaNode("Instant")));

        schemaExpression.evaluate(runtimeContextBuilder.build(null));

        assertThat(schemaExpression.getTypeInstance())
                .isEqualTo(Instant.parse("2000-10-10T00:00:00Z"));
    }

    @Test
    void support_return_schema_name() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode(1), singletonList(new SchemaNode("Integer")));

        assertThat(schemaExpression.getSchemaName()).isEqualTo("Integer");
    }

    @Test
    void record_schema_position_in_parsing() {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () ->
                new SchemaExpression(new ConstNode(1), singletonList((SchemaNode) new SchemaNode("Unknown").setPositionBegin(5)))
                        .evaluate(new RuntimeContextBuilder().build(null)));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", 5);
    }
}