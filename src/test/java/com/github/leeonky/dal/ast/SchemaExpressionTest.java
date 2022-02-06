package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeException;
import org.junit.jupiter.api.Test;

import java.math.BigInteger;
import java.time.Instant;

import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SchemaExpressionTest {
    RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

    @Test
    void unexpected_data_type_should_raise_error() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode(1),
                singletonList((SchemaNodeBak) new SchemaNodeBak("String").setPositionBegin(100)), 0);

        assertThat(assertThrows(AssertionFailure.class, () ->
                schemaExpression.evaluate(runtimeContextBuilder.build(null))))
                .hasMessage("Expecting 1 to match schema `String` but was not")
                .hasFieldOrPropertyWithValue("position", 100)
        ;
    }

    @Test
    void should_return_instance_when_type_matches() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode(1),
                singletonList(new SchemaNodeBak("Integer")), 0);

        assertThat(schemaExpression.evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(BigInteger.valueOf(1));
    }

    @Test
    void support_return_value_as_schema_type() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode("2000-10-10T00:00:00Z"),
                singletonList(new SchemaNodeBak("Instant")), 0);

        assertThat(schemaExpression.evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(Instant.parse("2000-10-10T00:00:00Z"));
    }

    @Test
    void support_return_schema_name() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode(1),
                singletonList(new SchemaNodeBak("Integer")), 0);

        assertThat(schemaExpression.getSchemaName()).isEqualTo("Integer");
    }

    @Test
    void record_schema_position_in_parsing() {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () ->
                new SchemaExpression(new ConstNode(1),
                        singletonList((SchemaNodeBak) new SchemaNodeBak("Unknown").setPositionBegin(5)), 0)
                        .evaluate(new RuntimeContextBuilder().build(null)));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", 5);
    }
}