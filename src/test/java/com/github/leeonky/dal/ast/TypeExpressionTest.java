package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import java.time.Instant;

import static org.assertj.core.api.Assertions.assertThat;

class TypeExpressionTest {
    RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

    @Test
    void unexpected_data_type_should_return_false() {
        TypeExpression typeExpression = new TypeExpression(new ConstNode(1), new SchemaNode("String"));

        assertThat(typeExpression.evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(false);
    }

    @Test
    void should_return_true_when_type_matches() {
        TypeExpression typeExpression = new TypeExpression(new ConstNode(1), new SchemaNode("Integer"));

        assertThat(typeExpression.evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(true);
    }

    @Test
    void support_return_value_as_schema_type() {
        TypeExpression typeExpression = new TypeExpression(new ConstNode("2000-10-10T00:00:00Z"), new SchemaNode("Instant"));

        typeExpression.evaluate(runtimeContextBuilder.build(null));

        assertThat(typeExpression.getTypeInstance())
                .isEqualTo(Instant.parse("2000-10-10T00:00:00Z"));
    }

    @Test
    void support_return_schema_name() {
        TypeExpression typeExpression = new TypeExpression(new ConstNode(1), new SchemaNode("Integer"));

        assertThat(typeExpression.getSchemaName()).isEqualTo("Integer");
    }
}