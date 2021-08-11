package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class TypeWhichExpressionTest {
    private static final TypeExpression NON_MATCH_TYPE_EXPRESSION = new TypeExpression(new ConstNode(1), new SchemaNode("String"));
    private static final TypeExpression MATCHES_TYPE_EXPRESSION = new TypeExpression(new ConstNode(1), new SchemaNode("Integer"));
    RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

    @Test
    void return_false_when_type_not_matches_or_clause_is_false() {
        assertThat(NON_MATCH_TYPE_EXPRESSION.which(new ConstNode(true)).evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(false);

        assertThat(NON_MATCH_TYPE_EXPRESSION.which(new ConstNode(false)).evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(false);

        assertThat(MATCHES_TYPE_EXPRESSION.which(new ConstNode(false)).evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(false);
    }

    @Test
    void should_return_true_when_both_type_matches_and_clause_is_true() {
        assertThat(MATCHES_TYPE_EXPRESSION.which(new ConstNode(true)).evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(true);
    }

    @Test
    void should_wrapper_object_as_target_type() {

        TypeWhichExpression typeWhichExpression = new TypeExpression(new ConstNode("http://www.baidu.com"),
                new SchemaNode("URL")).which(new PropertyNode(InputNode.INSTANCE, "protocol"));

        assertThat(typeWhichExpression.evaluate(runtimeContextBuilder.build(null))).isEqualTo("http");
    }
}