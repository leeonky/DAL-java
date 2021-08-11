package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.RuntimeContextBuilder;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.ast.Node;
import org.json.JSONException;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TypeExpressionTest extends NodeFactoryTestBase {

    @Override
    protected NodeFactory getDefaultNodeFactory() {
        return NodeFactory.createExpressionNodeFactory();
    }

    @Test
    void support_type_expression() {
        Node node = givenCode("1 is Integer").fetchNode();

        assertThat(node).isInstanceOf(com.github.leeonky.dal.ast.TypeExpression.class);
        assertThat(node.inspect()).isEqualTo("1 is Integer");
    }

    @Test
    void record_schema_position_in_parsing() {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () ->
                givenCode("1 is Unknown").fetchNode().evaluate(new RuntimeContextBuilder().build(null)));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", 5);
    }

    @Test
    void raise_error_when_no_schema() {
        assertThat(invalidSyntaxToken(givenCode("1 is 1")))
                .hasMessage("operand of `is` must be schema type")
                .hasFieldOrPropertyWithValue("position", 5);
    }

    @Test
    void raise_error_when_no_schema_token() {
        assertThat(invalidSyntaxToken(givenCode("1 is")))
                .hasMessage("Schema expression not finished")
                .hasFieldOrPropertyWithValue("position", 4);
    }

    @Test
    void support_multi_schema() {
        Node node = givenCode("1 is Integer | Number").fetchNode();

        assertThat(node).isInstanceOf(com.github.leeonky.dal.ast.TypeExpression.class);
        assertThat(node.inspect()).isEqualTo("1 is Integer | Number");
    }

    @Test
    void should_raise_error_when_schema_list_operator_different() throws JSONException {
        assertThat(invalidSyntaxToken(givenCode("1 is Integer | Number / Long")))
                .hasMessage("Schema operator should be consistent")
                .hasFieldOrPropertyWithValue("position", 24);
    }

    @Test
    void should_raise_error_when_schema_list_not_finished() throws JSONException {

        assertThat(invalidSyntaxToken(givenCode("1 is Integer |")))
                .hasMessage("Schema expression not finished")
                .hasFieldOrPropertyWithValue("position", 14);
    }
}
