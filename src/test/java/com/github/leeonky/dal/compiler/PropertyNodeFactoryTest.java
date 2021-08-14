package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class PropertyNodeFactoryTest extends NodeFactoryTestBase {

    @Override
    protected NodeFactory getDefaultNodeFactory() {
        return NodeFactory.createBracketPropertyNodeFactory();
    }

    @Test
    void matches_access_array_and_return_node() {
        Node node = givenCode("[0]").fetchNode();

        assertThat(node)
                .isInstanceOf(PropertyNode.class)
                .hasFieldOrPropertyWithValue("name", 0);
        assertThat(node.inspect()).isEqualTo("[0]");
    }

    @Test
    void matches_bracket_property_and_return_node() {
        Node node = givenCode("['a']").fetchNode();

        assertThat(node)
                .isInstanceOf(PropertyNode.class)
                .hasFieldOrPropertyWithValue("name", "a");
        assertThat(node.inspect()).isEqualTo("['a']");
    }

    @Test
    void do_not_allow_get_value_when_no_value() {
        assertThat(invalidSyntaxToken(givenCode("[]")))
                .hasMessage("should given one property or array index in `[]`")
                .hasFieldOrPropertyWithValue("position", 1);
    }

    @Test
    void should_raise_error_when_unexpected_token() {
        assertThat(invalidSyntaxToken(givenCode("[+]")))
                .hasMessage("should given one property or array index in `[]`")
                .hasFieldOrPropertyWithValue("position", 1);
    }

    @Test
    void do_not_allow_more_than_one_sub_token() {
        assertThat(invalidSyntaxToken(givenCode("[1 2]")))
                .hasMessage("should given one property or array index in `[]`")
                .hasFieldOrPropertyWithValue("position", 3);
    }

    @Test
    void do_not_allow_get_value_when_not_finished_1() {
        assertThat(invalidSyntaxToken(givenCode("[1")))
                .hasMessage("should end with `]`")
                .hasFieldOrPropertyWithValue("position", 2);
    }

    @Test
    void do_not_allow_get_value_when_not_finished_2() {
        assertThat(invalidSyntaxToken(givenCode("[")))
                .hasMessage("should end with `]`")
                .hasFieldOrPropertyWithValue("position", 1);
    }
}