package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.BracketNode;
import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Token.constValueToken;
import static com.github.leeonky.dal.token.Token.propertyToken;
import static org.assertj.core.api.Assertions.assertThat;

class SingleEvaluableNodeFactoryTest extends NodeFactoryTestBase {

    @Override
    protected NodeFactory getDefaultNodeFactory() {
        return NodeFactory.createSingleEvaluableNodeFactory();
    }

    @Test
    void raise_error_when_not_matches() {
        GivenCode givenCode = givenCode("1 +");
        givenCode.fetchNode();

        assertThat(invalidSyntaxToken(givenCode))
                .hasFieldOrPropertyWithValue("position", 2)
                .hasMessage("expect a value or expression");
    }

    @Test
    void raise_error_when_no_token() {
        GivenCode givenCode = givenCode("1");
        givenCode.fetchNode();

        assertThat(invalidSyntaxToken(givenCode))
                .hasFieldOrPropertyWithValue("position", 1)
                .hasMessage("expect a value or expression");
    }

    @Test
    void support_const_value() {
        assertThat(fetchNodeWhenGivenToken(constValueToken("const string"), 10))
                .isInstanceOf(ConstNode.class)
                .hasFieldOrPropertyWithValue("value", "const string")
                .hasFieldOrPropertyWithValue("positionBegin", 10);
    }

    @Test
    void support_access_property() {
        Node node = fetchNodeWhenGivenToken(propertyToken("name"), 10);

        assertThat(node)
                .isInstanceOf(PropertyNode.class)
                .hasFieldOrPropertyWithValue("name", "name")
                .hasFieldOrPropertyWithValue("positionBegin", 10);
        assertThat(node.inspect()).isEqualTo(".name");
    }


    @Test
    void recursive_property_node() {
        Node node = givenToken(propertyToken("order"))
                .givenToken(propertyToken("user"))
                .givenToken(propertyToken("name"), 14)
                .fetchNode();

        assertThat(node).isInstanceOf(PropertyNode.class)
                .hasFieldOrPropertyWithValue("name", "name")
                .hasFieldOrPropertyWithValue("positionBegin", 14);
        assertThat(node.inspect()).isEqualTo(".order.user.name");
    }

    @Test
    void recursive_property_node_after_other_evaluable_node() {
        assertThat(givenToken(constValueToken("string"))
                .givenToken(propertyToken("empty"))
                .fetchNode().inspect()).isEqualTo("'string'.empty");
    }

    @Test
    void support_bracket_node_with_single_value() {
        Node node = givenCode("(2)").fetchNode();

        assertThat(node).isInstanceOf(BracketNode.class);
        assertThat(node.inspect()).isEqualTo("(2)");
    }

    @Test
    void support_bracket_node_with_expression() {
        Node node = givenCode("(1+1)").fetchNode();

        assertThat(node).isInstanceOf(BracketNode.class);
        assertThat(node.inspect()).isEqualTo("(1 + 1)");
    }
}
