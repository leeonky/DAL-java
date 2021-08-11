package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Token.*;
import static org.assertj.core.api.Assertions.assertThat;

class SingleEvaluableNodeFactoryTest extends NodeFactoryTestBase {

    @Override
    protected NodeFactory getDefaultNodeFactory() {
        return NodeFactory.createSingleEvaluableNodeFactory();
    }

    @Test
    void raise_error_when_not_matches() {
        assertThat(invalidSyntaxToken(givenToken(operatorToken("+"), 100)))
                .hasFieldOrPropertyWithValue("position", 100)
                .hasMessage("expect a value");
    }

    @Test
    void raise_error_when_no_token() {
        assertThat(invalidSyntaxToken(givenToken()))
                .hasFieldOrPropertyWithValue("position", 0)
                .hasMessage("expect a value");
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
}
