package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.ParenthesesNode;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.compiler.NodeFactory.createPropertyNodeFactory;
import static com.github.leeonky.dal.token.Token.*;
import static org.assertj.core.api.Assertions.assertThat;

class SingleNodeFactoryTest {

    @Nested
    class FetchPropertyNode extends NodeFactoryTestBase {

        @Override
        protected NodeFactory getDefaultNodeFactory() {
            return createPropertyNodeFactory();
        }

        @Test
        void matches_and_return_node() {
            Node node = fetchNodeWhenGivenToken(propertyToken("name"), 10);

            assertThat(node)
                    .isInstanceOf(PropertyNode.class)
                    .hasFieldOrPropertyWithValue("name", "name")
                    .hasFieldOrPropertyWithValue("positionBegin", 10);
            assertThat(node.inspect()).isEqualTo(".name");
        }

        @Test
        void matches_bracket_property_and_return_node() {
            Node node = givenCode("[0]").fetchNode();

            assertThat(node)
                    .isInstanceOf(PropertyNode.class)
                    .hasFieldOrPropertyWithValue("name", 0);
            assertThat(node.inspect()).isEqualTo("[0]");
        }

        @Test
        void return_null_when_dost_not_match() {
            assertThat(fetchNodeWhenGivenToken(operatorToken("+")))
                    .isNull();
        }

        @Test
        void support_ignore_start_dot_char() {
            Node node = givenCode("name").fetchNode();

            assertThat(node)
                    .isInstanceOf(PropertyNode.class)
                    .hasFieldOrPropertyWithValue("name", "name");
            assertThat(node.inspect()).isEqualTo("name");
        }

        @Test
        void support_ignore_start_dot_char_for_property_chain() {
            Node node = givenCode("product.name").fetchNode();

            assertThat(node)
                    .isInstanceOf(PropertyNode.class)
                    .hasFieldOrPropertyWithValue("name", "name");
            assertThat(node.inspect()).isEqualTo("product.name");
        }
    }

    @Nested
    class FetchParenthesesNode extends NodeFactoryTestBase {

        @Override
        protected NodeFactory getDefaultNodeFactory() {
            return NodeFactory.createParenthesesNodeFactory();
        }

        @Test
        void return_empty_when_not_matches() {
            assertThat(fetchNodeWhenGivenToken(constValueToken("not start with (")))
                    .isNull();
        }

        @Test
        void support_single_node_wrapped_with_parentheses() {
            Node node = givenToken(openingParenthesisToken(), 10)
                    .givenToken(Token.constValueToken("str"))
                    .givenToken(Token.closingParenthesisToken())
                    .fetchNode();

            assertThat(node).isInstanceOf(ParenthesesNode.class)
                    .hasFieldOrPropertyWithValue("positionBegin", 10);

            assertThat(node.inspect()).isEqualTo("('str')");
        }

        @Test
        void support_expression_node_wrapped_with_parentheses() {
            assertThat(givenCode("(1+1)").fetchNode().inspect()).isEqualTo("(1 + 1)");
        }

        @Test
        void raiser_error_when_parentheses_has_no_data() {
            assertThat(invalidSyntaxToken(givenToken(openingParenthesisToken(), 100)))
                    .hasMessage("expect a value or expression")
                    .hasFieldOrPropertyWithValue("position", 100);
        }

        @Test
        void raiser_error_when_parentheses_not_finished() {
            assertThat(invalidSyntaxToken(givenCode("(1")))
                    .hasMessage("missed `)`")
                    .hasFieldOrPropertyWithValue("position", 2);
        }

        @Test
        void raiser_error_when_got_unexpected_token() {
            assertThat(invalidSyntaxToken(givenCode("(1 1")))
                    .hasMessage("unexpected token, `)` expected")
                    .hasFieldOrPropertyWithValue("position", 3);
        }
    }
}
