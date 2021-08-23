package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.ParenthesesNode;
import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Token.constValueToken;
import static com.github.leeonky.dal.token.Token.openingParenthesisToken;
import static org.assertj.core.api.Assertions.assertThat;

class SingleNodeFactoryTest {

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
