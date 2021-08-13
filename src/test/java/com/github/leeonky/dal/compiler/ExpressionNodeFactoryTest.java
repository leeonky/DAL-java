package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.RuntimeContextBuilder;
import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static com.github.leeonky.dal.token.Token.constValueToken;
import static org.assertj.core.api.Assertions.assertThat;

class ExpressionNodeFactoryTest extends NodeFactoryTestBase {

    @Override
    protected NodeFactory getDefaultNodeFactory() {
        return NodeFactory.createExpressionNodeFactory();
    }

    @Test
    void return_single_node_when_no_operator() {
        Node node = givenToken(constValueToken("str"), 10).fetchNode();

        assertThat(node)
                .isInstanceOf(ConstNode.class)
                .hasFieldOrPropertyWithValue("value", "str")
                .hasFieldOrPropertyWithValue("positionBegin", 10);
    }

    @Test
    void should_check_end_bracket() {
        assertThat(invalidSyntaxToken(givenCode("1)")))
                .hasFieldOrPropertyWithValue("position", 1)
                .hasMessage("missed begin bracket");
    }

    @Test
    void auto_reference_this_when_the_first_operand_not_exist() {
        Node node = givenCode("+1").fetchNode();

        assertThat(node).isInstanceOf(Expression.class);

        assertThat(node.evaluate(new RuntimeContextBuilder().build(1)))
                .isEqualTo(BigDecimal.valueOf(2));
    }

    @Test
    void should_raise_error_when_expression_not_finish() {
        assertThat(invalidSyntaxToken(givenCode("=")))
                .hasMessage("expression is not finished")
                .hasFieldOrPropertyWithValue("position", 1);
    }

    @Nested
    class OperatorExpression {

        @Test
        void return_expression_when_has_operator_and_another_token() {
            Node node = givenToken(constValueToken("hello"), 10)
                    .givenToken(Token.operatorToken("+"))
                    .givenToken(constValueToken("world"))
                    .fetchNode();

            assertThat(node)
                    .isInstanceOf(Expression.class)
                    .hasFieldOrPropertyWithValue("positionBegin", 10);
            assertThat(node.inspect()).isEqualTo("'hello' + 'world'");
        }

        @Test
        void return_expression_chain() {
            Node node = givenToken(constValueToken("hello"), 10)
                    .givenToken(Token.operatorToken("+"))
                    .givenToken(constValueToken("world"))
                    .givenToken(Token.operatorToken("+"))
                    .givenToken(constValueToken("goodbye"))
                    .fetchNode();

            assertThat(node)
                    .isInstanceOf(Expression.class)
                    .hasFieldOrPropertyWithValue("positionBegin", 10);
            assertThat(node.inspect()).isEqualTo("'hello' + 'world' + 'goodbye'");
        }

        @Test
        void should_change_expression_by_operator_precedence() {
            assertThat(givenCode("1+2*2").fetchNode().evaluate(new RuntimeContextBuilder().build(null)))
                    .isEqualTo(BigDecimal.valueOf(5));
        }
    }

    @Nested
    class SecondOperand {

        @Nested
        class Regex {

            @Test
            void equal_to_regex() {
                Node node = givenCode("= /1/").fetchNode();

                assertThat(node.inspect()).isEqualTo(" = /1/");
            }

            @Test
            void matches_with_regex() {
                Node node = givenCode(": /1/").fetchNode();

                assertThat(node.inspect()).isEqualTo(" : /1/");
            }
        }
    }
}
