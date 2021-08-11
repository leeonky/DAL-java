package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.RuntimeContextBuilder;
import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class ExpressionNodeFactoryTest extends NodeFactoryTestBase {

    @Override
    protected NodeFactory getDefaultNodeFactory() {
        return NodeFactory.createExpressionNodeFactory();
    }

    @Test
    void return_single_node_when_no_operator() {
        Node node = givenToken(Token.constValueToken("str"), 10)
                .fetchNode();

        assertThat(node)
                .isInstanceOf(ConstNode.class)
                .hasFieldOrPropertyWithValue("value", "str")
                .hasFieldOrPropertyWithValue("positionBegin", 10);
    }

    @Nested
    class OperatorExpression {

        @Test
        void return_expression_when_has_operator_and_another_token() {
            Node node = givenToken(Token.constValueToken("hello"), 10)
                    .givenToken(Token.operatorToken("+"))
                    .givenToken(Token.constValueToken("world"))
                    .fetchNode();

            assertThat(node)
                    .isInstanceOf(Expression.class)
                    .hasFieldOrPropertyWithValue("positionBegin", 10);
            assertThat(node.inspect()).isEqualTo("'hello' + 'world'");
        }

        @Test
        void return_expression_chain() {
            Node node = givenToken(Token.constValueToken("hello"), 10)
                    .givenToken(Token.operatorToken("+"))
                    .givenToken(Token.constValueToken("world"))
                    .givenToken(Token.operatorToken("+"))
                    .givenToken(Token.constValueToken("goodbye"))
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

}
