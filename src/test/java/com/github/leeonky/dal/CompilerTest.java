package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Node;
import com.github.leeonky.interpreter.SourceCode;
import com.github.leeonky.interpreter.SyntaxException;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CompilerTest {

    private final Compiler compiler = new Compiler();
    private final RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new DAL().getRuntimeContextBuilder().build(null);

    private void assertSyntaxException(String sourceCode, int position, String message) {
        SyntaxException syntaxException = assertThrows(SyntaxException.class,
                () -> compiler.compile(new SourceCode(sourceCode), DALRuntimeContext));
        assertThat(syntaxException)
                .hasFieldOrPropertyWithValue("position", position)
                .hasMessage(message);
    }

    private void assertCompileNode(String sourceCode, Node... expected) {
        assertThat(compiler.compile(new SourceCode(sourceCode), DALRuntimeContext)).isEqualTo(asList(expected));
    }

    @Nested
    class PropertyExpression {

        @Test
        void empty_source_code_should_return_input_node() {
            assertCompileNode("", InputNode.INSTANCE);
        }

        @Test
        void access_one_const_value_ignore_root_value() {
            assertCompileNode("1", new ConstNode(1));
        }
    }

    @Nested
    class AccessElementExpression {

        @Test
        void miss_opening_bracket() {
            assertSyntaxException("1]", 1, "expect a value or expression");
        }
    }

    @Nested
    class SimpleExpression {

        private void assertCompileOperator(String sourceCode, DALOperator operator) {
            assertCompileNode(sourceCode + " 1", new DALExpression(InputNode.INSTANCE, operator, new ConstNode(1)));
        }

        @Test
        void should_support_follow_operators() {
            assertCompileOperator(":", new DALOperator.Matcher());
            assertCompileOperator("=", new DALOperator.Equal());
            assertCompileOperator("!=", new DALOperator.NotEqual());
            assertCompileOperator(">", new DALOperator.Greater());
            assertCompileOperator("<", new DALOperator.Less());
            assertCompileOperator(">=", new DALOperator.GreaterOrEqual());
            assertCompileOperator("<=", new DALOperator.LessOrEqual());
            assertCompileOperator("+", new DALOperator.Plus());
            assertCompileOperator("-", new DALOperator.Subtraction());
            assertCompileOperator("*", new DALOperator.Multiplication());
            assertCompileOperator("/", new DALOperator.Division());
        }

        @Test
        void not_supported_operator() {
            assertSyntaxException("&1", 0, "unexpected token");
        }

        @Test
        void should_raise_error_when_expression_not_finished_1() {
            assertSyntaxException("=", 1, "expect a value or expression");
        }

        @Test
        void simple_expression() {
            assertCompileNode("+1=2",
                    new DALExpression(new DALExpression(InputNode.INSTANCE, new DALOperator.Plus(), new ConstNode(1)),
                            new DALOperator.Equal(), new ConstNode(2)));
        }
    }

    @Nested
    class ComplexExpression {

        @Test
        void plus_has_higher_precedence_then_equal() {
            assertCompileNode("=1+1", new DALExpression(InputNode.INSTANCE,
                    new DALOperator.Equal(), new DALExpression(new ConstNode(1), new DALOperator.Plus(), new ConstNode(1))
            ));
        }

        @Test
        void mul_has_higher_precedence_then_plus() {
            assertCompileNode("=1+1*1", new DALExpression(InputNode.INSTANCE,
                    new DALOperator.Equal(),
                    new DALExpression(
                            new ConstNode(1)
                            , new DALOperator.Plus(),
                            new DALExpression(new ConstNode(1), new DALOperator.Multiplication(), new ConstNode(1))
                    )
            ));
        }
    }

    @Nested
    class OperatorMinus {

        @Test
        void minus_should_after_an_operator() {
            assertCompileNode("-1=0", new DALExpression(
                    new DALExpression(
                            InputNode.INSTANCE
                            , new DALOperator.Subtraction(),
                            new ConstNode(1)
                    )
                    , new DALOperator.Equal(),
                    new ConstNode(0)
            ));

            assertCompileNode("1+ -1=0", new DALExpression(
                    new DALExpression(
                            new ConstNode(1)
                            , new DALOperator.Plus(),
                            new DALExpression(
                                    null
                                    , new DALOperator.Minus(),
                                    new ConstNode(1)
                            )
                    )
                    , new DALOperator.Equal(),
                    new ConstNode(0)
            ));
        }
    }

    @Nested
    class ParenthesesInExpression {

        @Test
        void miss_opening_parenthesis_should_raise_error() {
            assertSyntaxException("1)", 1, "expect a value or expression");
        }
    }

    @Nested
    class Regex {

        @Test
        void compile_match_regex() {
            assertCompileNode(" : /1/",
                    new DALExpression(InputNode.INSTANCE, new DALOperator.Matcher(), new RegexNode("1")));
        }

        @Test
        void compile_eq_regex() {
            assertCompileNode(" = /1/",
                    new DALExpression(InputNode.INSTANCE, new DALOperator.Equal(), new RegexNode("1")));
        }
    }

    @Nested
    class LogicExpression {

        @Test
        void simple_logic_and() {
            assertCompileNode("&& true", new DALExpression(InputNode.INSTANCE, new DALOperator.And("&&"), new ConstNode(true)));
            assertCompileNode("and true", new DALExpression(InputNode.INSTANCE, new DALOperator.And("&&"), new ConstNode(true)));
        }

        @Test
        void simple_logic_or() {
            assertCompileNode("|| true", new DALExpression(InputNode.INSTANCE, new DALOperator.Or("||"), new ConstNode(true)));
            assertCompileNode("or true", new DALExpression(InputNode.INSTANCE, new DALOperator.Or("||"), new ConstNode(true)));
        }

        @Test
        void lower_precedence_then_others() {
            assertCompileNode("=1 || 2>1", new DALExpression(
                    new DALExpression(InputNode.INSTANCE, new DALOperator.Equal(), new ConstNode(1))
                    , new DALOperator.Or("||"),
                    new DALExpression(new ConstNode(2), new DALOperator.Greater(), new ConstNode(1))
            ));
        }

        @Test
        void logical_not() {
            assertCompileNode("!true", new DALExpression(null, new DALOperator.Not(), new ConstNode(true)));
        }

        @Test
        void logical_not_should_has_highest_precedence() {
            assertCompileNode("!true=false", new DALExpression(
                    new DALExpression(null, new DALOperator.Not(), new ConstNode(true))
                    , new DALOperator.Equal(),
                    new ConstNode(false)
            ));
        }
    }

    @Test
    void miss_opening_brace() {
        assertSyntaxException("1}", 1, "expect a value or expression");
    }

    @Test
    void support_compile_multi_expression() {
        assertCompileNode("1 2", new ConstNode(1), new ConstNode(2));
    }
}
