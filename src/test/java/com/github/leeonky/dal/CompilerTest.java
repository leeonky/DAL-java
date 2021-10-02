package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.compiler.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CompilerTest {

    private final Compiler compiler = new Compiler();

    private void assertSyntaxException(String sourceCode, int position, String message) {
        SyntaxException syntaxException = assertThrows(SyntaxException.class,
                () -> compiler.compile(new SourceCode(sourceCode)));
        assertThat(syntaxException)
                .hasFieldOrPropertyWithValue("position", position)
                .hasMessage(message);
    }

    private void assertCompileNode(String sourceCode, Node expected) {
        assertThat(compiler.compile(new SourceCode(sourceCode))).isEqualTo(expected);
    }

    @Nested
    class PropertyExpression {

        @Test
        void empty_source_code_should_return_input_node() {
            assertCompileNode("", InputNode.INSTANCE);
        }

        @Test
        void access_property_of_root_value() {
            assertCompileNode(".result", new PropertyNode(InputNode.INSTANCE, "result", DOT));
        }

        @Test
        void access_property_list_of_root_value() {
            assertCompileNode(".sub .result", new PropertyNode(new PropertyNode(InputNode.INSTANCE, "sub", DOT), "result", DOT));
        }

        @Test
        void access_one_const_value_ignore_root_value() {
            assertCompileNode("1", new ConstNode(1));
        }

        @Test
        void access_one_const_value_property_ignore_root_value() {
            assertCompileNode("''.empty", new PropertyNode(new ConstNode(""), "empty", DOT));
        }

        @Test
        void access_property_after_parentheses() {
            ParenthesesNode parenthesesNode = new ParenthesesNode(new ConstNode(""));
            assertCompileNode("('').empty", new PropertyNode(parenthesesNode, "empty", DOT));
        }
    }

    @Nested
    class AccessElementExpression {

        @Test
        void support_access_array_by_const_index() {
            assertCompileNode("[0]", new PropertyNode(InputNode.INSTANCE, 0, DOT));
        }

        @Test
        void miss_opening_bracket() {
            assertSyntaxException("1]", 1, "unexpected token");
        }
    }

    @Nested
    class SimpleExpression {

        private void assertCompileOperator(String sourceCode, Operator operator) {
            assertCompileNode(sourceCode + " 1", new Expression(InputNode.INSTANCE, operator, new ConstNode(1)));
        }

        @Test
        void should_support_follow_operators() {
            assertCompileOperator(":", new Operator.Matcher());
            assertCompileOperator("=", new Operator.Equal());
            assertCompileOperator("!=", new Operator.NotEqual());
            assertCompileOperator(">", new Operator.Greater());
            assertCompileOperator("<", new Operator.Less());
            assertCompileOperator(">=", new Operator.GreaterOrEqual());
            assertCompileOperator("<=", new Operator.LessOrEqual());
            assertCompileOperator("+", new Operator.Plus());
            assertCompileOperator("-", new Operator.Subtraction());
            assertCompileOperator("*", new Operator.Multiplication());
            assertCompileOperator("/", new Operator.Division());
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
                    new Expression(new Expression(InputNode.INSTANCE, new Operator.Plus(), new ConstNode(1)),
                            new Operator.Equal(), new ConstNode(2)));
        }
    }

    @Nested
    class ComplexExpression {

        @Test
        void plus_has_higher_precedence_then_equal() {
            assertCompileNode("=1+1", new Expression(InputNode.INSTANCE,
                    new Operator.Equal(), new Expression(new ConstNode(1), new Operator.Plus(), new ConstNode(1))
            ));
        }

        @Test
        void mul_has_higher_precedence_then_plus() {
            assertCompileNode("=1+1*1", new Expression(InputNode.INSTANCE,
                    new Operator.Equal(),
                    new Expression(
                            new ConstNode(1)
                            , new Operator.Plus(),
                            new Expression(new ConstNode(1), new Operator.Multiplication(), new ConstNode(1))
                    )
            ));
        }
    }

    @Nested
    class OperatorMinus {

        @Test
        void minus_should_after_an_operator() {
            assertCompileNode("-1=0", new Expression(
                    new Expression(
                            InputNode.INSTANCE
                            , new Operator.Subtraction(),
                            new ConstNode(1)
                    )
                    , new Operator.Equal(),
                    new ConstNode(0)
            ));

            assertCompileNode("1+ -1=0", new Expression(
                    new Expression(
                            new ConstNode(1)
                            , new Operator.Plus(),
                            new Expression(
                                    new ConstNode(null)
                                    , new Operator.Minus(),
                                    new ConstNode(1)
                            )
                    )
                    , new Operator.Equal(),
                    new ConstNode(0)
            ));
        }
    }

    @Nested
    class ParenthesesInExpression {

        @Test
        void compile_simple_parentheses() {
            assertCompileNode("(1)", new ParenthesesNode(new ConstNode(1)));
        }

        @Test
        void miss_closing_parenthesis_should_raise_error() {
            assertSyntaxException("(1", 2, "should end with `)`");
        }

        @Test
        void miss_opening_parenthesis_should_raise_error() {
            assertSyntaxException("1)", 1, "unexpected token");
        }
    }

    @Nested
    class Regex {

        @Test
        void compile_match_regex() {
            assertCompileNode(" : /1/",
                    new Expression(InputNode.INSTANCE, new Operator.Matcher(), new RegexNode("1")));
        }

        @Test
        void compile_eq_regex() {
            assertCompileNode(" = /1/",
                    new Expression(InputNode.INSTANCE, new Operator.Equal(), new RegexNode("1")));
        }
    }

    @Nested
    class LogicExpression {

        @Test
        void simple_logic_and() {
            assertCompileNode("&& true", new Expression(InputNode.INSTANCE, new Operator.And("&&"), new ConstNode(true)));
            assertCompileNode("and true", new Expression(InputNode.INSTANCE, new Operator.And("&&"), new ConstNode(true)));
        }

        @Test
        void simple_logic_or() {
            assertCompileNode("|| true", new Expression(InputNode.INSTANCE, new Operator.Or("||"), new ConstNode(true)));
            assertCompileNode("or true", new Expression(InputNode.INSTANCE, new Operator.Or("||"), new ConstNode(true)));
        }

        @Test
        void lower_precedence_then_others() {
            assertCompileNode("=1 || 2>1", new Expression(
                    new Expression(InputNode.INSTANCE, new Operator.Equal(), new ConstNode(1))
                    , new Operator.Or("||"),
                    new Expression(new ConstNode(2), new Operator.Greater(), new ConstNode(1))
            ));
        }

        @Test
        void logical_not() {
            assertCompileNode("!true", new Expression(new ConstNode(null), new Operator.Not(), new ConstNode(true)));
        }

        @Test
        void logical_not_should_has_highest_precedence() {
            assertCompileNode("!true=false", new Expression(
                    new Expression(new ConstNode(null), new Operator.Not(), new ConstNode(true))
                    , new Operator.Equal(),
                    new ConstNode(false)
            ));
        }
    }

    @Test
    void miss_opening_brace() {
        assertSyntaxException("1}", 1, "unexpected token");
    }
}
