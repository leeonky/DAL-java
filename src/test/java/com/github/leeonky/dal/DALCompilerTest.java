package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class DALCompilerTest {

    private DALCompiler dalCompiler = new DALCompiler();

    private void assertSyntaxException(String sourceCode, int position, String message) {
        SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> dalCompiler.compile(new SourceCode(sourceCode)));
        assertThat(syntaxException)
                .hasFieldOrPropertyWithValue("position", position)
                .hasMessage(message);
    }

    private void assertCompileNode(String sourceCode, Node expected) {
        assertThat(dalCompiler.compile(new SourceCode(sourceCode))).isEqualTo(expected);
    }

    @Nested
    class PropertyExpression {

        @Test
        void empty_source_code_should_return_input_node() {
            assertCompileNode("", InputNode.INSTANCE);
        }

        @Test
        void access_property_of_root_value() {
            assertCompileNode(".result", new PropertyNode(InputNode.INSTANCE, singletonList("result")));
        }

        @Test
        void access_property_list_of_root_value() {
            assertCompileNode(".sub .result", new PropertyNode(new PropertyNode(InputNode.INSTANCE, singletonList("sub")), singletonList("result")));
        }

        @Test
        void access_one_const_value_ignore_root_value() {
            assertCompileNode("1", new ConstNode(new BigDecimal(1)));
        }

        @Test
        void access_one_const_value_property_ignore_root_value() {
            assertCompileNode("''.empty", new PropertyNode(new ConstNode(""), singletonList("empty")));
        }

        @Test
        void access_property_after_bracket() {
            BracketNode bracketNode = new BracketNode();
            bracketNode.setNode(new ConstNode(""));
            assertCompileNode("('').empty", new PropertyNode(bracketNode, singletonList("empty")));
        }
    }

    @Nested
    class AccessElementExpression {

        @Test
        void support_access_array_by_const_index() {
            assertCompileNode("[0]", new Expression(InputNode.INSTANCE, new Operator.Index(), new ConstNode(0)));
        }
    }

    @Nested
    class SimpleExpression {

        private void assertCompileOperator(String sourceCode, Operator operator) {
            assertCompileNode(sourceCode + " 1", new Expression(InputNode.INSTANCE, operator, new ConstNode(new BigDecimal(1))));
        }

        @Test
        void should_support_follow_operators() {
            assertCompileOperator("matches", new Operator.Matches());
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
            assertSyntaxException("&1", 0, "not support operator `&` yet");
        }

        @Test
        void should_raise_error_when_expression_not_finished_1() {
            assertSyntaxException("=", 1, "expression not finished");
        }

        @Test
        void should_raise_error_when_expression_not_finished_2() {
            assertSyntaxException("= =", 2, "expression not finished");
        }

        @Test
        void simple_expression() {
            assertCompileNode("+1=2",
                    new Expression(new Expression(InputNode.INSTANCE, new Operator.Plus(), new ConstNode(new BigDecimal(1))),
                            new Operator.Equal(), new ConstNode(new BigDecimal(2))));
        }
    }

    @Nested
    class ComplexExpression {

        @Test
        void plus_has_higher_precedence_then_equal() {
            assertCompileNode("=1+1", new Expression(InputNode.INSTANCE,
                    new Operator.Equal(), new Expression(new ConstNode(new BigDecimal(1)), new Operator.Plus(), new ConstNode(new BigDecimal(1)))
            ));
        }

        @Test
        void mul_has_higher_precedence_then_plus() {
            assertCompileNode("=1+1*1", new Expression(InputNode.INSTANCE,
                    new Operator.Equal(),
                    new Expression(
                            new ConstNode(new BigDecimal(1))
                            , new Operator.Plus(),
                            new Expression(new ConstNode(new BigDecimal(1)), new Operator.Multiplication(), new ConstNode(new BigDecimal(1)))
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
                            new ConstNode(new BigDecimal(1))
                    )
                    , new Operator.Equal(),
                    new ConstNode(new BigDecimal(0))
            ));

            assertCompileNode("1+ -1=0", new Expression(
                    new Expression(
                            new ConstNode(new BigDecimal(1))
                            , new Operator.Plus(),
                            new Expression(
                                    new ConstNode(null)
                                    , new Operator.Minus(),
                                    new ConstNode(new BigDecimal(1))
                            )
                    )
                    , new Operator.Equal(),
                    new ConstNode(new BigDecimal(0))
            ));
        }
    }

    @Nested
    class BracketInExpression {

        @Test
        void compile_simple_bracket() {
            assertCompileNode("(1)", new BracketNode().setNode(new ConstNode(new BigDecimal(1))).finishBracket());
        }

        @Test
        void miss_end_bracket_should_raise_error() {
            assertSyntaxException("(1", 0, "missed end bracket");
        }

        @Test
        void miss_begin_bracket_should_raise_error() {
            assertSyntaxException("1)", 1, "missed begin bracket");
        }
    }

    @Nested
    class Regex {

        @Test
        void compile_regex() {
            assertCompileNode("matches /1/",
                    new Expression(InputNode.INSTANCE, new Operator.Matches(), new RegexNode("1")));

            assertCompileNode("matches /1 2/",
                    new Expression(InputNode.INSTANCE, new Operator.Matches(), new RegexNode("1 2")));

            assertCompileNode("matches /1\\/2/",
                    new Expression(InputNode.INSTANCE, new Operator.Matches(), new RegexNode("1/2")));
        }

        @Test
        void regex_not_finished() {
            assertSyntaxException("matches /1", 10, "regex should end with '/'");
        }

        @Test
        void escape_char() {
            assertCompileNode("matches /\\/\\t\\n\\\\/",
                    new Expression(InputNode.INSTANCE, new Operator.Matches(), new RegexNode("/\t\n\\\\")));
        }

        @Test
        void invalid_escape_char() {
            assertSyntaxException("matches /\\a/", 10, "unsupported escape char");
        }
    }

    @Nested
    class LogicExpression {

        @Test
        void simple_logic_and() {
            assertCompileNode("&& true", new Expression(InputNode.INSTANCE, new Operator.And(), new ConstNode(true)));
            assertCompileNode("and true", new Expression(InputNode.INSTANCE, new Operator.And(), new ConstNode(true)));
        }

        @Test
        void simple_logic_or() {
            assertCompileNode("|| true", new Expression(InputNode.INSTANCE, new Operator.Or(), new ConstNode(true)));
            assertCompileNode("or true", new Expression(InputNode.INSTANCE, new Operator.Or(), new ConstNode(true)));
        }

        @Test
        void lower_precedence_then_others() {
            assertCompileNode("=1 || 2>1", new Expression(
                    new Expression(InputNode.INSTANCE, new Operator.Equal(), new ConstNode(new BigDecimal(1)))
                    , new Operator.Or(),
                    new Expression(new ConstNode(new BigDecimal(2)), new Operator.Greater(), new ConstNode(new BigDecimal(1)))
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

    @Nested
    class InstanceOf {

        @Test
        void is_type_expression() {
            assertCompileNode("is String", new SchemaAssertionExpression(
                    InputNode.INSTANCE,
                    new SchemaNode("String"),
                    new ConstNode(true)
            ));
        }

        @Test
        void is_type_expression_and_which_assertion() {
            assertCompileNode("is String which .empty", new SchemaAssertionExpression(
                    InputNode.INSTANCE,
                    new SchemaNode("String"),
                    new PropertyNode(InputNode.INSTANCE, asList("empty"))
            ));
        }
    }
}
