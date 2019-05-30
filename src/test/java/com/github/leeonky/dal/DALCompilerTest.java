package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class DALCompilerTest {

    private DALCompiler dalCompiler = new DALCompiler();

    @Nested
    class NoExpression {

        @Test
        void empty_source_code_should_return_input_node() {
            Node node = dalCompiler.compile(new SourceCode(""));
            assertThat(node).isEqualTo(InputNode.INSTANCE);
        }

        @Test
        void access_property_of_root_value() {
            Node node = dalCompiler.compile(new SourceCode(".result"));
            assertThat(node).isEqualTo(new PropertyNode(InputNode.INSTANCE, singletonList("result")));
        }

        @Test
        void access_property_list_of_root_value() {
            Node node = dalCompiler.compile(new SourceCode(".sub .result"));
            assertThat(node).isEqualTo(new PropertyNode(new PropertyNode(InputNode.INSTANCE, singletonList("sub")), singletonList("result")));
        }

        @Test
        void access_one_const_value_ignore_root_value() {
            Node node = dalCompiler.compile(new SourceCode("1"));
            assertThat(node).isEqualTo(new ConstNode(new BigDecimal(1)));
        }

        @Test
        void access_one_const_value_property_ignore_root_value() {
            Node node = dalCompiler.compile(new SourceCode("''.empty"));
            assertThat(node).isEqualTo(new PropertyNode(new ConstNode(""), singletonList("empty")));
        }
    }

    @Nested
    class SimpleExpression {

        private void assertCompileOperator(String sourceCode, Operator operator) {
            Node node = dalCompiler.compile(new SourceCode(sourceCode + "1"));
            assertThat(node).isEqualTo(new Expression(InputNode.INSTANCE, operator, new ConstNode(new BigDecimal(1))));
        }

        @Test
        void expression_with_root_value() {
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
            SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> dalCompiler.compile(new SourceCode("&1")));
            assertThat(syntaxException)
                    .hasFieldOrPropertyWithValue("position", 0)
                    .hasMessage("not support operator & yet");
        }

        @Test
        void should_raise_error_when_expression_not_finished_1() {
            SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> dalCompiler.compile(new SourceCode("=")));
            assertThat(syntaxException)
                    .hasFieldOrPropertyWithValue("position", 1)
                    .hasMessage("expression not finished");
        }

        @Test
        void should_raise_error_when_expression_not_finished_2() {
            SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> dalCompiler.compile(new SourceCode("= =")));
            assertThat(syntaxException)
                    .hasFieldOrPropertyWithValue("position", 2)
                    .hasMessage("expression not finished");
        }

        @Test
        void simple_expression() {
            Node node = dalCompiler.compile(new SourceCode("+1=2"));
            assertThat(node).isEqualTo(new Expression(new Expression(InputNode.INSTANCE, new Operator.Plus(), new ConstNode(new BigDecimal(1))),
                    new Operator.Equal(), new ConstNode(new BigDecimal(2))));
        }
    }

    @Nested
    class ComplexExpression {

        @Test
        void operator_order_plus_first() {
            Node node = dalCompiler.compile(new SourceCode("=1+1"));
            assertThat(node).isEqualTo(new Expression(InputNode.INSTANCE,
                    new Operator.Equal(), new Expression(new ConstNode(new BigDecimal(1)), new Operator.Plus(), new ConstNode(new BigDecimal(1)))
            ));
        }

        @Test
        void operator() {
            Node node = dalCompiler.compile(new SourceCode("=1+1*1"));
            assertThat(node).isEqualTo(new Expression(InputNode.INSTANCE,
                    new Operator.Equal(),
                    new Expression(
                            new ConstNode(new BigDecimal(1))
                            , new Operator.Plus(),
                            new Expression(new ConstNode(new BigDecimal(1)), new Operator.Multiplication(), new ConstNode(new BigDecimal(1)))
                    )
            ));
        }
    }

//    @Test
//    void one_const_value_source_code_should_return_input_node() {
//        Node node = dalCompiler.compile(new SourceCode("true"));
//        assertThat(node).isEqualTo(new ConstNode(true));
//    }

    @Nested
    class TypeAssertionExpressionTest {

//        @Test
//        void is_which_structure() {
//            Node node = dalCompiler.compile(new SourceCode("is Object which 1=1"));
//            assertThat(node).isEqualTo(
//                    new TypeAssertionExpression(new InputNode(), new TypeNode("Object"),
//                            new Expression(new ConstNode(new BigDecimal(1)), new ConstNode(new BigDecimal(1)), EQUAL)
//                    ));
//        }
//
//        @Test
//        void common_expression() {
//            Node node = dalCompiler.compile(new SourceCode("is Object"));
//            assertThat(node).isEqualTo(
//                    new Expression(new InputNode(), new TypeNode("Object"), EQUAL)
//            );
//        }

        //        @Test
//        void type_assertion_and_property_assertion_with_no_word_which() {
//            Node node = dalCompiler.compile(new SourceCode("is Object 1=1"));
//            assertThat(node).isEqualTo(
//                    new TypeAssertionExpression(new InputNode(), new TypeNode("Object"),
//                            new Expression(new ConstNode(new BigDecimal(1)), new ConstNode(new BigDecimal(1)), new Equal())
//                    ));
//        }


        //        @Test
//        void only_type_assertion_no_property_assertion_no_which() {
//            Node node = dalCompiler.compile(new SourceCode("is Object"));
//            assertThat(node).isEqualTo(new TypeAssertionExpression(new InputNode(), "Object", new ConstNode(true)));
//        }

        //        @Test
//        void nested_is_which_struct() {
//            Node node = dalCompiler.compile(new SourceCode("is Object which '' is Object which 1=1"));
//            InputNode instance = new InputNode();
//            assertThat(node).isEqualTo(
//                    new TypeAssertionExpression(instance, "Object",
//                            new TypeAssertionExpression(new ConstNode(""), "Object",
//                                    new Expression(new ConstNode(new BigDecimal(1)), new ConstNode(new BigDecimal(1)), new Equal())
//                            )
//                    ));
//        }

        //        @Test
//        void default_type_assertion_and_property_assertion() {
//            Node node = dalCompiler.compile(new SourceCode("1=1"));
//            assertThat(node).isEqualTo(
//                    new TypeAssertionExpression(new InputNode(), "Object",
//                            new Expression(new ConstNode(new BigDecimal(1)), new ConstNode(new BigDecimal(1)), new Equal())
//                    ));
//        }
    }
}
