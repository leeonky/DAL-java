package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;

class DALCompilerTest {

    DALCompiler dalCompiler = new DALCompiler();

    @Test
    void empty_source_code_should_return_input_node() {
        Node node = dalCompiler.compile2(new SourceCode(""));
        assertThat(node).isEqualTo(InputNode.INSTANCE);
    }

    @Test
    void access_property_of_root_value() {
        Node node = dalCompiler.compile2(new SourceCode(".result"));
        assertThat(node).isEqualTo(new PropertyNode(InputNode.INSTANCE, singletonList("result")));
    }

    @Test
    void access_property_list_of_root_value() {
        Node node = dalCompiler.compile2(new SourceCode(".sub .result"));
        assertThat(node).isEqualTo(new PropertyNode(new PropertyNode(InputNode.INSTANCE, singletonList("sub")), singletonList("result")));
    }

//    @Test
//    void one_const_value_source_code_should_return_input_node() {
//        Node node = dalCompiler.compile2(new SourceCode("true"));
//        assertThat(node).isEqualTo(new ConstNode(true));
//    }

    @Nested
    class TypeAssertionExpressionTest {

//        @Test
//        void is_which_structure() {
//            Node node = dalCompiler.compile2(new SourceCode("is Object which 1=1"));
//            assertThat(node).isEqualTo(
//                    new TypeAssertionExpression(new InputNode(), new TypeNode("Object"),
//                            new Expression(new ConstNode(new BigDecimal(1)), new ConstNode(new BigDecimal(1)), EQUAL)
//                    ));
//        }
//
//        @Test
//        void common_expression() {
//            Node node = dalCompiler.compile2(new SourceCode("is Object"));
//            assertThat(node).isEqualTo(
//                    new Expression(new InputNode(), new TypeNode("Object"), EQUAL)
//            );
//        }

        //        @Test
//        void type_assertion_and_property_assertion_with_no_word_which() {
//            Node node = dalCompiler.compile2(new SourceCode("is Object 1=1"));
//            assertThat(node).isEqualTo(
//                    new TypeAssertionExpression(new InputNode(), new TypeNode("Object"),
//                            new Expression(new ConstNode(new BigDecimal(1)), new ConstNode(new BigDecimal(1)), new Equal())
//                    ));
//        }


        //        @Test
//        void only_type_assertion_no_property_assertion_no_which() {
//            Node node = dalCompiler.compile2(new SourceCode("is Object"));
//            assertThat(node).isEqualTo(new TypeAssertionExpression(new InputNode(), "Object", new ConstNode(true)));
//        }

        //        @Test
//        void nested_is_which_struct() {
//            Node node = dalCompiler.compile2(new SourceCode("is Object which '' is Object which 1=1"));
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
//            Node node = dalCompiler.compile2(new SourceCode("1=1"));
//            assertThat(node).isEqualTo(
//                    new TypeAssertionExpression(new InputNode(), "Object",
//                            new Expression(new ConstNode(new BigDecimal(1)), new ConstNode(new BigDecimal(1)), new Equal())
//                    ));
//        }
    }
}
