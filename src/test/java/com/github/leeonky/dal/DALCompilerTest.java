package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.TypeAssertionExpression;
import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class DALCompilerTest {

    @Nested
    class FromBeginning {
        DALCompiler dalCompiler = new DALCompiler();

        @Test
        void type_assertion_and_property_assertion() {
            Object input = new Object();
            Node node = dalCompiler.compile2(input, new SourceCode("is Object which 1=1"));
            assertThat(node).isEqualTo(
                    new TypeAssertionExpression(new ConstNode(input), "Object",
                            new Expression(new ConstNode(new BigDecimal(1)), new ConstNode(new BigDecimal(1)), new Equal())
                    ));
        }

        @Test
        void only_type_assertion_no_property_assertion_no_which() {
            Object input = new Object();
            Node node = dalCompiler.compile2(input, new SourceCode("is Object"));
            assertThat(node).isEqualTo(new TypeAssertionExpression(new ConstNode(input), "Object", new ConstNode(true)));
        }
    }
}