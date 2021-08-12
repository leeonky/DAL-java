package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.TypeWhichExpression;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class TypeWhichExpressionTest extends NodeFactoryTestBase {

    @Override
    protected NodeFactory getDefaultNodeFactory() {
        return NodeFactory.createExpressionNodeFactory();
    }

    @Test
    void support_type_which_expression() {
        Node node = givenCode("1 is Integer which true").fetchNode();

        assertThat(node).isInstanceOf(TypeWhichExpression.class);
        assertThat(node.inspect()).isEqualTo("1 is Integer which true");
    }

    @Test
    void support_type_which_complex_expression() {
        Node node = givenCode("1 is Integer which true and true").fetchNode();

        assertThat(node).isInstanceOf(TypeWhichExpression.class);
        assertThat(node.inspect()).isEqualTo("1 is Integer which true and true");
    }

    @Test
    void should_raise_error_when_no_clause() {
        assertThat(invalidSyntaxToken(givenCode("1 is Integer which")))
                .hasMessage("expect a value or expression")
                .hasFieldOrPropertyWithValue("position", 18);
    }
}
