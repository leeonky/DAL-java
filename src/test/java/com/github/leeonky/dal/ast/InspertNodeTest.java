package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.DALCompiler;
import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class InspertNodeTest {

    @Test
    void input_node_is_empty_string() {
        inspectAssert("", "");
    }

    private void inspectAssert(String sourceCode, String expected) {
        assertThat(new DALCompiler().compile(new SourceCode(sourceCode)).inspect()).isEqualTo(expected);
    }

    @Test
    void const_node_is_value_string() {
        inspectAssert("'hello'", "'hello'");
        inspectAssert("null", "null");
        inspectAssert("100", "100");
    }

    @Test
    void support_inspect_bracket_node() {
        inspectAssert("(100)", "(100)");
    }

    @Test
    void support_inspect_property_node() {
        inspectAssert(".product.name", ".product.name");
        inspectAssert("''.empty", "''.empty");
    }

    @Test
    void support_inspect_expression() {
        inspectAssert("1=1", "1 = 1");
        inspectAssert("1+1", "1 + 1");
        inspectAssert("1-1", "1 - 1");
        inspectAssert("1*1", "1 * 1");
        inspectAssert("1/1", "1 / 1");

        inspectAssert("true&&true", "true && true");
        inspectAssert("true||true", "true || true");

        inspectAssert("1>=1", "1 >= 1");
        inspectAssert("1<=1", "1 <= 1");
        inspectAssert("1!=2", "1 != 2");

        inspectAssert("2>1", "2 > 1");
        inspectAssert("1>2", "1 > 2");
        inspectAssert("!false", "!false");
        inspectAssert("(-1)", "(-1)");
    }

    @Test
    void support_inspect_index_expression() {
        inspectAssert(".lines[0]", ".lines[0]");
    }

    @Test
    void support_inspect_type_assert() {
        inspectAssert("'' is String which .size=0", "'' is String which .size = 0");
    }
}
