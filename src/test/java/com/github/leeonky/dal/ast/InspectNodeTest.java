package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.DALCompiler;
import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class InspectNodeTest {

    private void inspectAssert(String sourceCode, String expected) {
        assertThat(new DALCompiler().compile(new SourceCode(sourceCode)).inspect()).isEqualTo(expected);
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
    void support_inspect_type_assert() {
        inspectAssert("'' is String which .size=0", "'' is String which .size = 0");
    }
}
