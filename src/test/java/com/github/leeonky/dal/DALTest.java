package com.github.leeonky.dal;

import com.github.leeonky.dal.compiler.SyntaxException;
import com.github.leeonky.dal.extensions.DALExtension;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class DALTest {

    @Test
    void raise_error_when_give_multi_expression() {
        assertThat(assertThrows(SyntaxException.class, () -> new DAL().evaluate(null, "1 2")))
                .hasMessage("more than one expression")
                .hasFieldOrPropertyWithValue("position", 2);
    }

    @Test
    void support_extend() {
        DALExtension.extensionForTest = dal -> dal.getRuntimeContextBuilder().registerSchema("ExtensionSchema", a -> true);

        assertThat(new DAL().extend().<String>evaluate("input", "is ExtensionSchema")).isEqualTo("input");
    }
}