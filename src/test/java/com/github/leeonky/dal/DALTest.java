package com.github.leeonky.dal;

import com.github.leeonky.dal.extensions.DALExtension;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DALTest {

    @Test
    void support_extend() {
        DALExtension.extensionForTest = dal ->
                dal.getRuntimeContextBuilder().registerSchema("ExtensionSchema", (a, c) -> true);

        assertThat(new DAL().extend().<String>evaluate("input", "is ExtensionSchema")).isEqualTo("input");
    }
}