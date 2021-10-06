package com.github.leeonky.dal.util;

import com.github.leeonky.dal.compiler.Compiler;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class CodeHelperTest {
    @Test
    void parse_property_chain_when_start_with_property_name() {
        assertThat(new Compiler().toChainNodes("a.b[1]")).containsExactly("a", "b", 1);
    }

    @Test
    void parse_property_chain_when_start_with_bracket_property() {
        assertThat(new Compiler().toChainNodes("[' a '].b")).containsExactly(" a ", "b");
    }
}
