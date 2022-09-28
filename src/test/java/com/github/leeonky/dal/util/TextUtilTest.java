package com.github.leeonky.dal.util;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class TextUtilTest {

    @Test
    void string_to_lines() {
        assertThat(TextUtil.lines("a")).containsExactly("a");
    }

    @Test
    void _r_n_to_lines() {
        assertThat(TextUtil.lines("a\r\nb\r\nc")).containsExactly("a", "b", "c");
    }

    @Test
    void _n_r_to_lines() {
        assertThat(TextUtil.lines("a\n\rb\n\rc")).containsExactly("a", "b", "c");
    }

    @Test
    void _n_to_lines() {
        assertThat(TextUtil.lines("a\nb\nc")).containsExactly("a", "b", "c");
    }

    @Test
    void _r_to_lines() {
        assertThat(TextUtil.lines("a\rb\rc")).containsExactly("a", "b", "c");
    }

    @Test
    void string_lines() {
        assertThat(TextUtil.lines("a\rb\r\nc\n\rd\ne")).containsExactly("a", "b", "c", "d", "e");
    }
}
