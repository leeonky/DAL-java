package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class RegexParserTest {

    private void assertString(String code, String expect) {
        TextParser parser = new RegexParser();
        for (char c : code.toCharArray())
            parser.feed(c);
        assertThat(parser.value()).isEqualTo(expect);
    }

    @Test
    void empty_pattern() {
        assertString("//", "");
    }

    @Test
    void complex_pattern() {
        assertString("/a/", "a");
        assertString("/[0-9]/", "[0-9]");
    }

    @Test
    void escape_char() {
        assertString("/\\\\/", "\\");
        assertString("/\\//", "/");
    }

    @Test
    void keep_origin_when_not_supported_escape_char() {
        assertString("/\\h/", "\\h");
    }
}
