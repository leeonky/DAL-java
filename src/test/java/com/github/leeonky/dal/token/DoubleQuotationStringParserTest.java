package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DoubleQuotationStringParserTest {

    private void assertString(String code, String expect) {
        TokenParser parser = new DoubleQuotationStringParser();
        for (char c : code.toCharArray())
            parser.feed(c);
        assertThat(parser.value()).isEqualTo(expect);
    }

    @Test
    void empty_string() {
        assertString("\"\"", "");
    }

    @Test
    void complex_string() {
        assertString("\"a\"", "a");
        assertString("\"abc\"", "abc");
        assertString("\" \"", " ");
    }

    @Test
    void escape_char() {
        assertString("\"\\\\\"", "\\");
        assertString("\"\\t\"", "\t");
        assertString("\"\\n\"", "\n");
        assertString("\"\\\"\"", "\"");
    }

    @Test
    void keep_origin_when_not_supported_escape_char() {
        assertString("\"\\h\"", "\\h");
    }
}