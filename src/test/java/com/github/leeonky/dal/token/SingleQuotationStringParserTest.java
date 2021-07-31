package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class SingleQuotationStringParserTest {
    SingleQuotationStringParser parser = new SingleQuotationStringParser();

    private void assertString(String code, String expect) {
        SingleQuotationStringParser parser = new SingleQuotationStringParser();
        for (char c : code.toCharArray())
            parser.feed(c);
        assertThat(parser.value()).isEqualTo(expect);
    }

    @Test
    void empty_string() {
        assertString("''", "");
    }

    @Test
    void one_char() {
        assertString("'a'", "a");
    }

    @Test
    void escape_char() {
        assertString("'\\''", "'");
        assertString("'\\\\'", "\\");
    }

    @Test
    void no_escape_char() {
        assertString("'\\a'", "\\a");
    }

    @Test
    void return_true_when_string_finish() {
        assertTrue(parser.feed('\''));
        assertTrue(parser.feed('a'));
        assertFalse(parser.feed('\''));
    }

    @Test
    void should_raise_error_when_no_data() {
        assertThrows(IllegalStateException.class, () -> assertString("", "any"));
        assertThrows(IllegalStateException.class, () -> assertString("'", "any"));
    }

    @Test
    void should_raise_error_when_feed_more_than_one_string() {
        assertThrows(IllegalArgumentException.class, () -> assertString("'a''b'", "any"));
    }
}