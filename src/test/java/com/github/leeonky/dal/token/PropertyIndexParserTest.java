package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PropertyIndexParserTest {
    TokenParser parser = new PropertyIndexParser();

    private void assertPropertyIndex(String code, String expect) {
        TokenParser parser = new PropertyIndexParser();
        for (char c : code.toCharArray())
            parser.feed(c);
        assertThat(parser.value()).isEqualTo(expect);
    }

    @Test
    void parse_empty() {
        assertPropertyIndex("]", "");
    }

    @Test
    void parse_any_text() {
        assertPropertyIndex("a]", "a");
        assertPropertyIndex(" ]", " ");
    }

    @Test
    void right_bracket_is_mandatory() {
        assertFalse(parser.canFinish());
    }

    @Test
    void can_finish_when_have_content() {
        parser.feed(' ');
        assertFalse(parser.canFinish());
    }

    @Test
    void can_finish_when_empty() {
        parser.feed(']');
        assertTrue(parser.canFinish());
    }

    @Test
    void not_finished() {
        assertTrue(parser.feed('a'));
    }

    @Test
    void finished() {
        assertFalse(parser.feed(']'));
    }
}
