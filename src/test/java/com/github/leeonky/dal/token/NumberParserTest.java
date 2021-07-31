package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class NumberParserTest {
    NumberParser parser = new NumberParser();

    private void assertNumber(String code, String expect) {
        NumberParser parser = new NumberParser();
        for (char c : code.toCharArray())
            parser.feed(c);
        assertThat(parser.value()).isEqualTo(expect);
    }

    @Test
    void parse_integer() {
        assertNumber("1", "1");
    }

    @Test
    void parse_double() {
        assertNumber("1.1", "1.1");
    }

    @Test
    void token_split_after_parse_integer() {
        assertNumber("1 ", "1");
        assertNumber("1\t", "1");
        assertNumber("1\n", "1");
        assertNumber("1(", "1");
        assertNumber("1)", "1");
        assertNumber("1=", "1");
        assertNumber("1>", "1");
        assertNumber("1<", "1");
        assertNumber("1+", "1");
        assertNumber("1-", "1");
        assertNumber("1*", "1");
        assertNumber("1/", "1");
        assertNumber("1&", "1");
        assertNumber("1|", "1");
        assertNumber("1!", "1");
        assertNumber("1[", "1");
        assertNumber("1]", "1");
    }

    @Test
    void not_finished_status() {
        assertTrue(parser.feed('1'));
    }

    @Test
    void finished_status() {
        parser.feed('1');
        assertFalse(parser.feed(' '));
    }

    @Test
    void should_not_finish() {
        assertFalse(parser.canFinish());
    }

    @Test
    void can_be_finished() {
        parser.feed('1');
        assertTrue(parser.canFinish());
    }
}
