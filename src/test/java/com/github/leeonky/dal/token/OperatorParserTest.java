package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OperatorParserTest {
    OperatorParser parser = new OperatorParser();

    private void assertOperator(String code, String expect) {
        OperatorParser parser = new OperatorParser();
        for (char c : code.toCharArray())
            parser.feed(c);
        assertThat(parser.value()).isEqualTo(expect);
    }

    @Test
    void code_end_with_operator() {
        assertOperator("-", "-");
        assertOperator("!", "!");
        assertOperator("=", "=");
        assertOperator(">", ">");
        assertOperator("<", "<");
        assertOperator("+", "+");
        assertOperator("*", "*");
        assertOperator("/", "/");
        assertOperator("~", "~");

        assertOperator(">=", ">=");
        assertOperator("<=", "<=");
        assertOperator("!=", "!=");
        assertOperator("&&", "&&");
        assertOperator("||", "||");
    }

    @Test
    void operator_before_opt() {
        assertOperator("-a", "-");
        assertOperator("!a", "!");
        assertOperator("=a", "=");
        assertOperator(">a", ">");
        assertOperator("<a", "<");
        assertOperator("+a", "+");
        assertOperator("*a", "*");
        assertOperator("/a", "/");
        assertOperator("~a", "~");

        assertOperator(">=a", ">=");
        assertOperator("<=a", "<=");
        assertOperator("!=a", "!=");
        assertOperator("&&a", "&&");
        assertOperator("||a", "||");
    }

    @Test
    void distinguish_matches_regex() {
        assertOperator("~/", "~");
    }

    @Test
    void not_finished_status() {
        assertTrue(parser.feed('-'));
    }

    @Test
    void finished_status() {
        parser.feed('-');
        assertFalse(parser.feed('a'));
    }

    @Test
    void should_not_finish() {
        assertFalse(parser.canFinish());
    }

    @Test
    void can_be_finished() {
        parser.feed('-');
        assertTrue(parser.canFinish());
    }
}
