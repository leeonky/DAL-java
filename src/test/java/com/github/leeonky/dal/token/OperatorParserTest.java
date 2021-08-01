package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class OperatorParserTest extends ParserTestBase {

    @Override
    protected TokenParser createParser() {
        return new OperatorParser();
    }

    @Nested
    class Parse {

        @Test
        void get_whole_string() {
            assertString("-a", "-");
            assertString("!a", "!");
            assertString("=a", "=");
            assertString(">a", ">");
            assertString("<a", "<");
            assertString("+a", "+");
            assertString("*a", "*");
            assertString("/a", "/");
            assertString("~a", "~");
            assertString(">=a", ">=");
            assertString("<=a", "<=");
            assertString("!=a", "!=");
            assertString("&&a", "&&");
            assertString("||a", "||");
        }

        @Test
        void distinguish_regex_after_operator_matches() {
            assertString("~/", "~");
        }
    }

    @Nested
    class CanFinish {

        @Test
        void do_not_allow_get_value_when_parser_not_start() {
            assertThrows(IllegalStateException.class, () -> parse(""));
        }

        @Test
        void allow_get_value_when_parser_not_finished() {
            assertString("-", "-");
            assertString("!", "!");
            assertString("=", "=");
            assertString(">", ">");
            assertString("<", "<");
            assertString("+", "+");
            assertString("*", "*");
            assertString("/", "/");
            assertString("~", "~");

            assertString(">=", ">=");
            assertString("<=", "<=");
            assertString("!=", "!=");
            assertString("&&", "&&");
            assertString("||", "||");
        }
    }

    @Nested
    class IsFinished {

        @Test
        void should_raise_error_when_parser_finished() {
            TokenParser parser = createParserWithCode("- ");

            assertThrows(IllegalArgumentException.class, () -> parser.feed(' '));
        }

        @Test
        void not_finished_with_out_unexpected_char() {
            assertTrue(createParser().feed('-'));
            assertTrue(createParserWithCode("-").feed('-'));
            assertFalse(createParserWithCode("-").feed('1'));
        }
    }
}
