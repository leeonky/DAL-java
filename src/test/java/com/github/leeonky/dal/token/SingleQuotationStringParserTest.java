package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SingleQuotationStringParserTest extends ParserTestBase {

    @Override
    protected TokenParser createParser() {
        return new SingleQuotationStringParser();
    }

    @Nested
    class Parse {

        @Test
        void should_not_contains_quotations() {
            assertString("''", "");
            assertString("'a'", "a");
        }

        @Test
        void escape_char() {
            assertString("'\\''", "'");
            assertString("'\\\\'", "\\");
        }

        @Test
        void keep_origin_when_not_supported_escape_char() {
            assertString("'\\a'", "\\a");
        }
    }

    @Nested
    class CanFinish {

        @Test
        void do_not_allow_get_value_when_parser_not_start() {
            assertThrows(IllegalStateException.class, () -> parse(""));
        }

        @Test
        void do_not_allow_get_value_when_parser_not_finished() {
            assertThrows(IllegalStateException.class, () -> parse("'incomplete string"));
        }
    }

    @Nested
    class IsFinished {

        @Test
        void should_raise_error_when_parser_finished() {
            TokenParser parser = createParserWithCode("''");

            assertThrows(IllegalArgumentException.class, () -> parser.feed(' '));
        }

        @Test
        void not_finished_with_out_feed_last_quotation() {
            assertTrue(createParser().feed('\''));
            assertTrue(createParserWithCode("'").feed('o'));
            assertFalse(createParserWithCode("'").feed('\''));
        }
    }
}