package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class PropertyIndexParserTest extends ParserTestBase {

    @Override
    protected TokenParser createParser() {
        return new PropertyIndexParser();
    }

    @Nested
    class Parse {

        @Test
        void parse_any_text() {
            assertString("]", "");
            assertString("a]", "a");
            assertString(" ]", " ");
        }
    }

    @Nested
    class CanFinish {

        @Test
        void do_not_allow_get_value_when_parser_not_start() {
            assertThrows(IllegalStateException.class, () -> parse(""));
        }

        @Test
        void dot_not_allow_get_value_when_parser_not_finished() {
            assertThrows(IllegalStateException.class, () -> parse("incomplete string"));
        }
    }

    @Nested
    class IsFinished {

        @Test
        void should_raise_error_when_parser_finished() {
            TokenParser parser = createParserWithCode("]");

            assertThrows(IllegalArgumentException.class, () -> parser.feed(' '));
        }

        @Test
        void not_finished_with_out_unexpected_char() {
            assertTrue(createParser().feed('-'));
            assertTrue(createParserWithCode("-").feed('-'));
            assertFalse(createParserWithCode("-").feed(']'));
            assertFalse(createParser().feed(']'));
        }
    }
}
