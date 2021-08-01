package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

//TODO word parser
class NumberParserTest extends ParserTestBase {

    @Override
    protected TokenParser createParser() {
        return new NumberParser();
    }

    @Nested
    class Parse {

        @Test
        void get_whole_string() {
            assertString("1 ", "1");
            assertString("1\t", "1");
            assertString("1\n", "1");
            assertString("1(", "1");
            assertString("1)", "1");
            assertString("1=", "1");
            assertString("1>", "1");
            assertString("1<", "1");
            assertString("1+", "1");
            assertString("1-", "1");
            assertString("1*", "1");
            assertString("1/", "1");
            assertString("1&", "1");
            assertString("1|", "1");
            assertString("1!", "1");
            assertString("1[", "1");
            assertString("1]", "1");
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
            assertString("1.1", "1.1");
        }
    }

    @Nested
    class IsFinished {

        @Test
        void should_raise_error_when_parser_finished() {
            TokenParser parser = createParserWithCode("1 ");

            assertThrows(IllegalArgumentException.class, () -> parser.feed(' '));
        }

        @Test
        void not_finished_with_out_unexpected_char() {
            assertTrue(createParser().feed('1'));
            assertTrue(createParserWithCode("1").feed('1'));
            assertFalse(createParserWithCode("0").feed(' '));
            assertFalse(createParserWithCode("0").feed('\t'));
            assertFalse(createParserWithCode("0").feed('\n'));
            assertFalse(createParserWithCode("0").feed('('));
            assertFalse(createParserWithCode("0").feed(')'));
            assertFalse(createParserWithCode("0").feed('='));
            assertFalse(createParserWithCode("0").feed('>'));
            assertFalse(createParserWithCode("0").feed('<'));
            assertFalse(createParserWithCode("0").feed('+'));
            assertFalse(createParserWithCode("0").feed('-'));
            assertFalse(createParserWithCode("0").feed('*'));
            assertFalse(createParserWithCode("0").feed('/'));
            assertFalse(createParserWithCode("0").feed('&'));
            assertFalse(createParserWithCode("0").feed('|'));
            assertFalse(createParserWithCode("0").feed('!'));
            assertFalse(createParserWithCode("0").feed('['));
            assertFalse(createParserWithCode("0").feed(']'));
        }
    }
}
