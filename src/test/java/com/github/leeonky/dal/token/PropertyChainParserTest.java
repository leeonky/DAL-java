package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class PropertyChainParserTest extends ParserTestBase {

    @Override
    protected TokenParser createParser() {
        return new PropertyChainParser();
    }

    @Nested
    class Parse {

        @Test
        void parse_property() {
            assertString(".a ", "a");
        }

        @Test
        void support_skip_white_space() {
            assertString(". a ", "a");
            assertString(".\na ", "a");
            assertString(".\ta ", "a");
        }

        @Test
        void parse_property_chain() {
            assertString(".a.b ", "a.b");
        }
    }

    @Nested
    class CanFinish {

        @Test
        void do_not_allow_get_value_when_parser_not_start() {
            assertThrows(IllegalStateException.class, () -> parse(""));
        }

        @Test
        void do_not_allow_get_value_when_property_chain_is_empty() {
            assertThrows(IllegalStateException.class, () -> parse("."));
            assertThrows(IllegalStateException.class, () -> parse(". "));
        }

        @Test
        void allow_get_value_when_property_chain_has_value() {
            assertString(".a", "a");
        }
    }

    @Nested
    class IsFinished {

        @Test
        void should_raise_error_when_parser_finished() {
            TokenParser parser = createParserWithCode(".a ");

            assertThrows(IllegalArgumentException.class, () -> parser.feed(' '));
        }

        @Test
        void not_finished_with_out_unexpected_char() {
            assertTrue(createParser().feed('.'));
            assertTrue(createParserWithCode(".").feed('a'));
            assertFalse(createParserWithCode(".a").feed(' '));
            assertFalse(createParserWithCode(".a").feed('\t'));
            assertFalse(createParserWithCode(".a").feed('\n'));
            assertFalse(createParserWithCode(".a").feed('('));
            assertFalse(createParserWithCode(".a").feed(')'));
            assertFalse(createParserWithCode(".a").feed('='));
            assertFalse(createParserWithCode(".a").feed('>'));
            assertFalse(createParserWithCode(".a").feed('<'));
            assertFalse(createParserWithCode(".a").feed('+'));
            assertFalse(createParserWithCode(".a").feed('-'));
            assertFalse(createParserWithCode(".a").feed('*'));
            assertFalse(createParserWithCode(".a").feed('/'));
            assertFalse(createParserWithCode(".a").feed('&'));
            assertFalse(createParserWithCode(".a").feed('|'));
            assertFalse(createParserWithCode(".a").feed('!'));
            assertFalse(createParserWithCode(".a").feed('['));
            assertFalse(createParserWithCode(".a").feed(']'));
        }
    }
}
