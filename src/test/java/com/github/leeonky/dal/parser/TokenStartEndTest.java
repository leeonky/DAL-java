package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.parser.TokenContentInString.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenParser.*;
import static com.github.leeonky.dal.parser.TokenStartEnd.before;
import static com.github.leeonky.dal.token.TokenFactory.CONST_STRING_TOKEN;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TokenStartEndTest {

    private SourceCode sourceCode;

    private TokenParser givenParseContextBySourceCode(String sourceCode) {
        return new TokenParser(this.sourceCode = new SourceCode(sourceCode));
    }

    private Object getParsedCode(TokenParser parser) {
        return parser.parseToken(excluded(ANY_CHARACTERS), ALL_CHARACTERS, before(ANY_CHARACTERS), CONST_STRING_TOKEN).getValue();
    }

    @Nested
    class Include {

        private boolean includedMatches(SourceCodeMatcher character, TokenParser parser) {
            return TokenParser.included(character).matches(parser);
        }

        @Test
        void should_return_true_when_code_matches() {
            TokenParser parser = givenParseContextBySourceCode("a");

            assertThat(includedMatches(CHARACTER('a'), parser)).isTrue();
        }

        @Test
        void should_seek_source_code_to_next_char() {
            TokenParser parser = givenParseContextBySourceCode("ab");

            includedMatches(CHARACTER('a'), parser);

            assertThat(sourceCode.currentChar()).isEqualTo('b');
        }

        @Test
        void should_include_matched_char_in_code() {
            TokenParser parser = givenParseContextBySourceCode("abc");

            includedMatches(CHARACTER('a'), parser);

            assertThat(getParsedCode(parser)).isEqualTo("a");
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            TokenParser parser = givenParseContextBySourceCode("a");

            assertThat(includedMatches(CHARACTER('x'), parser)).isFalse();
        }
    }

    @Nested
    class Exclude {

        private boolean excludedMatches(SourceCodeMatcher character, TokenParser parser) {
            return excluded(character).matches(parser);
        }

        @Test
        void should_return_true_when_code_matches() {
            TokenParser parser = givenParseContextBySourceCode("a");

            assertThat(excludedMatches(CHARACTER('a'), parser)).isTrue();
        }

        @Test
        void should_seek_source_code_to_next_char() {
            TokenParser parser = givenParseContextBySourceCode("ab");

            excludedMatches(CHARACTER('a'), parser);

            assertThat(sourceCode.currentChar()).isEqualTo('b');
        }

        @Test
        void should_not_include_matched_char_in_code() {
            TokenParser parser = givenParseContextBySourceCode("abc");

            excludedMatches(CHARACTER('a'), parser);

            assertThat(getParsedCode(parser)).isEqualTo("");
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            TokenParser parser = givenParseContextBySourceCode("a");

            assertThat(excludedMatches(CHARACTER('x'), parser)).isFalse();
        }
    }

    @Nested
    class Before {

        private boolean beforeMatches(SourceCodeMatcher character, TokenParser parser) {
            return before(character).matches(parser);
        }

        @Test
        void should_return_true_when_code_matches() {
            TokenParser parser = givenParseContextBySourceCode("a");

            assertThat(beforeMatches(CHARACTER('a'), parser)).isTrue();
        }

        @Test
        void should_keep_matcher_source_code_position() {
            TokenParser parser = givenParseContextBySourceCode("ab");

            beforeMatches(CHARACTER('a'), parser);

            assertThat(sourceCode.currentChar()).isEqualTo('a');
        }

        @Test
        void should_not_include_matcher_char_in_code() {
            TokenParser parser = givenParseContextBySourceCode("abc");

            beforeMatches(CHARACTER('a'), parser);

            assertThat(getParsedCode(parser)).isEqualTo("");
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            TokenParser parser = givenParseContextBySourceCode("a");

            assertThat(beforeMatches(CHARACTER('x'), parser)).isFalse();
        }
    }

    @Nested
    class EndOfCode {

        private boolean matchEndOfCode(TokenParser parser) {
            return TokenParser.END_OF_CODE.matches(parser);
        }

        @Test
        void should_return_true_when_no_more_code() {
            TokenParser parser = givenParseContextBySourceCode("");

            assertThat(matchEndOfCode(parser)).isTrue();
        }

        @Test
        void should_return_false_when_has_any_code() {
            TokenParser parser = givenParseContextBySourceCode("any code");

            assertThat(matchEndOfCode(parser)).isFalse();
        }
    }

    @Nested
    class LogicOr {

        @Test
        void support_logic_or() {
            TokenParser parser = givenParseContextBySourceCode("a");

            assertThat(TokenParser.END_OF_CODE.or(TokenParser.included(CHARACTER('a'))).matches(parser)).isTrue();
        }
    }

    @Nested
    class ThrowException {

        @Test
        void should_throw_exception_with_message_when_no_more_code() {
            TokenParser parser = givenParseContextBySourceCode("");

            assertThat(assertThrows(SyntaxException.class, () ->
                    excluded(OPERATOR).orThrow("error").matches(parser)))
                    .hasMessage("error")
                    .hasFieldOrPropertyWithValue("position", 0);
        }
    }
}
