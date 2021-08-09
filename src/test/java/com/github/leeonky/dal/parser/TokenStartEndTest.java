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

    private Object getParsedCode(TokenParser context) {
        return context.parseToken(excluded(ANY_CHARACTERS), ALL_CHARACTERS, before(ANY_CHARACTERS), CONST_STRING_TOKEN).getValue();
    }

    @Nested
    class Include {

        private boolean includedMatches(SourceCodeMatcher character, TokenParser context) {
            return TokenParser.included(character).matches(context);
        }

        @Test
        void should_return_true_when_code_matches() {
            TokenParser context = givenParseContextBySourceCode("a");

            assertThat(includedMatches(CHARACTER('a'), context)).isTrue();
        }

        @Test
        void should_seek_source_code_to_next_char() {
            TokenParser context = givenParseContextBySourceCode("ab");

            includedMatches(CHARACTER('a'), context);

            assertThat(sourceCode.currentChar()).isEqualTo('b');
        }

        @Test
        void should_include_matches_char_in_code() {
            TokenParser context = givenParseContextBySourceCode("abc");

            includedMatches(CHARACTER('a'), context);

            assertThat(getParsedCode(context)).isEqualTo("a");
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            TokenParser context = givenParseContextBySourceCode("a");

            assertThat(includedMatches(CHARACTER('x'), context)).isFalse();
        }
    }

    @Nested
    class Exclude {

        private boolean excludedMatches(SourceCodeMatcher character, TokenParser context) {
            return excluded(character).matches(context);
        }

        @Test
        void should_return_true_when_code_matches() {
            TokenParser context = givenParseContextBySourceCode("a");

            assertThat(excludedMatches(CHARACTER('a'), context)).isTrue();
        }

        @Test
        void should_seek_source_code_to_next_char() {
            TokenParser context = givenParseContextBySourceCode("ab");

            excludedMatches(CHARACTER('a'), context);

            assertThat(sourceCode.currentChar()).isEqualTo('b');
        }

        @Test
        void should_not_include_matches_char_in_code() {
            TokenParser context = givenParseContextBySourceCode("abc");

            excludedMatches(CHARACTER('a'), context);

            assertThat(getParsedCode(context)).isEqualTo("");
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            TokenParser context = givenParseContextBySourceCode("a");

            assertThat(excludedMatches(CHARACTER('x'), context)).isFalse();
        }
    }

    @Nested
    class Before {

        private boolean beforeMatches(SourceCodeMatcher character, TokenParser context) {
            return before(character).matches(context);
        }

        @Test
        void should_return_true_when_code_matches() {
            TokenParser context = givenParseContextBySourceCode("a");

            assertThat(beforeMatches(CHARACTER('a'), context)).isTrue();
        }

        @Test
        void should_keep_matches_source_code_position() {
            TokenParser context = givenParseContextBySourceCode("ab");

            beforeMatches(CHARACTER('a'), context);

            assertThat(sourceCode.currentChar()).isEqualTo('a');
        }

        @Test
        void should_not_include_matches_char_in_code() {
            TokenParser context = givenParseContextBySourceCode("abc");

            beforeMatches(CHARACTER('a'), context);

            assertThat(getParsedCode(context)).isEqualTo("");
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            TokenParser context = givenParseContextBySourceCode("a");

            assertThat(beforeMatches(CHARACTER('x'), context)).isFalse();
        }
    }

    @Nested
    class EndOfCode {

        private boolean matchEndOfCode(TokenParser context) {
            return TokenParser.END_OF_CODE.matches(context);
        }

        @Test
        void should_return_true_when_no_more_code() {
            TokenParser context = givenParseContextBySourceCode("");

            assertThat(matchEndOfCode(context)).isTrue();
        }

        @Test
        void should_return_false_when_has_any_code() {
            TokenParser context = givenParseContextBySourceCode("any code");

            assertThat(matchEndOfCode(context)).isFalse();
        }
    }

    @Nested
    class LogicOr {

        @Test
        void support_logic_or() {
            TokenParser context = givenParseContextBySourceCode("a");

            assertThat(TokenParser.END_OF_CODE.or(TokenParser.included(CHARACTER('a'))).matches(context)).isTrue();
        }
    }

    @Nested
    class ThrowException {

        @Test
        void should_throw_exception_with_message_when_no_more_code() {
            TokenParser context = givenParseContextBySourceCode("");

            assertThat(assertThrows(SyntaxException.class, () ->
                    excluded(OPERATOR).orThrow("error").matches(context)))
                    .hasMessage("error")
                    .hasFieldOrPropertyWithValue("position", 0);
        }
    }
}
