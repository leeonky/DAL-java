package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.github.leeonky.dal.parser.SourceCodeMatcher.createSourceCodeMatcher;
import static com.github.leeonky.dal.parser.TokenParser.*;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SourceCodeMatcherTest {

    private TokenParser createContext(char c) {
        return new TokenParser(new SourceCode(String.valueOf(c)));
    }

    @Nested
    class CodeMatches {

        @Test
        void digital() {
            assertThat(allCharMatchesBy(TokenParser.DIGITAL))
                    .isEqualTo(new HashSet<>(asList('1', '2', '3', '4', '5', '6', '7', '8', '9', '0')));
        }

        @Test
        void delimiter() {
            assertThat(allCharMatchesBy(TokenParser.DELIMITER))
                    .isEqualTo(Constants.TOKEN_DELIMITER);
        }

        @Test
        void operator() {
            assertThat(allCharMatchesBy(TokenParser.OPERATOR))
                    .isEqualTo(Constants.OPERATOR_CHAR);
        }

        @Test
        void always() {
            assertTrue(ANY_CHARACTERS.matches(createContext('a')));
            assertTrue(ANY_CHARACTERS.matches(createContext('1')));
        }

        @Test
        void one_specified_char() {
            assertThat(allCharMatchesBy(TokenParser.CHARACTER('a')))
                    .isEqualTo(new HashSet<>(singletonList('a')));
        }

        private Set<Character> allCharMatchesBy(SourceCodeMatcher matcher) {
            return IntStream.range(0, Character.MAX_VALUE)
                    .mapToObj(c -> (char) c)
                    .filter(c -> matcher.matches(createContext(c)))
                    .collect(Collectors.toSet());
        }
    }

    @Nested
    class LogicOperation {
        public static final boolean MATCH = true;

        private SourceCodeMatcher constMatcher(final boolean match) {
            return createSourceCodeMatcher(parser -> match);
        }

        @Nested
        class When {

            @Test
            void return_match_when_matches_and_when_condition_matches() {
                assertThat(constMatcher(MATCH).when(constMatcher(MATCH)).matches(null)).isEqualTo(MATCH);
            }

            @Test
            void return_not_matches_when_any_of_matcher_and_when_condition_matcher_not_matches() {
                assertThat(constMatcher(!MATCH).when(constMatcher(MATCH)).matches(null)).isEqualTo(!MATCH);
                assertThat(constMatcher(MATCH).when(constMatcher(!MATCH)).matches(null)).isEqualTo(!MATCH);
                assertThat(constMatcher(!MATCH).when(constMatcher(!MATCH)).matches(null)).isEqualTo(!MATCH);
            }
        }

        @Nested
        class Except {
            @Test
            void return_match_when_matches_and_not_exception() {
                assertThat(constMatcher(MATCH).except(constMatcher(!MATCH)).matches(null)).isEqualTo(MATCH);
            }

            @Test
            void return_not_match_when_matches_but_has_exception() {
                assertThat(constMatcher(MATCH).except(constMatcher(MATCH)).matches(null)).isEqualTo(!MATCH);
            }

            @Test
            void return_not_match_when_not_matches_no_matter_exception() {
                assertThat(constMatcher(!MATCH).except(constMatcher(MATCH)).matches(null)).isEqualTo(!MATCH);
                assertThat(constMatcher(!MATCH).except(constMatcher(!MATCH)).matches(null)).isEqualTo(!MATCH);
            }
        }

        @Nested
        class Or {

            @Test
            void return_true() {
                assertThat(createSourceCodeMatcher(tokenParser -> true)
                        .or(createSourceCodeMatcher(tokenParser -> false))
                        .matches(null)).isTrue();

                assertThat(createSourceCodeMatcher(tokenParser -> false)
                        .or(createSourceCodeMatcher(tokenParser -> true))
                        .matches(null)).isTrue();

                assertThat(createSourceCodeMatcher(tokenParser -> true)
                        .or(createSourceCodeMatcher(tokenParser -> true))
                        .matches(null)).isTrue();
            }

            @Test
            void return_false() {
                assertThat(createSourceCodeMatcher(tokenParser -> false)
                        .or(createSourceCodeMatcher(tokenParser -> false))
                        .matches(null)).isFalse();
            }
        }
    }

    @Nested
    class StatusMatches {

        @Nested
        class AfterCode {
            private TokenParser givenParsedCode(String code) {
                TokenParser parser = new TokenParser(new SourceCode(code));
                for (char c : code.toCharArray())
                    included(ANY_CHARACTERS).matches(parser);
                return parser;
            }

            @Test
            void matches_after_same_code_string() {
                TokenParser parser = givenParsedCode("abc");

                assertThat(after("abc").matches(parser)).isTrue();
                assertThat(after("ab").matches(parser)).isFalse();
                assertThat(after("abcd").matches(parser)).isFalse();
            }
        }

        @Nested
        class AfterToken {

            @Test
            void dose_not_match_when_last_token_is_null() {
                TokenParser parser = new TokenParser(null);

                assertThat(after(token -> true).matches(parser)).isFalse();
            }

            @Test
            void dose_not_match_when_last_token_is_not_matches() {
                TokenParser parser = givenLastToken(Token.constValueToken(100));

                assertThat(after(token -> (int) token.getValue() != 100).matches(parser)).isFalse();
            }

            @Test
            void matches_when_last_token_matches() {
                TokenParser parser = givenLastToken(Token.constValueToken(100));

                assertThat(after(token -> (int) token.getValue() == 100).matches(parser)).isTrue();
            }

            private TokenParser givenLastToken(Token token) {
                TokenStream tokenStream = new TokenStream();
                tokenStream.appendToken(token);
                return new TokenParser(null, tokenStream);
            }
        }
    }
}
