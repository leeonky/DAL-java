package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.DALCompiler;
import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.github.leeonky.dal.parser.ParsingContext.ANY_CHARACTERS;
import static com.github.leeonky.dal.parser.ParsingContext.included;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SourceCodeMatcherTest {

    private ParsingContext createContext(char c) {
        return new ParsingContext(new SourceCode(String.valueOf(c)), null);
    }

    @Nested
    class CodeMatches {

        @Test
        void digital() {
            assertThat(allCharMatchesBy(ParsingContext.DIGITAL))
                    .isEqualTo(new HashSet<>(asList('1', '2', '3', '4', '5', '6', '7', '8', '9', '0')));
        }

        @Test
        void delimiter() {
            assertThat(allCharMatchesBy(ParsingContext.DELIMITER))
                    .isEqualTo(Scanner.TOKEN_DELIMITER);
        }

        @Test
        void operator() {
            assertThat(allCharMatchesBy(ParsingContext.OPERATOR))
                    .isEqualTo(Scanner.OPERATOR_CHAR);
        }

        @Test
        void always() {
            assertTrue(ANY_CHARACTERS.matches(createContext('a')));
            assertTrue(ANY_CHARACTERS.matches(createContext('1')));
        }

        @Test
        void one_specified_char() {
            assertThat(allCharMatchesBy(ParsingContext.CHARACTER('a')))
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
            return SourceCodeMatcher.createSourceCodeMatcher(context -> match);
        }

        @Nested
        class When {

            @Test
            void return_matches_when_matches_and_when_condition_matches() {
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
            void return_matches_when_matches_and_not_exception() {
                assertThat(constMatcher(MATCH).except(constMatcher(!MATCH)).matches(null)).isEqualTo(MATCH);
            }

            @Test
            void return_not_matches_when_matches_but_has_exception() {
                assertThat(constMatcher(MATCH).except(constMatcher(MATCH)).matches(null)).isEqualTo(!MATCH);
            }

            @Test
            void return_not_matches_when_not_matches_no_matter_exception() {
                assertThat(constMatcher(!MATCH).except(constMatcher(MATCH)).matches(null)).isEqualTo(!MATCH);
                assertThat(constMatcher(!MATCH).except(constMatcher(!MATCH)).matches(null)).isEqualTo(!MATCH);
            }
        }
    }

    @Nested
    class StatusMatches {

        @Nested
        class LastTokenOptMatches {

            private boolean givenLastToken(Token last) {
                return ParsingContext.AFTER_TOKEN_MATCHES.matches(new ParsingContext(null, last));
            }

            @Test
            void last_opt_token_matches() {
                assertTrue(givenLastToken(Token.operatorToken(DALCompiler.MATCHES)));
            }

            @Test
            void last_token_invalid() {
                assertFalse(givenLastToken(Token.constValueToken(1)));
            }

            @Test
            void last_token_not_exist() {
                assertFalse(givenLastToken(null));
            }
        }

        @Nested
        class ParsedCodeIsOptMatches {

            @Test
            void parsed_code_is_opt_matches() {
                assertTrue(givenParsedCode(":"));
            }

            @Test
            void no_code() {
                assertFalse(givenParsedCode(""));
            }

            @Test
            void not_matched_code() {
                assertFalse(givenParsedCode("::"));
            }

            private boolean givenParsedCode(String code) {
                ParsingContext context = new ParsingContext(new SourceCode(code), null);
                for (char c : code.toCharArray())
                    included(ANY_CHARACTERS).matches(context);
                return ParsingContext.AFTER_OPERATOR_MATCHES.matches(context);
            }
        }
    }
}