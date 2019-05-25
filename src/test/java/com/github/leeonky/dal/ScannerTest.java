package com.github.leeonky.dal;

import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static com.github.leeonky.dal.token.Token.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ScannerTest {
    @Nested
    class GetToken {

        @Test
        void empty_code() {
            assertThat(new Scanner().scan(new SourceCode(""))).isEmpty();
            assertThat(new Scanner().scan(new SourceCode("  "))).isEmpty();
        }

        private void assertGetToken(String sourceCode, Token... tokens) {
            assertThat(new Scanner().scan(new SourceCode(sourceCode))).containsOnly(tokens);
        }

        @Nested
        class NumberToken {

            @Test
            void number_token() {
                assertGetToken("1", numberToken(new BigDecimal(1)));
                assertGetToken("11", numberToken(new BigDecimal(11)));
            }
        }

        @Nested
        class WordToken {

            @Test
            void single_word_token() {
                assertGetToken("ab", wordToken("ab"));
            }

            @Test
            void nested_word_token() {
                assertGetToken("a.b", wordToken("a.b"));
            }
        }

        @Nested
        class ConstValueToken {

            @Test
            void get_item_by_index() {
                assertGetToken("[1]", constIndexToken(1));
            }

            @Test
            void item_token_end_char() {
                assertGetToken("[1][2]", constIndexToken(1), constIndexToken(2));
            }

            @Test
            void only_support_int_index() {
                SyntexException syntexException = assertThrows(SyntexException.class, () -> assertGetToken(" [x]", constIndexToken(1)));
                assertThat(syntexException)
                        .hasMessage("only support const int array index")
                        .hasFieldOrPropertyWithValue("position", 2);
            }

            @Test
            void should_end_with_end_bracket() {
                SyntexException syntexException = assertThrows(SyntexException.class, () -> assertGetToken(" [xx   ", constIndexToken(1)));
                assertThat(syntexException)
                        .hasMessage("missed ']'")
                        .hasFieldOrPropertyWithValue("position", 4);

                "aue".charAt(0);
            }
        }

        @Nested
        class PropertyToken {

            @Test
            void single_property_token() {
                assertGetToken(".a", propertyToken(".a"));
            }

            @Test
            void property_chain_token() {
                assertGetToken(".a.x", propertyToken(".a.x"));
            }
        }

        @Nested
        class OperatorToken {
            @Test
            void supported_single_char_operators() {
                assertGetToken("=", operatorToken("="));
                assertGetToken(">", operatorToken(">"));
                assertGetToken("<", operatorToken("<"));
                assertGetToken("-", operatorToken("-"));
                assertGetToken("+", operatorToken("+"));
                assertGetToken("*", operatorToken("*"));
                assertGetToken("/", operatorToken("/"));
            }

            @Test
            void supported_two_chars_operators() {
                assertGetToken(">=", operatorToken(">="));
                assertGetToken("<=", operatorToken("<="));
                assertGetToken("&&", operatorToken("&&"));
                assertGetToken("||", operatorToken("||"));
                assertGetToken("!=", operatorToken("!="));
            }
        }

        @Nested
        class BracketToken {
            @Test
            void begin_end_bracket_token() {
                assertGetToken("(", beginBracketToken());
                assertGetToken(")", endBracketToken());
            }
        }

        @Nested
        class SingleQuotationStringToken {

            @Test
            void begin_with_single_quotation() {
                assertGetToken("'a'", stringToken("a"));
                assertGetToken("'a''b'", stringToken("a"), stringToken("b"));
            }

            @Test
            void should_end_with_end_single_quotation() {
                SyntexException syntexException = assertThrows(SyntexException.class, () -> assertGetToken(" 'xx   ", stringToken("xx")));
                assertThat(syntexException)
                        .hasMessage("string should end with '\''")
                        .hasFieldOrPropertyWithValue("position", 4);
            }
        }

        @Nested
        class DoubleQuotationStringToken {

            @Test
            void no_escape_char() {
                assertGetToken("\"a\"", stringToken("a"));
            }

            @Test
            void should_end_with_end_single_quotation() {
                SyntexException syntexException = assertThrows(SyntexException.class, () -> assertGetToken(" \"xx   ", stringToken("xx")));
                assertThat(syntexException)
                        .hasMessage("string should end with '\"'")
                        .hasFieldOrPropertyWithValue("position", 4);
            }
        }

        @Nested
        class SplitTokens {

            @Test
            void split_number() {
                assertGetToken("11 ", numberToken(new BigDecimal(11)));
                assertGetToken("11\t", numberToken(new BigDecimal(11)));
                assertGetToken("11\n", numberToken(new BigDecimal(11)));

                assertGetToken("11=", numberToken(new BigDecimal(11)), operatorToken("="));
                assertGetToken("11>", numberToken(new BigDecimal(11)), operatorToken(">"));
                assertGetToken("11<", numberToken(new BigDecimal(11)), operatorToken("<"));
                assertGetToken("11-", numberToken(new BigDecimal(11)), operatorToken("-"));
                assertGetToken("11+", numberToken(new BigDecimal(11)), operatorToken("+"));
                assertGetToken("11*", numberToken(new BigDecimal(11)), operatorToken("*"));
                assertGetToken("11/", numberToken(new BigDecimal(11)), operatorToken("/"));
                assertGetToken("11|", numberToken(new BigDecimal(11)), operatorToken("|"));
                assertGetToken("11&", numberToken(new BigDecimal(11)), operatorToken("&"));
                assertGetToken("11||", numberToken(new BigDecimal(11)), operatorToken("||"));
                assertGetToken("11&&", numberToken(new BigDecimal(11)), operatorToken("&&"));

                assertGetToken("11(", numberToken(new BigDecimal(11)), beginBracketToken());

                assertGetToken("11[1]", numberToken(new BigDecimal(11)), constIndexToken(1));
            }

            @Test
            void split_word_token() {
                assertGetToken("a ", wordToken("a"));

                assertGetToken("a=", wordToken("a"), operatorToken("="));

                assertGetToken("a[0]", wordToken("a"), constIndexToken(0));

                assertGetToken("a(", wordToken("a"), beginBracketToken());
            }

            @Test
            void split_property_token() {
                assertGetToken(".a ", propertyToken(".a"));

                assertGetToken(".a[1]", propertyToken(".a"), constIndexToken(1));

                assertGetToken(".a=", propertyToken(".a"), operatorToken("="));

                assertGetToken(".a(", propertyToken(".a"), beginBracketToken());
            }

            @Test
            void split_operator_token() {
                assertGetToken("= ", operatorToken("="));

                assertGetToken("==", operatorToken("="), operatorToken("="));

                assertGetToken("=1", operatorToken("="), numberToken(new BigDecimal(1)));

                assertGetToken("=a", operatorToken("="), wordToken("a"));

                assertGetToken("=(", operatorToken("="), beginBracketToken());

                assertGetToken("=.a", operatorToken("="), propertyToken(".a"));

                assertGetToken("=[0]", operatorToken("="), constIndexToken(0));
            }
        }
    }
}