package com.github.leeonky.dal;

import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import static com.github.leeonky.dal.token.Token.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SourceCodeTest {
    @Nested
    class GetToken {

        @Test
        void empty_code() {
            assertThrows(RuntimeException.class, () -> new SourceCode("").getToken());
            assertThrows(RuntimeException.class, () -> new SourceCode(" ").getToken());
        }

        private void assertGetToken(String sourceCode, Token... tokens) {
            List<Token> actualTokens = new ArrayList<>();
            SourceCode sourceCode1 = new SourceCode(sourceCode);
            while (!sourceCode1.isEnd())
                actualTokens.add(sourceCode1.getToken());
            assertThat(actualTokens).containsOnly(tokens);
        }

        @Nested
        class NumberToken {
            @Test
            void number_token() {
                assertGetToken("1", numebrToken(new BigDecimal(1)));
                assertGetToken("11", numebrToken(new BigDecimal(11)));
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
        class ItemToken {

            @Test
            void get_item_by_index() {
                assertGetToken("[1]", constIndexToken(1));
            }

            @Test
            void item_token_end_char() {
                assertGetToken("[1][2]", constIndexToken(1), constIndexToken(2));
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
                assertGetToken("(", beginBrachetToken());
                assertGetToken(")", endBrachetToken());
            }
        }

        @Nested
        class StringToken {

            @Test
            void begin_with_single_quotation() {
                assertGetToken("'a'", stringToken("a"));
                assertGetToken("'a''b", stringToken("a"), stringToken("b"));
            }
        }

        @Nested
        class SplitTokens {

            @Test
            void split_number() {
                assertGetToken("11 ", numebrToken(new BigDecimal(11)));
                assertGetToken("11\t", numebrToken(new BigDecimal(11)));
                assertGetToken("11\n", numebrToken(new BigDecimal(11)));

                assertGetToken("11=", numebrToken(new BigDecimal(11)), operatorToken("="));
                assertGetToken("11>", numebrToken(new BigDecimal(11)), operatorToken(">"));
                assertGetToken("11<", numebrToken(new BigDecimal(11)), operatorToken("<"));
                assertGetToken("11-", numebrToken(new BigDecimal(11)), operatorToken("-"));
                assertGetToken("11+", numebrToken(new BigDecimal(11)), operatorToken("+"));
                assertGetToken("11*", numebrToken(new BigDecimal(11)), operatorToken("*"));
                assertGetToken("11/", numebrToken(new BigDecimal(11)), operatorToken("/"));
                assertGetToken("11|", numebrToken(new BigDecimal(11)), operatorToken("|"));
                assertGetToken("11&", numebrToken(new BigDecimal(11)), operatorToken("&"));
                assertGetToken("11||", numebrToken(new BigDecimal(11)), operatorToken("||"));
                assertGetToken("11&&", numebrToken(new BigDecimal(11)), operatorToken("&&"));

                assertGetToken("11(", numebrToken(new BigDecimal(11)), beginBrachetToken());

                assertGetToken("11[1]", numebrToken(new BigDecimal(11)), constIndexToken(1));
            }

            @Test
            void split_word_token() {
                assertGetToken("a ", wordToken("a"));

                assertGetToken("a=", wordToken("a"), operatorToken("="));

                assertGetToken("a[0]", wordToken("a"), constIndexToken(0));

                assertGetToken("a(", wordToken("a"), beginBrachetToken());
            }

            @Test
            void split_property_token() {
                assertGetToken(".a ", propertyToken(".a"));

                assertGetToken(".a[1]", propertyToken(".a"), constIndexToken(1));

                assertGetToken(".a=", propertyToken(".a"), operatorToken("="));

                assertGetToken(".a(", propertyToken(".a"), beginBrachetToken());
            }

            @Test
            void split_operator_token() {
                assertGetToken("= ", operatorToken("="));

                assertGetToken("==", operatorToken("="), operatorToken("="));

                assertGetToken("=1", operatorToken("="), numebrToken(new BigDecimal(1)));

                assertGetToken("=a", operatorToken("="), wordToken("a"));

                assertGetToken("=(", operatorToken("="), beginBrachetToken());

                assertGetToken("=.a", operatorToken("="), propertyToken(".a"));

                assertGetToken("=[0]", operatorToken("="), constIndexToken(0));
            }
        }
    }
}