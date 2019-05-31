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
            assertThat(new Scanner().scan(new SourceCode("")).allTokens()).isEmpty();
            assertThat(new Scanner().scan(new SourceCode("  ")).allTokens()).isEmpty();
        }

        private void assertGetToken(String sourceCode, Token... tokens) {
            assertThat(new Scanner().scan(new SourceCode(sourceCode)).allTokens()).containsOnly(tokens);
        }

        @Nested
        class NumberToken {

            @Test
            void number_token() {
                assertGetToken("1", constValueToken(new BigDecimal(1)));
                assertGetToken("11", constValueToken(new BigDecimal(11)));
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
                SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> assertGetToken(" [x]", constIndexToken(1)));
                assertThat(syntaxException)
                        .hasMessage("only support const int array index")
                        .hasFieldOrPropertyWithValue("position", 2);
            }

            @Test
            void should_end_with_end_bracket() {
                SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> assertGetToken(" [xx   ", constIndexToken(1)));
                assertThat(syntaxException)
                        .hasMessage("missed ']'")
                        .hasFieldOrPropertyWithValue("position", 7);

                "aue".charAt(0);
            }
        }

        @Nested
        class PropertyToken {

            @Test
            void single_property_token() {
                assertGetToken(".a", propertyToken("a"));
            }

            @Test
            void property_chain_token() {
                assertGetToken(".a.x", propertyToken("a", "x"));
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
                assertGetToken("(0)", beginBracketToken(), constValueToken(new BigDecimal(0)), endBracketToken());
            }
        }

        @Nested
        class SingleQuotationStringToken {

            @Test
            void begin_with_single_quotation() {
                assertGetToken("'a''b'", constValueToken("a"), constValueToken("b"));
            }

            @Test
            void should_end_with_end_single_quotation() {
                SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> assertGetToken(" 'xx   "));
                assertThat(syntaxException)
                        .hasMessage("string should end with '\''")
                        .hasFieldOrPropertyWithValue("position", 7);
            }

            @Test
            void string_contains_blank() {
                assertGetToken("'  '", constValueToken("  "));
                assertGetToken("' a '", constValueToken(" a "));
            }
        }

        @Nested
        class DoubleQuotationStringToken {

            @Test
            void no_escape_char() {
                assertGetToken("\"a\"", constValueToken("a"));
            }

            @Test
            void should_end_with_end_single_quotation() {
                SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> assertGetToken(" \"xx   "));
                assertThat(syntaxException)
                        .hasMessage("string should end with '\"'")
                        .hasFieldOrPropertyWithValue("position", 7);
            }

            @Test
            void escape_char() {
                assertGetToken("\"\\\"\"", constValueToken("\""));
                assertGetToken("\"\\t\"", constValueToken("\t"));
                assertGetToken("\"\\n\"", constValueToken("\n"));
                assertGetToken("\"\\\\\"", constValueToken("\\"));

                assertGetToken("\"\\n\\n\"", constValueToken("\n\n"));
            }

            @Test
            void unfinished_escape_char() {
                SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> assertGetToken("\"a\\"));

                assertThat(syntaxException)
                        .hasMessage("string should end with '\"'")
                        .hasFieldOrPropertyWithValue("position", 3);
            }

            @Test
            void unsupported_escape_char() {
                SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> assertGetToken("\"a\\a"));

                assertThat(syntaxException)
                        .hasMessage("unsupported escape char")
                        .hasFieldOrPropertyWithValue("position", 3);
            }

            @Test
            void string_contains_blank() {
                assertGetToken("\"  \"", constValueToken("  "));
                assertGetToken("\" a \"", constValueToken(" a "));
            }
        }

        @Nested
        class SplitTokens {

            @Test
            void split_number() {
                assertGetToken("11 12", constValueToken(new BigDecimal(11)), constValueToken(new BigDecimal(12)));
                assertGetToken("11\t", constValueToken(new BigDecimal(11)));
                assertGetToken("11\n", constValueToken(new BigDecimal(11)));

                assertGetToken("11=", constValueToken(new BigDecimal(11)), operatorToken("="));
                assertGetToken("11>", constValueToken(new BigDecimal(11)), operatorToken(">"));
                assertGetToken("11<", constValueToken(new BigDecimal(11)), operatorToken("<"));
                assertGetToken("11-", constValueToken(new BigDecimal(11)), operatorToken("-"));
                assertGetToken("11+", constValueToken(new BigDecimal(11)), operatorToken("+"));
                assertGetToken("11*", constValueToken(new BigDecimal(11)), operatorToken("*"));
                assertGetToken("11/", constValueToken(new BigDecimal(11)), operatorToken("/"));
                assertGetToken("11|", constValueToken(new BigDecimal(11)), operatorToken("|"));
                assertGetToken("11&", constValueToken(new BigDecimal(11)), operatorToken("&"));
                assertGetToken("11||", constValueToken(new BigDecimal(11)), operatorToken("||"));
                assertGetToken("11&&", constValueToken(new BigDecimal(11)), operatorToken("&&"));

                assertGetToken("11(", constValueToken(new BigDecimal(11)), beginBracketToken());
                assertGetToken("11)", constValueToken(new BigDecimal(11)), endBracketToken());

                assertGetToken("11[1]", constValueToken(new BigDecimal(11)), constIndexToken(1));
            }

            @Test
            void split_word_token() {
                assertGetToken("a b", wordToken("a"), wordToken("b"));

                assertGetToken("a=", wordToken("a"), operatorToken("="));

                assertGetToken("a[0]", wordToken("a"), constIndexToken(0));

                assertGetToken("a(", wordToken("a"), beginBracketToken());
                assertGetToken("a)", wordToken("a"), endBracketToken());
            }

            @Test
            void split_property_token() {
                assertGetToken(".a .b", propertyToken("a"), propertyToken("b"));

                assertGetToken(".a[1]", propertyToken("a"), constIndexToken(1));

                assertGetToken(".a=", propertyToken("a"), operatorToken("="));

                assertGetToken(".a(", propertyToken("a"), beginBracketToken());
                assertGetToken(".a)", propertyToken("a"), endBracketToken());
            }

            @Test
            void split_operator_token() {
                assertGetToken("= =", operatorToken("="), operatorToken("="));

                assertGetToken("==", operatorToken("="), operatorToken("="));

                assertGetToken("=1", operatorToken("="), constValueToken(new BigDecimal(1)));

                assertGetToken("=a", operatorToken("="), wordToken("a"));

                assertGetToken("=(", operatorToken("="), beginBracketToken());
                assertGetToken("=)", operatorToken("="), endBracketToken());

                assertGetToken("=.a", operatorToken("="), propertyToken("a"));

                assertGetToken("=[0]", operatorToken("="), constIndexToken(0));

                assertGetToken("='0'", operatorToken("="), constValueToken("0"));

                assertGetToken("=\"0\"", operatorToken("="), constValueToken("0"));
            }
        }
    }
}