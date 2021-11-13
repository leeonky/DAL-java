package com.github.leeonky.dal.runtime;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.runtime.DalException.Position.Type.CHAR;
import static com.github.leeonky.dal.runtime.DalException.Position.Type.LINE;
import static org.assertj.core.api.Assertions.assertThat;

class DalExceptionTest {

    @Nested
    class Position {

        private void assertShowCode(DalException dalException, StringBuilder code, StringBuilder expected) {
            assertShowCode(dalException, code, expected, 0);
        }

        private void assertShowCode(DalException dalException, StringBuilder code, StringBuilder expected, int offset) {
            assertThat(dalException.show(code.toString(), offset)).isEqualTo(expected.toString());
        }

        @Test
        void show_single_position_in_the_first_char_of_one_line_code() {
            assertShowCode(new DalException("", 0), new StringBuilder()
                            .append("a")
                    , new StringBuilder()
                            .append("a").append("\n")
                            .append("^")
            );
        }

        @Test
        void show_single_position_in_the_one_line_code() {
            assertShowCode(new DalException("", 1), new StringBuilder()
                            .append("abc")
                    , new StringBuilder()
                            .append("abc").append("\n")
                            .append(" ^")
            );
        }

        @Test
        void show_single_position_in_the_first_line_of_double_line_code() {
            assertShowCode(new DalException("", 1), new StringBuilder()
                            .append("abc").append("\n")
                            .append("efg")
                    , new StringBuilder()
                            .append("abc").append("\n")
                            .append(" ^").append("\n")
                            .append("efg")
            );
        }

        @Test
        void show_single_position_in_second_line() {
            assertShowCode(new DalException("", 2), new StringBuilder()
                            .append("a").append("\n")
                            .append("b").append("\n")
                            .append("c")
                    , new StringBuilder()
                            .append("a").append("\n")
                            .append("b").append("\n")
                            .append("^").append("\n")
                            .append("c")
            );
        }

        @Test
        void show_second_char_position_before_line_of_position() {
            assertShowCode(new DalException("", 4).multiPosition(2, CHAR), new StringBuilder()
                            .append("a").append("\n")
                            .append("b").append("\n")
                            .append("c")
                    , new StringBuilder()
                            .append("a").append("\n")
                            .append("b").append("\n")
                            .append("^").append("\n")
                            .append("c").append("\n")
                            .append("^")
            );
        }

        @Test
        void show_second_char_position_after_line_of_position() {
            assertShowCode(new DalException("", 0).multiPosition(4, CHAR), new StringBuilder()
                            .append("a").append("\n")
                            .append("b").append("\n")
                            .append("c")
                    , new StringBuilder()
                            .append("a").append("\n")
                            .append("^").append("\n")
                            .append("b").append("\n")
                            .append("c").append("\n")
                            .append("^")
            );
        }

        @Test
        void support_line_mark() {
            assertShowCode(new DalException("", 0).multiPosition(3, LINE), new StringBuilder()
                            .append("a").append("\n")
                            .append("bcde").append("\n")
                            .append("c")
                    , new StringBuilder()
                            .append("a").append("\n")
                            .append("^").append("\n")
                            .append("bcde").append("\n")
                            .append("^^^^").append("\n")
                            .append("c")
            );
        }

        @Nested
        class OffsetCode {

            @Test
            void show_single_position_in_the_first_char_of_one_line_code() {
                assertShowCode(new DalException("", 3), new StringBuilder()
                                .append("***a")
                        , new StringBuilder()
                                .append("a").append("\n")
                                .append("^")
                        , 3);
            }

            @Test
            void show_single_position_in_the_one_line_code() {
                assertShowCode(new DalException("", 4), new StringBuilder()
                                .append("***abc")
                        , new StringBuilder()
                                .append("abc").append("\n")
                                .append(" ^")
                        , 3);
            }

            @Test
            void show_single_position_in_the_first_line_of_double_line_code() {
                assertShowCode(new DalException("", 4), new StringBuilder()
                                .append("***abc").append("\n")
                                .append("efg")
                        , new StringBuilder()
                                .append("abc").append("\n")
                                .append(" ^").append("\n")
                                .append("efg")
                        , 3);
            }

            @Test
            void show_single_position_in_second_line() {
                assertShowCode(new DalException("", 5), new StringBuilder()
                                .append("***a").append("\n")
                                .append("b").append("\n")
                                .append("c")
                        , new StringBuilder()
                                .append("a").append("\n")
                                .append("b").append("\n")
                                .append("^").append("\n")
                                .append("c")
                        , 3);
            }

            @Test
            void show_second_char_position_before_line_of_position() {
                assertShowCode(new DalException("", 7).multiPosition(5, CHAR), new StringBuilder()
                                .append("***a").append("\n")
                                .append("b").append("\n")
                                .append("c")
                        , new StringBuilder()
                                .append("a").append("\n")
                                .append("b").append("\n")
                                .append("^").append("\n")
                                .append("c").append("\n")
                                .append("^")
                        , 3);
            }

            @Test
            void show_second_char_position_after_line_of_position() {
                assertShowCode(new DalException("", 3).multiPosition(7, CHAR), new StringBuilder()
                                .append("***a").append("\n")
                                .append("b").append("\n")
                                .append("c")
                        , new StringBuilder()
                                .append("a").append("\n")
                                .append("^").append("\n")
                                .append("b").append("\n")
                                .append("c").append("\n")
                                .append("^")
                        , 3);
            }

            @Test
            void support_line_mark() {
                assertShowCode(new DalException("", 3).multiPosition(6, LINE), new StringBuilder()
                                .append("***a").append("\n")
                                .append("bcde").append("\n")
                                .append("c")
                        , new StringBuilder()
                                .append("a").append("\n")
                                .append("^").append("\n")
                                .append("bcde").append("\n")
                                .append("^^^^").append("\n")
                                .append("c")
                        , 3);
            }
        }
    }
}