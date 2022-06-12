package com.github.leeonky.interpreter;

import com.github.leeonky.dal.runtime.DalException;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.interpreter.InterpreterException.Position.Type.CHAR;
import static com.github.leeonky.interpreter.InterpreterException.Position.Type.LINE;
import static org.assertj.core.api.Assertions.assertThat;

class InterpreterExceptionTest {

    @Nested
    class Position {

        private void assertShowCode(InterpreterException interpreterException, StringBuilder code, StringBuilder expected) {
            assertShowCode(interpreterException, code, expected, 0);
        }

        private void assertShowCode(InterpreterException interpreterException, StringBuilder code, StringBuilder expected, int offset) {
            assertThat(interpreterException.show(code.toString(), offset)).isEqualTo(expected.toString());
        }

        @Test
        void position_for_chinese_chars() {
            assertShowCode(new InterpreterException("", 7), new StringBuilder()
                            .append("a你c").append("\n")
                            .append("d你你好")
                    , new StringBuilder()
                            .append("a你c").append("\n")
                            .append("d你你好").append("\n")
                            .append("     ^")
            );
        }

        @Test
        void position_for_chinese_chars_in_line_mode() {
            assertShowCode(new InterpreterException("", 3, LINE), new StringBuilder()
                            .append("a你").append("\n")
                            .append("d你")
                    , new StringBuilder()
                            .append("a你").append("\n")
                            .append("d你").append("\n")
                            .append("^^^")
            );
        }

        @Test
        void show_single_position_in_the_first_char_of_one_line_code() {
            assertShowCode(new InterpreterException("", 0), new StringBuilder()
                            .append("a")
                    , new StringBuilder()
                            .append("a").append("\n")
                            .append("^")
            );
        }

        @Test
        void show_single_position_in_the_one_line_code() {
            assertShowCode(new InterpreterException("", 1), new StringBuilder()
                            .append("abc")
                    , new StringBuilder()
                            .append("abc").append("\n")
                            .append(" ^")
            );
        }

        @Test
        void show_single_position_in_the_first_line_of_double_line_code() {
            assertShowCode(new InterpreterException("", 1), new StringBuilder()
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
            assertShowCode(new InterpreterException("", 2), new StringBuilder()
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
            assertShowCode(new InterpreterException("", 4).multiPosition(2, CHAR), new StringBuilder()
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
            assertShowCode(new InterpreterException("", 0).multiPosition(4, CHAR), new StringBuilder()
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
        void support_line_mark_for_one_line() {
            assertShowCode(new InterpreterException("", 0, LINE), new StringBuilder()
                            .append("a")
                    , new StringBuilder()
                            .append("a").append("\n")
                            .append("^")
            );
        }

        @Test
        void support_line_mark() {
            assertShowCode(new InterpreterException("", 0).multiPosition(3, LINE), new StringBuilder()
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
                assertShowCode(new InterpreterException("", 3), new StringBuilder()
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
                assertShowCode(new InterpreterException("", 4), new StringBuilder()
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
                assertShowCode(new InterpreterException("", 5), new StringBuilder()
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
                assertShowCode(new InterpreterException("", 7).multiPosition(5, CHAR), new StringBuilder()
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
                assertShowCode(new InterpreterException("", 3).multiPosition(7, CHAR), new StringBuilder()
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
                assertShowCode(new InterpreterException("", 3).multiPosition(6, LINE), new StringBuilder()
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

        @Nested
        class ClearPosition {

            @Test
            void clear_position_data_of_exception() {
                InterpreterException interpreterException = new InterpreterException("", 7);
                interpreterException.clearPosition();
                assertShowCode(interpreterException, new StringBuilder()
                                .append("a你c").append("\n")
                                .append("d你你好")
                        , new StringBuilder()
                                .append("a你c").append("\n")
                                .append("d你你好")
                );
            }
        }

        @Nested
        class ChangePositionType {

            @Test
            void change_position_when_no_type_should_not_raise_any_error() {
                InterpreterException interpreterException = new InterpreterException("", 7, CHAR);
                interpreterException.clearPosition();
                interpreterException.setType(LINE);
                assertShowCode(interpreterException, new StringBuilder()
                                .append("a你c").append("\n")
                                .append("d你你好")
                        , new StringBuilder()
                                .append("a你c").append("\n")
                                .append("d你你好")
                );
            }

            @Test
            void should_change_first_postion_line() {
                InterpreterException interpreterException = new InterpreterException("", 1, CHAR);
                interpreterException.multiPosition(4, CHAR);
                interpreterException.setType(LINE);
                assertShowCode(interpreterException, new StringBuilder()
                                .append("a你c").append("\n")
                                .append("d你你好")
                        , new StringBuilder()
                                .append("a你c").append("\n")
                                .append("^^^^").append("\n")
                                .append("d你你好").append("\n")
                                .append("^")
                );
            }
        }
    }
}