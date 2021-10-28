package com.github.leeonky.dal.runtime;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DalExceptionTest {

    @Nested
    class Position {

        private void extracted(DalException dalException, StringBuilder code, StringBuilder expected) {
            assertThat(dalException.show(code.toString())).isEqualTo(expected.toString());
        }

        @Test
        void show_single_position_in_the_first_char_of_one_line_code() {
            extracted(new DalException("", 0), new StringBuilder()
                            .append("a")
                    , new StringBuilder()
                            .append("a").append("\n")
                            .append("^")
            );
        }

        @Test
        void show_single_position_in_the_one_line_code() {
            extracted(new DalException("", 1), new StringBuilder()
                            .append("abc")
                    , new StringBuilder()
                            .append("abc").append("\n")
                            .append(" ^")
            );
        }

        @Test
        void show_single_position_in_the_first_line_of_double_line_code() {
            extracted(new DalException("", 1), new StringBuilder()
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
            extracted(new DalException("", 2), new StringBuilder()
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

//        @Test
//        void show_second_char_position_before_line_of_postion() {
//            assertThat(new DalException("", 2).multiPosition(0).show(new StringBuilder()
//                    .append("a").append("\n")
//                    .append("b").append("\n")
//                    .append("c").append("\n")
//                    .toString())).isEqualTo(new StringBuilder()
//                    .append("a").append("\n")
//                    .append("^").append("\n")
//                    .append("b").append("\n")
//                    .append("^").append("\n")
//                    .append("c").append("\n")
//                    .toString().trim());
//        }
    }
}