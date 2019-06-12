package com.github.leeonky.dal.util;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class StringUtilTest {

    @Nested
    class Capitalize {

        @Test
        void should_support_empty_string() {
            assertThat(StringUtil.capitalize("")).isEqualTo("");
        }

        @Test
        void should_capitalize_string() {
            assertThat(StringUtil.capitalize("a")).isEqualTo("A");
            assertThat(StringUtil.capitalize("test")).isEqualTo("Test");
        }

        @Test
        void should_keep_original_for_capitalized_string() {
            assertThat(StringUtil.capitalize("Test")).isEqualTo("Test");
        }
    }

    @Nested
    class UnCapitalize {

        @Test
        void should_support_empty_string() {
            assertThat(StringUtil.unCapitalize("")).isEqualTo("");
        }

        @Test
        void should_capitalize_string() {
            assertThat(StringUtil.unCapitalize("A")).isEqualTo("a");
            assertThat(StringUtil.unCapitalize("Test")).isEqualTo("test");
        }

        @Test
        void should_keep_original_for_uncapitalized_string() {
            assertThat(StringUtil.unCapitalize("test")).isEqualTo("test");
        }
    }
}