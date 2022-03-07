package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.function.Predicate;

import static com.github.leeonky.interpreter.FunctionUtil.oneOf;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.internal.bytebuddy.matcher.ElementMatchers.any;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

class FunctionUtilTest {

    @Nested
    class Not {

        @Test
        void not() {
            Predicate<String> predicate = str -> {
                assertThat(str).isEqualTo("given");
                return true;
            };

            assertThat(FunctionUtil.not(predicate).test("given")).isFalse();
        }
    }

    @Nested
    class NotAllowParallelReduce {

        @Test
        void should_raise_exception() {
            assertThrows(IllegalStateException.class, () -> FunctionUtil.notAllowParallelReduce().apply(any(), any()));
        }
    }

    @Nested
    class OneOf {

        @Test
        void return_empty_when_all_supplier_empty() {
            assertThat(oneOf(Optional::empty)).isEmpty();
            assertThat(oneOf(Optional::empty, Optional::empty)).isEmpty();
        }

        @Test
        void return_option_value_when_present() {
            assertThat(FunctionUtil.oneOf(Optional::empty, () -> Optional.of("hello"))).hasValue("hello");
        }

        @Test
        void return_first_option_value_and_ignore_others() {
            assertThat(FunctionUtil.oneOf(Optional::empty, () -> Optional.of("hello"), () -> {
                fail();
                return Optional.of("any str");
            })).hasValue("hello");
        }
    }
}