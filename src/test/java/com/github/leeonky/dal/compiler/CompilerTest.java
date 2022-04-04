package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;

class CompilerTest {

    @Nested
    class RelaxString {
        private Compiler compiler = new Compiler();
        private RuntimeContextBuilder.DALRuntimeContext runtimeContext = new RuntimeContextBuilder().build(null);

        @Test
        void relax_string_end_with_chars() {
            Set<Character> DELIMITER = new HashSet<>(asList('\r'));

            DELIMITER.forEach(c -> relaxStringShouldBe(String.format("hello%cworld", c), "hello"));

            relaxStringShouldBe("hello", "hello");
        }

        @Nested
        class Invalid {

            @Test
            void should_not_starts_with() {
                Set<Character> DELIMITER = new HashSet<>(asList('=', '>', '<', '+', '-', '*', '/', ':', '&', '|', '!',
                        ',', '(', ')', '[', ']', '{', '}', ' ', '\t', '\n', '\r', '#', '\'', '"', '.'));

                DELIMITER.forEach(c -> invalidRelaxString(c + "any"));
            }
        }

        private void relaxStringShouldBe(String code, String expected) {
            assertThat(compiler.EXPRESSION_RELAX_STRING.parse(new DALProcedure(SourceCode.createSourceCode(code, emptyList()),
                    runtimeContext, DALExpression::new)).get().evaluate(runtimeContext)).isEqualTo(expected);
        }

        private void invalidRelaxString(String code) {
            assertThat(compiler.EXPRESSION_RELAX_STRING.parse(new DALProcedure(SourceCode.createSourceCode(code, emptyList()),
                    runtimeContext, DALExpression::new))).as("invalid relax string: " + code).isEmpty();
        }
    }
}