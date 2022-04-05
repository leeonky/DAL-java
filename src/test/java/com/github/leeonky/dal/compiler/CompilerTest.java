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
    class ExpressionRelaxString {
        private Compiler compiler = new Compiler();
        private RuntimeContextBuilder.DALRuntimeContext runtimeContext = new RuntimeContextBuilder().build(null);

        @Test
        void relax_string_end_with_chars() {
            Set<Character> DELIMITER = new HashSet<>(asList('\r'));

            DELIMITER.forEach(c -> relaxStringShouldBe(String.format("hello%cworld", c), "hello"));

            relaxStringShouldBe("hello", "hello");
        }

        private void relaxStringShouldBe(String code, String expected) {
            assertThat(compiler.EXPRESSION_RELAX_STRING.parse(new DALProcedure(SourceCode.createSourceCode(code, emptyList()),
                    runtimeContext, DALExpression::new)).evaluate(runtimeContext)).isEqualTo(expected);
        }
    }

    @Nested
    class ObjectScopeRelaxString {
        private Compiler compiler = new Compiler();
        private RuntimeContextBuilder.DALRuntimeContext runtimeContext = new RuntimeContextBuilder().build(null);

        @Test
        void relax_string_end_with_chars() {
            Set<Character> DELIMITER = new HashSet<>(asList('\r'));

            DELIMITER.forEach(c -> relaxStringShouldBe(String.format("hello%cworld", c), "hello"));

            relaxStringShouldBe("hello", "hello");
        }

        private void relaxStringShouldBe(String code, String expected) {
            assertThat(compiler.OBJECT_SCOPE_RELAX_STRING.parse(new DALProcedure(SourceCode.createSourceCode(code, emptyList()),
                    runtimeContext, DALExpression::new)).evaluate(runtimeContext)).isEqualTo(expected);
        }
    }

    @Nested
    class ListScopeRelaxString {
        private Compiler compiler = new Compiler();
        private RuntimeContextBuilder.DALRuntimeContext runtimeContext = new RuntimeContextBuilder().build(null);

        @Test
        void relax_string_end_with_chars() {
            Set<Character> DELIMITER = new HashSet<>(asList('\r'));

            DELIMITER.forEach(c -> relaxStringShouldBe(String.format("hello%cworld", c), "hello"));

            relaxStringShouldBe("hello", "hello");
        }

        private void relaxStringShouldBe(String code, String expected) {
            assertThat(compiler.LIST_SCOPE_RELAX_STRING.parse(new DALProcedure(SourceCode.createSourceCode(code, emptyList()),
                    runtimeContext, DALExpression::new)).evaluate(runtimeContext)).isEqualTo(expected);
        }
    }
}