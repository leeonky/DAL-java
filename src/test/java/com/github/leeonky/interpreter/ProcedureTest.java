package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static com.github.leeonky.interpreter.SourceCodeTest.NO_ESCAPE;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ProcedureTest {

    @Nested
    class FetchNodeBetween {

        public Optional<TestNode> oneCharNode(TestProcedure testProcedure) {
            return Optional.of(new TestNode(testProcedure.getSourceCode().popCharBk(NO_ESCAPE)));
        }

        @Test
        void return_empty_when_not_match_opening_char() {
            TestProcedure testProcedure = givenProcedureWithCode("'a'");

            assertThat(testProcedure.fetchNodeBetween("(", ")", this::oneCharNode)).isEmpty();
            assertThat(testProcedure.getSourceCode().popCharBk(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_object_between_opening_and_closing() {
            TestProcedure testProcedure = givenProcedureWithCode("(a)");

            TestNode testNode = testProcedure.fetchNodeBetween("(", ")", this::oneCharNode).get();
            assertThat(testNode.getContent()).isEqualTo('a');
            assertThat(testProcedure.getSourceCode().isEndOfLine()).isTrue();
        }

        @Test
        void raise_error_when_no_closing() {
            TestProcedure testProcedure = givenProcedureWithCode("(a");

            assertThat(assertThrows(SyntaxException.class, () -> testProcedure.fetchNodeBetween("(", ")", this::oneCharNode)))
                    .hasMessageContaining("should end with `)`");
        }

        @Test
        void return_empty_when_fetch_empty_between_opening_and_closing() {
            TestProcedure procedure = givenProcedureWithCode("'a'");

            assertThat(procedure.fetchNodeBetween("'", "'", testProcedure -> Optional.empty())).isEmpty();
            assertThat(procedure.getSourceCode().popCharBk(NO_ESCAPE)).isEqualTo('\'');
        }
    }

    private TestProcedure givenProcedureWithCode(String s) {
        return new TestProcedure(new SourceCode(s));
    }
}