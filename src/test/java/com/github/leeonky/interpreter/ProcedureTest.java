package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Optional;

import static com.github.leeonky.interpreter.SourceCodeTest.NO_ESCAPE;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

class ProcedureTest {

    @Nested
    class FetchString {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestProcedure testProcedure = givenProcedureWithCode("'a'");

            assertThat(testProcedure.fetchString('"', '"', s -> fail(), new HashMap<>())).isEmpty();
            assertThat(testProcedure.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_empty_string() {
            TestProcedure testProcedure = givenProcedureWithCode(" '\"");
            TestNode actual = testProcedure.fetchString('\'', '"', TestNode::new, new HashMap<>()).get();
            assertThat(actual.getContent()).isEqualTo("");
            assertThat(actual.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void escape_char() {
            TestProcedure testProcedure = givenProcedureWithCode(" 'a\\x'");
            TestNode actual = testProcedure.fetchString('\'', '\'', TestNode::new, new HashMap<String, Character>() {{
                put("\\x", 'y');
            }}).get();
            assertThat(actual.getContent()).isEqualTo("ay");
            assertThat(actual.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void raise_error_when_not_finish() {
            TestProcedure testProcedure = givenProcedureWithCode(" 'a");
            SyntaxException syntaxException = assertThrows(SyntaxException.class, () ->
                    testProcedure.fetchString('\'', '\'', TestNode::new, new HashMap<>()));
            assertThat(syntaxException).hasMessageContaining("should end with `'`");

            assertThat(syntaxException.show(" 'a")).isEqualTo(" 'a\n   ^");
        }
    }

    @Nested
    class FetchNodeBetween {

        public Optional<TestNode> oneCharNode(TestProcedure testProcedure) {
            return Optional.of(new TestNode(testProcedure.getSourceCode().popChar(NO_ESCAPE)));
        }

        @Test
        void return_empty_when_not_match_opening_char() {
            TestProcedure testProcedure = givenProcedureWithCode("'a'");

            assertThat(testProcedure.fetchNodeBetween("(", ")", this::oneCharNode)).isEmpty();
            assertThat(testProcedure.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
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
            assertThat(procedure.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }
    }

    private TestProcedure givenProcedureWithCode(String s) {
        return new TestProcedure(new SourceCode(s));
    }
}