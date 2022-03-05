package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ProcedureTest {
    private TestProcedure givenProcedureWithCode(String s) {
        return new TestProcedure(new SourceCode(s));
    }

    @Nested
    class UnderOperator {

        @Test
        void under_operator() {
            TestProcedure procedure = givenProcedureWithCode("");
            TestOperator operator = new TestOperator();
            TestOperator operator2 = new TestOperator();
            TestNode testNode = new TestNode();
            TestNode testNode2 = new TestNode();
            assertThat(procedure.currentOperator()).isEmpty();

            assertThat(procedure.underOperator(operator, () -> {
                assertThat(procedure.currentOperator().get()).isSameAs(operator);

                assertThat(procedure.underOperator(operator2, () -> {
                    assertThat(procedure.currentOperator().get()).isSameAs(operator2);

                    return testNode2;
                })).isEqualTo(testNode2);

                assertThat(procedure.currentOperator().get()).isSameAs(operator);
                return testNode;
            })).isEqualTo(testNode);

            assertThat(procedure.currentOperator()).isEmpty();
        }

        @Test
        void should_pop_operator_when_got_exception() {
            TestProcedure procedure = givenProcedureWithCode("");
            TestOperator operator = new TestOperator();

            assertThrows(RuntimeException.class, () -> procedure.underOperator(operator, () -> {
                throw new RuntimeException();
            }));

            assertThat(procedure.currentOperator()).isEmpty();
        }
    }

    @Nested
    class PositionOf {

        @Test
        void position_of_source_code() {
            TestProcedure procedure = givenProcedureWithCode("a");

            assertThat((int) procedure.positionOf(i -> i)).isEqualTo(0);
        }

        @Test
        void left_trim_when_get_position_of_source_code() {
            TestProcedure procedure = givenProcedureWithCode(" a");

            assertThat((int) procedure.positionOf(i -> i)).isEqualTo(1);
        }
    }

    @Nested
    class Index {

        @Test
        void get_and_move_index() {
            TestProcedure procedure = givenProcedureWithCode("");
            TestNode testNode = new TestNode();
            TestNode testNode2 = new TestNode();

            assertThat(procedure.actionUnderIndex(() -> {
                assertThat(procedure.getIndex()).isEqualTo(0);
                procedure.incrementIndex();
                assertThat(procedure.getIndex()).isEqualTo(1);

                assertThat(procedure.actionUnderIndex(() -> {
                    assertThat(procedure.getIndex()).isEqualTo(0);
                    procedure.incrementIndex();
                    assertThat(procedure.getIndex()).isEqualTo(1);
                    procedure.incrementIndex();
                    assertThat(procedure.getIndex()).isEqualTo(2);
                    return testNode2;
                })).isEqualTo(testNode2);

                assertThat(procedure.getIndex()).isEqualTo(1);
                return testNode;
            })).isEqualTo(testNode);
        }

        @Test
        void pop_index_when_got_exception() {
            TestProcedure procedure = givenProcedureWithCode("");
            TestNode testNode = new TestNode();

            assertThat(procedure.actionUnderIndex(() -> {
                assertThat(procedure.getIndex()).isEqualTo(0);
                procedure.incrementIndex();
                assertThat(procedure.getIndex()).isEqualTo(1);

                assertThrows(RuntimeException.class, () -> procedure.actionUnderIndex(() -> {
                    throw new RuntimeException();
                }));

                assertThat(procedure.getIndex()).isEqualTo(1);
                return testNode;
            })).isEqualTo(testNode);
        }
    }
}