package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.DalException;
import com.github.leeonky.dal.DataAssert;
import lombok.SneakyThrows;
import org.json.JSONArray;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SpecContext {
    private Object inputObject;
    private String dalSourceCode = "";
    private AssertResult assertResult;
    private DalException dalException;

    @SneakyThrows
    public void givenInputByJson(String json) {
        inputObject = new JSONArray(String.format("[%s]", json)).get(0);
    }

    public void shouldPass() {
        assertTrue(assertResult.isPassed());
    }

    public void executeDal(String dalSourceCode, DataAssert dataAssert) {
        try {
            assertResult = dataAssert.assertData(inputObject, this.dalSourceCode = dalSourceCode);
        } catch (DalException dalException) {
            this.dalException = dalException;
        }
    }

    public void exceptionWithMessage(String message) {
        assertThat(dalException).as("expected failed or exception but not").isNotNull();
        assertThat(dalException).hasMessage(message);
    }

    public void sourceCodePositionMessage(String sourceCodePosition) {
        assertThat(dalException.show(dalSourceCode)).isEqualTo(sourceCodePosition);
    }
}
