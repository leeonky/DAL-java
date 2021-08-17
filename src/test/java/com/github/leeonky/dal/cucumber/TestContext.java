package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.DalException;
import com.github.leeonky.dal.DataAssert;
import lombok.SneakyThrows;
import org.json.JSONArray;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TestContext {
    Object inputObject;
    String dalSourceCode = "";
    AssertResult assertResult;
    DalException dalException;

    @SneakyThrows
    public void givenInputByJson(String json) {
        inputObject = new JSONArray(String.format("[%s]", json)).get(0);
    }

    public void shouldPass() {
        assertTrue(assertResult.isPassed());
    }

    public void executeDal(String docString, DataAssert dataAssert) {
        try {
            assertResult = dataAssert.assertData(inputObject, dalSourceCode = docString);
        } catch (DalException dalException) {
            this.dalException = dalException;
        }
    }

    public void exceptionWithMessage(String message) {
        assertThat(dalException).as("expected failed or exception but not").isNotNull();
        assertThat(dalException).hasMessage(message);
    }

    public void sourceCodePositionMessage(String docString) {
        assertThat(dalException.show(dalSourceCode)).isEqualTo(docString);
    }
}
