package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.DalException;
import com.github.leeonky.dal.ast.Node;
import lombok.SneakyThrows;
import org.json.JSONArray;
import org.json.JSONObject;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class TestContext2 {
    public static TestContext2 INSTANCE = new TestContext2();
    DAL dal = new DAL();

    Object inputObject = null;
    SourceCode sourceCode = null;
    String sourceCodeString = null;
    AssertResult assertResult;
    DalException dalException;
    Node node = null;

    public static void reset() {
        INSTANCE = new TestContext2();
        INSTANCE.initDataAssert();
    }

    private void initDataAssert() {
        dal.getRuntimeContextBuilder()
                .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                .registerListAccessor(JSONArray.class, new JSONArrayAccessor());
    }

    public void giveDalSourceCode(String code) {
        sourceCode = new SourceCode(sourceCodeString = parseTabAndSpace(code));
    }

    private String parseTabAndSpace(String code) {
        return code.replace("`TAB", "\t").replace("`SPACE", " ");
    }

    public void assertLastNodeValue(String assertion) {
        dal.assertData(node.evaluate(dal.getRuntimeContextBuilder().build(null)), assertion);
    }

    public void assertNodeValue(String assertion, NodeParser nodeParser) {
        dal.assertData(nodeParser.fetch(new SourceCode(sourceCodeString)).orElse(null)
                .evaluate(dal.getRuntimeContextBuilder().build(null)), assertion);
    }

    public void shouldShowSourceCodePosition(String sourceCodePosition) {
        assertThat(dalException.show(sourceCodeString)).isEqualTo(sourceCodePosition);
    }

    public void failedToGetNodeWithMessage(NodeParser nodeParser, String message) {
        dalException = assertThrows(DalException.class, () ->
                nodeParser.fetch(new SourceCode(sourceCodeString)));
        shouldHasDalMessage(message);
    }

    public void shouldHasDalMessage(String message) {
        assertThat(dalException).hasMessage(message);
    }

    public void compileAndAssertNode(NodeParser nodeParser, String assertion) {
        node = nodeParser.fetch(sourceCode).orElse(null);
        try {
            dal.assertData(INSTANCE.node, assertion);
        } catch (DalException e) {
            System.err.println(e.show(assertion));
            throw e;
        }
    }

    public void assertInputData(String assertion) {
        giveDalSourceCode(assertion);
        try {
            dal.assertData(inputObject, assertion);
        } catch (DalException e) {
            dalException = e;
        }
    }

    public void shouldNoException() {
        assertThat(dalException).isNull();
    }

    @SneakyThrows
    public void givenInputByJson(String json) {
        inputObject = new JSONArray(String.format("[%s]", json)).get(0);
    }
}
