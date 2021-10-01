package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.DalException;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.compiler.NodeParser;
import com.github.leeonky.dal.compiler.SourceCode;
import lombok.SneakyThrows;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class TestContext {
    public static TestContext INSTANCE = new TestContext();
    DAL dal = new DAL();

    Object inputObject = null;
    SourceCode sourceCode = null;
    String sourceCodeString = null;
    DalException dalException;
    Node node = null;
    private List<String> schemas = new ArrayList<>();

    public static void reset() {
        INSTANCE = new TestContext();
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
        dal.assertData(node.evaluate(dal.getRuntimeContextBuilder().build(inputObject)), assertion);
    }

    public void assertNodeValue(String assertion, NodeParser nodeParser) {
        dal.assertData(nodeParser.fetch(new SourceCode(sourceCodeString)).orElse(null)
                .evaluate(dal.getRuntimeContextBuilder().build(null)), assertion);
    }

    public void shouldShowSourceCodePosition(String sourceCodePosition) {
        assertThat("\n" + dalException.show(sourceCodeString)).isEqualTo("\n" + sourceCodePosition);
    }

    public void failedToGetNodeWithMessage(NodeParser nodeParser, String message) {
        dalException = assertThrows(DalException.class, () -> nodeParser.fetch(sourceCode));
        shouldHasDalMessage(message);
    }

    public void shouldHasDalMessage(String message) {
        assertThat(dalException).hasMessage(message);
    }

    public void compileAndAssertNode(NodeParser nodeParser, String assertion) {
        try {
            node = nodeParser.fetch(sourceCode).orElse(null);
        } catch (DalException e) {
            System.err.println(e.show(sourceCodeString));
            throw e;
        }
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
            new Compiler().compileToClasses(schemas.stream().map(s ->
                    "import com.github.leeonky.dal.type.*;\n" +
                            "import java.util.*;\n" + s)
                    .collect(Collectors.toList()))
                    .forEach(dal.getRuntimeContextBuilder()::registerSchema);
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

    public void ignoreNodeBy(NodeParser nodeParser) {
        nodeParser.fetch(sourceCode);
    }

    public void registerSchema(String schemaCode) {
        schemas.add(schemaCode);
    }
}
