package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.DalException;
import com.github.leeonky.dal.RuntimeContextBuilder;
import com.github.leeonky.dal.ast.MandatoryNodeFactory;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.NodeParser;
import com.github.leeonky.dal.parser.TokenParser;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;
import com.github.leeonky.dal.token.TokenStream;
import lombok.SneakyThrows;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.token.TokenFactory.createDALTokenFactory;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TestContext {
    public static TestContext INSTANCE = new TestContext();
    private static DAL dal = new DAL();
    private static RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

    private Object inputObject = null;
    private String dalSourceCode = "";
    private AssertResult assertResult;
    private DalException dalException;
    private Token token = null;
    private Node node = null;
    private TokenParser tokenParser = null;
    private SourceCode sourceCode;
    private NodeParser nodeParser;
    private TokenStream tokenStream;
    private List<String> schemas = new ArrayList<>();

    public static void reset() {
        INSTANCE = new TestContext();
    }

    public void initDataAssert() {
        dal.getRuntimeContextBuilder()
                .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                .registerListAccessor(JSONArray.class, new JSONArrayAccessor());
        runtimeContextBuilder
                .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                .registerListAccessor(JSONArray.class, new JSONArrayAccessor());
    }

    @SneakyThrows
    public void givenInputByJson(String json) {
        inputObject = new JSONArray(String.format("[%s]", json)).get(0);
    }

    public void shouldPass(String dalCode) {
        if (dalException != null)
            System.err.println(dalException.show(dalCode));
        assertThat(dalException).isNull();
        assertTrue(assertResult.isPassed());
    }

    public void executeDal(String dalSourceCode) {
        try {
            new Compiler().compileToClasses(schemas.stream().map(s ->
                    "import com.github.leeonky.dal.type.*;\n" +
                            "import java.util.*;\n" + s)
                    .collect(Collectors.toList()))
                    .forEach(dal.getRuntimeContextBuilder()::registerSchema);
            assertResult = dal.assertData(inputObject, this.dalSourceCode = dalSourceCode);
        } catch (DalException dalException) {
            this.dalException = dalException;
        }
    }

    public void exceptionWithMessage(String message) {
        assertThat(dalException).as("expected failed or exception but not").isNotNull();
        assertThat(dalException).hasMessage(message);
    }

    public void sourceCodePositionMessage(String sourceCodePosition) {
        assertThat("\n" + dalException.show(dalSourceCode)).isEqualTo("\n" + sourceCodePosition);
    }

    public void givenDalSourceCode(String dalSourceCode) {
        this.dalSourceCode = dalSourceCode;
        sourceCode = new SourceCode(this.dalSourceCode);
        tokenParser = new TokenParser(sourceCode);
    }

    public void parseToken(TokenFactory tokenFactory) {
        try {
            token = tokenFactory.fetchToken(tokenParser);
        } catch (DalException e) {
            System.err.println(e.getMessage());
            System.err.println(e.show(dalSourceCode));
            throw e;
        }
    }

    public void assertToken(String assertion) {
        try {
            assertThat(dal.assertData(token, assertion).isPassed()).isTrue();
        } catch (DalException e) {
            System.err.println(e.getMessage());
            System.err.println(e.show(assertion));
            throw e;
        }
    }

    public SourceCode getSourceCode() {
        return sourceCode;
    }

    public void failedParseToken(TokenFactory tokenFactory, String message) {
        dalException = assertThrows(DalException.class, () -> tokenFactory.fetchToken(tokenParser));
        assertThat(dalException).hasMessage(message);
    }

    public void failedCompileNode(MandatoryNodeFactory factory, String message) {
        dalException = assertThrows(DalException.class, () -> compileNode(factory));
        assertThat(dalException).hasMessage(message);
    }

    public void compileNode(MandatoryNodeFactory mandatoryNodeFactory) {
        node = mandatoryNodeFactory.fetch(getNodeParser());
    }

    private NodeParser getNodeParser() {
        if (nodeParser == null) {
            tokenStream = createDALTokenFactory().fetchToken(tokenParser).getTokenStream();
            nodeParser = new NodeParser(tokenStream);
        }
        return nodeParser;
    }

    public void assertNode(String assertion) {
        try {
            assertThat(dal.assertData(node, assertion).isPassed()).isTrue();
        } catch (DalException e) {
            System.err.println(e.getMessage());
            System.err.println(e.show(assertion));
            throw e;
        }
    }

    public void assertEvaluateNode(String assertion) {
        Object evaluate = node.evaluate(runtimeContextBuilder.build(inputObject));
        try {
            assertThat(dal.assertData(evaluate, assertion).isPassed()).isTrue();
        } catch (DalException e) {
            System.err.println(e.getMessage());
            System.err.println(e.show(assertion));
            throw e;
        }
    }

    public void registerSchema(String schemaCode) {
        schemas.add(schemaCode);
    }
}
