package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.DalException;
import com.github.leeonky.dal.DataAssert;
import com.github.leeonky.dal.RuntimeContextBuilder;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.compiler.NodeFactory;
import com.github.leeonky.dal.compiler.NodeParser;
import com.github.leeonky.dal.parser.TokenParser;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;
import com.github.leeonky.dal.token.TokenStream;
import lombok.SneakyThrows;
import org.json.JSONArray;
import org.json.JSONObject;

import static com.github.leeonky.dal.token.TokenFactory.createDALTokenFactory;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TestContext {
    public static TestContext INSTANCE = new TestContext();
    private static DataAssert dataAssert = new DataAssert();
    private static RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

    static {
        dataAssert.getRuntimeContextBuilder()
                .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                .registerListAccessor(JSONArray.class, new JSONArrayListAccessor());
        runtimeContextBuilder
                .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                .registerListAccessor(JSONArray.class, new JSONArrayListAccessor());
    }

    private Object inputObject = null;
    private String dalSourceCode = "";
    private AssertResult assertResult;
    private DalException dalException;
    private Token token = null;
    private Node node = null;
    private TokenParser tokenParser = null;
    private SourceCode sourceCode;

    public static void reset() {
        INSTANCE = new TestContext();
    }

    @SneakyThrows
    public void givenInputByJson(String json) {
        inputObject = new JSONArray(String.format("[%s]", json)).get(0);
    }

    public void shouldPass() {
        assertThat(dalException).isNull();
        assertTrue(assertResult.isPassed());
    }

    public void executeDal(String dalSourceCode) {
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
            assertThat(dataAssert.assertData(token, assertion).isPassed()).isTrue();
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

    public void compileNode(NodeFactory nodeFactory) {
        TokenStream tokenStream = createDALTokenFactory().fetchToken(tokenParser).getTokenStream();
        node = nodeFactory.fetchNode(new NodeParser(tokenStream));
    }

    public void assertNode(String assertion) {
        try {
            assertThat(dataAssert.assertData(node, assertion).isPassed()).isTrue();
        } catch (DalException e) {
            System.err.println(e.getMessage());
            System.err.println(e.show(assertion));
            throw e;
        }
    }

    public void assertEvaluateNode(String assertion) {
        try {
            assertThat(dataAssert.assertData(node.evaluate(runtimeContextBuilder.build(inputObject)), assertion)
                    .isPassed()).isTrue();
        } catch (DalException e) {
            System.err.println(e.getMessage());
            System.err.println(e.show(assertion));
            throw e;
        }
    }
}
