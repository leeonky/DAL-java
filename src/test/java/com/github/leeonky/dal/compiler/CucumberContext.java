package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.cucumber.JSONArrayAccessor;
import com.github.leeonky.dal.cucumber.JSONObjectAccessor;
import com.github.leeonky.dal.runtime.DalException;
import lombok.SneakyThrows;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.*;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class CucumberContext {
    private static final Compiler compiler = new Compiler();
    private static final Map<String, NodeParser> parserMap = new HashMap<String, NodeParser>() {{
        put("number", compiler.NUMBER);
        put("integer", compiler.INTEGER);
        put("single-quoted-string", compiler.SINGLE_QUOTED_STRING);
        put("double-quoted-string", compiler.DOUBLE_QUOTED_STRING);
        put("const-true", compiler.CONST_TRUE);
        put("const-false", compiler.CONST_FALSE);
        put("const-null", compiler.CONST_NULL);
        put("const", compiler.CONST);
        put("regex", compiler.REGEX);
        put("dot-property", compiler.DOT_PROPERTY.defaultInputNode());
        put("identity-property", compiler.IDENTITY_PROPERTY);
        put("bracket-property", compiler.BRACKET_PROPERTY.defaultInputNode());
        put("explicit-property", compiler.EXPLICIT_PROPERTY.defaultInputNode());
        put("property", compiler.PROPERTY);
        put("operand", optional(compiler.OPERAND));
        put("binary-operator-expression", compiler.BINARY_OPERATOR_EXPRESSION.defaultInputNode());
        put("schema-expression", compiler.SCHEMA_EXPRESSION.defaultInputNode());
        put("expression", optional(compiler.EXPRESSION));
        put("parentheses", compiler.PARENTHESES);
        put("object", compiler.OBJECT);
        put("list", compiler.LIST);
        put("judgement-expression-operand", optional(compiler.JUDGEMENT_EXPRESSION_OPERAND));
        put("schema", optional(compiler.SCHEMA));
    }};

    private static NodeParser optional(NodeCompiler nodeCompiler) {
        return sourceCode -> Optional.ofNullable(nodeCompiler.fetch(sourceCode));
    }

    public static CucumberContext INSTANCE = new CucumberContext();
    DAL dal = new DAL();

    Object inputObject = null;
    SourceCode sourceCode = null;
    String sourceCodeString = null;
    DalException dalException;
    Node node = null;
    private List<String> schemas = new ArrayList<>();

    public static void reset() {
        INSTANCE = new CucumberContext();
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

    public void assertNodeValue(String assertion, String factory) {
        dal.assertData(parserMap.get(factory).fetch(new SourceCode(sourceCodeString)).orElse(null)
                .evaluate(dal.getRuntimeContextBuilder().build(null)), assertion);
    }

    public void shouldShowSourceCodePosition(String sourceCodePosition) {
        assertThat("\n" + dalException.show(sourceCodeString)).isEqualTo("\n" + sourceCodePosition);
    }

    public void failedToGetNodeWithMessage(String factory, String message) {
        dalException = assertThrows(DalException.class, () -> parserMap.get(factory).fetch(sourceCode));
        shouldHasDalMessage(message);
    }

    public void shouldHasDalMessage(String message) {
        assertThat(dalException).hasMessage(message);
    }

    public void compileAndAssertNode(String factory, String assertion) {
        try {
            node = parserMap.get(factory).fetch(sourceCode).orElse(null);
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
            new com.github.leeonky.dal.cucumber.Compiler().compileToClasses(schemas.stream().map(s ->
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

    public void ignoreNodeBy(String factory) {
        parserMap.get(factory).fetch(sourceCode);
    }

    public void registerSchema(String schemaCode) {
        schemas.add(schemaCode);
    }
}
