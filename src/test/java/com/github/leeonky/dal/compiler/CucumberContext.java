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
    private static final Map<String, NodeMatcher> matcherMap = new HashMap<String, NodeMatcher>() {{
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
        put("table", compiler.TABLE);
        put("schema", optional(SchemaExpressionClauseFactory.SCHEMA));
    }};

    private static NodeMatcher optional(NodeFactory nodeFactory) {
        return parser -> Optional.ofNullable(nodeFactory.fetch(parser));
    }

    public static CucumberContext INSTANCE = new CucumberContext();
    DAL dal = new DAL();

    Object inputObject = null;
    TokenParser tokenParser = null;
    String sourceCodeString = null;
    DalException dalException;
    Node node = null;
    private List<String> schemas = new ArrayList<>();
    private List<String> javaClasses = new ArrayList<>();

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
        tokenParser = new TokenParser(new SourceCode(sourceCodeString = parseTabAndSpace(code)));
    }

    private String parseTabAndSpace(String code) {
        return code.replace("`TAB", "\t").replace("`SPACE", " ");
    }

    public void assertLastNodeValue(String assertion) {
        dal.evaluate(node.evaluate(dal.getRuntimeContextBuilder().build(inputObject)), assertion);
    }

    public void assertNodeValue(String assertion, String factory) {
        dal.evaluate(matcherMap.get(factory).fetch(new TokenParser(new SourceCode(sourceCodeString))).orElse(null)
                .evaluate(dal.getRuntimeContextBuilder().build(null)), assertion);
    }

    public void shouldShowSourceCodePosition(String sourceCodePosition) {
        assertThat("\n" + dalException.show(sourceCodeString)).isEqualTo("\n" + sourceCodePosition);
    }

    public void failedToGetNodeWithMessage(String factory, String message) {
        dalException = assertThrows(DalException.class, () -> matcherMap.get(factory).fetch(tokenParser));
        shouldHasDalMessage(message);
    }

    public void shouldHasDalMessage(String message) {
        assertThat(dalException).hasMessage(message);
    }

    public void compileAndAssertNode(String factory, String assertion) {
        try {
            node = matcherMap.get(factory).fetch(tokenParser).orElse(null);
        } catch (DalException e) {
            System.err.println(e.show(sourceCodeString));
            throw e;
        }
        try {
            dal.evaluateAll(node, assertion);
        } catch (DalException e) {
            System.err.println(e.show(assertion));
            throw e;
        }
    }

    public void assertInputData(String assertion) {
        giveDalSourceCode(assertion);
        try {
            com.github.leeonky.dal.cucumber.Compiler compiler
                    = new com.github.leeonky.dal.cucumber.Compiler();
            compiler.compileToClasses(schemas.stream().map(s ->
                    "import com.github.leeonky.dal.type.*;\n" +
                            "import com.github.leeonky.dal.runtime.*;\n" +
                            "import java.util.*;\n" + s)
                    .collect(Collectors.toList()))
                    .forEach(dal.getRuntimeContextBuilder()::registerSchema);
            dal.evaluate(inputObject, assertion);
            dalException = null;
        } catch (DalException e) {
            dalException = e;
        }
    }

    public void shouldNoException(String assertion) {
        if (dalException != null)
            System.err.println(dalException.show(assertion));
        assertThat(dalException).isNull();
    }

    @SneakyThrows
    public void givenInputByJson(String json) {
        inputObject = new JSONArray(String.format("[%s]", json)).get(0);
    }

    public void ignoreNodeBy(String factory) {
        matcherMap.get(factory).fetch(tokenParser);
    }

    public void registerSchema(String schemaCode) {
        schemas.add(schemaCode);
    }

    public void assertEvaluateValue(String assertionCode) {
        Object evaluate;
        try {
            evaluate = dal.evaluate(inputObject, sourceCodeString);
        } catch (DalException dalException) {
            System.out.println(dalException.show(sourceCodeString));
            throw dalException;
        }

        try {
            dal.evaluateAll(evaluate, assertionCode);
        } catch (DalException dalException) {
            System.out.println(dalException.show(assertionCode));
            throw dalException;
        }
    }

    public void assertEvaluateValues(String assertionCode) {
        Object evaluate;
        try {
            evaluate = dal.evaluateAll(inputObject, sourceCodeString);
        } catch (DalException dalException) {
            System.out.println(dalException.show(sourceCodeString));
            throw dalException;
        }

        try {
            dal.evaluateAll(evaluate, assertionCode);
        } catch (DalException dalException) {
            System.out.println(dalException.show(assertionCode));
            throw dalException;
        }
    }

    public void addInputJavaClass(String sourceCode) {
        javaClasses.add(sourceCode);
    }

    @SneakyThrows
    public void assertJavaClass(String className, String assertion) {
        giveDalSourceCode(assertion);
        try {
            com.github.leeonky.dal.cucumber.Compiler compiler
                    = new com.github.leeonky.dal.cucumber.Compiler();
            List<Class<?>> classes = compiler.compileToClasses(javaClasses.stream().map(s ->
                    "import java.math.*;\n" + s).collect(Collectors.toList()));
            Class<?> type = classes.stream().filter(clazz -> clazz.getName().equals(className))
                    .findFirst().orElseThrow(() -> new IllegalArgumentException
                            ("cannot find bean class: " + className + "\nclasses: " + classes));
            dal.evaluateAll(type.newInstance(), assertion);
        } catch (DalException e) {
            dalException = e;
        }
    }
}
