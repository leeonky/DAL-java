package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.cucumber.JSONArrayAccessor;
import com.github.leeonky.dal.cucumber.JSONObjectAccessor;
import com.github.leeonky.dal.runtime.ListAccessor;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.BaseTest;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.interpreter.NodeParser;
import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.*;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.extension.assertj.DALAssert.expect;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

@Deprecated
public class CucumberContextBak {
    private static final Compiler compiler = new Compiler();
    private static final Map<String, NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALProcedure>> matcherMap = new HashMap<String, NodeParser<DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALProcedure>>() {{
        put("number", compiler.NUMBER);
        put("integer", compiler.INTEGER);
        put("single-quoted-string", compiler.SINGLE_QUOTED_STRING);
        put("double-quoted-string", compiler.DOUBLE_QUOTED_STRING);
        put("const-true", compiler.CONST_TRUE);
        put("const-false", compiler.CONST_FALSE);
        put("const-null", compiler.CONST_NULL);
        put("const", compiler.CONST);
        put("regex", compiler.REGEX);
        put("explicit-property", compiler.EXPLICIT_PROPERTY.defaultInputNode(InputNode.INSTANCE));
        put("property", compiler.PROPERTY);
        put("operand", optional(compiler.OPERAND));
//        put("binary-operator-expression", compiler.BINARY_OPERATOR_EXPRESSION.defaultInputNode(InputNode.INSTANCE));
        put("expression", optional(compiler.EXPRESSION));
        put("parentheses", compiler.PARENTHESES);
        put("object", compiler.OBJECT);
        put("list", compiler.LIST);
        put("judgement-expression-operand", optional(compiler.SHORT_VERIFICATION_OPERAND_bk));
        put("table", compiler.TABLE);
        put("schema", compiler.SCHEMA);
        put("symbol", compiler.SYMBOL);
    }};

    private static NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> optional(
            NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeFactory) {
        return procedure -> Optional.ofNullable(nodeFactory.parse(procedure));
    }

    public static CucumberContextBak INSTANCE = new CucumberContextBak();
    DAL dal = new DAL();

    Object inputObject = null;
    DALProcedure dalProcedure = null;
    String sourceCodeString = null;
    InterpreterException interpreterException;
    DALNode node = null;
    private final List<String> schemas = new ArrayList<>();
    private final List<String> javaClasses = new ArrayList<>();

    public static void reset() {
        INSTANCE = new CucumberContextBak();
        INSTANCE.initDataAssert();
    }

    private void initDataAssert() {
        dal.getRuntimeContextBuilder()
                .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                .registerListAccessor(JSONArray.class, new JSONArrayAccessor());
    }

    public void giveDalSourceCode(String code) {
        dalProcedure = new DALProcedure(BaseTest.createSourceCode(sourceCodeString = parseTabAndSpace(code)),
                dal.getRuntimeContextBuilder().build(null), DALExpression::new);
    }

    private String parseTabAndSpace(String code) {
        return code.replace("`TAB", "\t").replace("`SPACE", " ");
    }

    public void assertLastNodeValue(String assertion) {
        expect(node.evaluate(dal.getRuntimeContextBuilder().build(inputObject))).should(assertion);
    }

    public void assertNodeValue(String assertion, String factory) {
        Object evaluate = matcherMap.get(factory).parse(new DALProcedure(BaseTest.createSourceCode(sourceCodeString),
                dal.getRuntimeContextBuilder().build(null), DALExpression::new)).orElse(null)
                .evaluate(dal.getRuntimeContextBuilder().build(null));
        expect(evaluate).should(assertion);
    }

    public void shouldShowSourceCodePosition(String sourceCodePosition) {
        assertThat("\n" + interpreterException.show(sourceCodeString)).isEqualTo("\n" + sourceCodePosition);
    }

    public void failedToGetNodeWithMessage(String factory, String message) {
        interpreterException = assertThrows(InterpreterException.class, () -> matcherMap.get(factory).parse(dalProcedure));
        shouldHasDalMessage(message);
    }

    public void shouldHasDalMessage(String message) {
        assertThat(interpreterException).hasMessage(message);
    }

    public void compileAndAssertNode(String factory, String assertion) {
        try {
            node = matcherMap.get(factory).parse(dalProcedure).orElse(null);
        } catch (InterpreterException e) {
            System.err.println(e.show(sourceCodeString));
            throw e;
        }
        expect(node).should(assertion);
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
            interpreterException = null;
        } catch (InterpreterException e) {
            interpreterException = e;
        }
    }

    public void shouldNoException(String assertion) {
        if (interpreterException != null)
            System.err.println(interpreterException.show(assertion));
        assertThat(interpreterException).isNull();
    }

    @SneakyThrows
    public void givenInputByJson(String json) {
        inputObject = new JSONArray(String.format("[%s]", json)).get(0);
    }

    public void ignoreNodeBy(String factory) {
        matcherMap.get(factory).parse(dalProcedure);
    }

    public void registerSchema(String schemaCode) {
        schemas.add(schemaCode);
    }

    public void assertEvaluateValue(String assertionCode) {
        Object evaluate;
        try {
            evaluate = dal.evaluate(inputObject, sourceCodeString);
        } catch (InterpreterException dalException) {
            System.out.println(dalException.show(sourceCodeString));
            throw dalException;
        }

        expect(evaluate).should(assertionCode);
    }

    public void assertEvaluateValues(String assertionCode) {
        Object evaluate;
        try {
            evaluate = dal.evaluateAll(inputObject, sourceCodeString);
        } catch (InterpreterException dalException) {
            System.out.println(dalException.show(sourceCodeString));
            throw dalException;
        }
        expect(evaluate).should(assertionCode);
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
            Class type = classes.stream().filter(clazz -> clazz.getName().equals(className))
                    .findFirst().orElseThrow(() -> new IllegalArgumentException
                            ("cannot find bean class: " + className + "\nclasses: " + classes));
            if (firstIndexes.containsKey(className)) {
                dal.getRuntimeContextBuilder().registerListAccessor(type, new ListAccessor<Object>() {
                    @Override
                    public Iterable<?> toIterable(Object instance) {
                        return (Iterable<?>) instance;
                    }

                    @Override
                    public int firstIndex() {
                        return firstIndexes.get(className);
                    }
                });
            }
            dal.evaluateAll(type.newInstance(), assertion);
        } catch (InterpreterException e) {
            interpreterException = e;
        }
    }

    public void setArrayFirstIndex(String type, int index) {
        firstIndexes.put(type, index);
    }

    private final Map<String, Integer> firstIndexes = new HashMap<>();

    @Getter
    @Setter
    public static class USDollar {
        private int amount;

        public USDollar(int amount) {
            this.amount = amount;
        }
    }
}
