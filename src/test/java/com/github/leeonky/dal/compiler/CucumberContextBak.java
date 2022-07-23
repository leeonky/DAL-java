package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.BaseTest;
import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.cucumber.JSONArrayAccessor;
import com.github.leeonky.dal.cucumber.JSONObjectAccessor;
import com.github.leeonky.dal.runtime.ListAccessor;
import com.github.leeonky.interpreter.InterpreterException;
import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.Assertions.expect;
import static org.assertj.core.api.Assertions.assertThat;

@Deprecated
public class CucumberContextBak {

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

    public void shouldShowSourceCodePosition(String sourceCodePosition) {
        assertThat("\n" + interpreterException.show(sourceCodeString)).isEqualTo("\n" + sourceCodePosition);
    }

    public void shouldHasDalMessage(String message) {
        assertThat(interpreterException).hasMessage(message);
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
                    public int firstIndex(Object instance) {
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

        @Override
        public String toString() {
            return amount + "$";
        }
    }
}
