package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.BaseTest;
import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.cucumber.JSONArrayDALCollectionFactory;
import com.github.leeonky.dal.cucumber.JSONObjectAccessor;
import com.github.leeonky.dal.runtime.IterableDALCollection;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.util.JavaCompiler;
import com.github.leeonky.util.JavaCompilerPool;
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
import static com.github.leeonky.dal.cucumber.TestTask.threadsCount;
import static org.assertj.core.api.Assertions.assertThat;

@Deprecated
public class CucumberContextBak {
    private final JavaCompiler javaCompiler;

    DAL dal = new DAL().extend();

    Object inputObject = null;
    DALProcedure dalProcedure = null;
    String sourceCodeString = null;
    InterpreterException interpreterException;
    DALNode node = null;
    private final List<String> schemas = new ArrayList<>();
    private final List<String> javaClasses = new ArrayList<>();
    private static final JavaCompilerPool JAVA_COMPILER_POOL =
            new JavaCompilerPool(threadsCount("COMPILER_THREAD_SIZE", 8) * 2, "src.test.generate.wsbk");

    public CucumberContextBak() {
        dal.getRuntimeContextBuilder()
                .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                .registerDALCollectionFactory(JSONArray.class, new JSONArrayDALCollectionFactory())
        ;
        javaCompiler = JAVA_COMPILER_POOL.take();
    }

    public void release() {
        JAVA_COMPILER_POOL.giveBack(javaCompiler);
    }

    public void giveDalSourceCode(String code) {
        dalProcedure = new DALProcedure(BaseTest.createSourceCode(sourceCodeString = parseTabAndSpace(code)),
                dal.getRuntimeContextBuilder().build(null));
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
            javaCompiler.compileToClasses(schemas.stream().map(s ->
                                    "import com.github.leeonky.dal.type.*;\n" +
                                            "import com.github.leeonky.dal.runtime.*;\n" +
                                            "import java.util.*;\n" + s)
                            .collect(Collectors.toList()))
                    .forEach(c -> dal.getRuntimeContextBuilder().registerSchema((Class) c));
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
            List<Class<?>> classes = javaCompiler.compileToClasses(javaClasses.stream().map(s ->
                    "import java.math.*;\n" + s).collect(Collectors.toList()));
            Class type = classes.stream().filter(clazz -> clazz.getName().equals(className))
                    .findFirst().orElseThrow(() -> new IllegalArgumentException
                            ("cannot find bean class: " + className + "\nclasses: " + classes));
            if (firstIndexes.containsKey(className)) {
                dal.getRuntimeContextBuilder().registerDALCollectionFactory(type, (instance) ->
                        new IterableDALCollection((Iterable) instance) {
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

        @Override
        public String toString() {
            return amount + "$";
        }
    }
}
