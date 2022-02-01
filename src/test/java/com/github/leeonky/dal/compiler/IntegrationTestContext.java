package com.github.leeonky.dal.compiler;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Result;
import com.github.leeonky.interpreter.InterpreterException;
import lombok.SneakyThrows;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.extension.assertj.DALAssert.expect;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

public class IntegrationTestContext {
    private final DAL dal = new DAL();
    private Object input = null;
    private Object result;
    private InterpreterException exception;
    private String expression;
    private final List<String> javaClasses = new ArrayList<>();

    public void evaluate(String expression) {
        this.expression = expression;
        try {
            result = dal.evaluate(input, expression);
        } catch (InterpreterException e) {
            exception = e;
        }
    }

    public void verifyLastEvaluated(String verification) {
        expect(result).should("\n" + verification);
    }

    public void registerJavaClass(String classCode) {
        javaClasses.add(classCode);
    }

    @SneakyThrows
    public void givenJavaDataByClassName(String className) {
        com.github.leeonky.dal.cucumber.Compiler compiler
                = new com.github.leeonky.dal.cucumber.Compiler();
        List<Class<?>> classes = compiler.compileToClasses(javaClasses.stream().map(s ->
                "import java.math.*;\n" + s).collect(Collectors.toList()));
        Class type = classes.stream().filter(clazz -> clazz.getName().equals(className))
                .findFirst().orElseThrow(() -> new IllegalArgumentException
                        ("cannot find bean class: " + className + "\nclasses: " + classes));
        input = type.newInstance();
    }

    public void shouldPass() {
        if (exception != null)
            fail("Last evaluation has exception:\n" + exception.show(expression) + "\n" + exception.getMessage());
    }

    public void registerUSMoney(String regex) {
        dal.getRuntimeContextBuilder().registerUserDefinedLiterals(token -> token.matches(regex) ?
                Result.of(new CucumberContextBak.USDollar(Integer.parseInt(token.replace("$", "")))) : Result.empty());
    }

    @SneakyThrows
    public void givenJsonData(String json) {
        input = new ObjectMapper().readValue(String.format("[%s]", json), List.class).get(0);
    }

    public void evaluateAll(String expression) {
        this.expression = expression;
        try {
            result = dal.evaluateAll(input, expression);
        } catch (InterpreterException e) {
            exception = e;
        }
    }

    public void shouldFailedWith(String message) {
        assertThat(exception.getMessage()).isEqualTo(message);
    }

    public void shouldHaveNotation(String notation) {
        assertThat("\n" + exception.show(expression)).isEqualTo("\n" + notation);
    }
}
