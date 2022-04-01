package com.github.leeonky.dal.compiler;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.leeonky.dal.Assertions;
import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.ListAccessor;
import com.github.leeonky.dal.runtime.NameStrategy;
import com.github.leeonky.dal.runtime.Result;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.BaseTest;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.interpreter.NodeParser;
import com.github.leeonky.interpreter.SyntaxException;
import lombok.SneakyThrows;

import java.util.*;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.extension.assertj.DALAssert.expect;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

public class IntegrationTestContext {
    private final DAL dal = new DAL();
    private Object input = null;
    private Object result;
    private InterpreterException exception;
    private Throwable bizException;
    private String expression;
    private final List<String> schemas = new ArrayList<>();
    private final List<String> javaClasses = new ArrayList<>();

    private static final Compiler compiler = new Compiler();
    private static final Map<String, NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALProcedure>> parserMap = new HashMap<String, NodeParser<DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALProcedure>>() {{
        put("symbol", compiler.SYMBOL);
        put("number", compiler.NUMBER);
        put("integer", compiler.INTEGER);
        put("regex", compiler.REGEX);
        put("schema", optional(compiler.SCHEMA_COMPOSE));
    }};
    private DALNode dalNode = null;
    private Map<String, Integer> firstIndexes = new HashMap<>();

    private static NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> optional(
            NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeFactory) {
        return procedure -> Optional.ofNullable(nodeFactory.parse(procedure));
    }

    public void evaluate(String expression) {
        givenDALExpression(expression);
        exception = null;
        result = null;
        try {
            com.github.leeonky.dal.cucumber.Compiler compiler
                    = new com.github.leeonky.dal.cucumber.Compiler();
            compiler.compileToClasses(schemas.stream().map(s ->
                    "import com.github.leeonky.dal.type.*;\n" +
                            "import com.github.leeonky.dal.runtime.*;\n" +
                            "import java.util.*;\n" + s)
                    .collect(Collectors.toList()))
                    .forEach(schema -> {
                        dal.getRuntimeContextBuilder().registerSchema(NameStrategy.SIMPLE_NAME_WITH_PARENT, schema);
                        Arrays.stream(schema.getDeclaredClasses()).forEach(c ->
                                dal.getRuntimeContextBuilder().registerSchema(NameStrategy.SIMPLE_NAME_WITH_PARENT, c));
                    });
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
        classes.forEach(dal.getRuntimeContextBuilder()::registerStaticMethodExtension);
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
        givenDALExpression(expression);
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

    public void givenDALExpression(String expression) {
        this.expression = expression.replace("`TAB", "\t").replace("`SPACE", " ");
    }

    public void verifyNode(String factory, String verification) {
        expect(parseNode(factory)).should(verification);
    }

    public DALNode parseNode(String factory) {
        try {
            return dalNode = parserMap.get(factory).parse(new DALProcedure(BaseTest.createSourceCode(expression),
                    dal.getRuntimeContextBuilder().build(null), DALExpression::new)).orElse(null);
        } catch (InterpreterException e) {
            exception = e;
            return null;
        }
    }

    public void parseAndEvaluate(String expression, String nodeType) {
        givenDALExpression(expression);
        try {
            result = parseNode(nodeType).evaluate(dal.getRuntimeContextBuilder().build(input));
        } catch (InterpreterException e) {
            exception = e;
        }
    }

    public void evaluateLast() {
        try {
            result = dalNode.evaluate(dal.getRuntimeContextBuilder().build(input));
        } catch (InterpreterException e) {
            exception = e;
        }
    }

    public void shouldFailed() {
        assertThat(exception).isInstanceOf(DalException.class);
    }

    public void shouldSyntaxError() {
        assertThat(exception).isInstanceOf(SyntaxException.class);
    }

    public void givenSchemaClass(String schema) {
        schemas.add(schema);
    }

    public void verifyInspect(String inspect) {
        assertThat(compiler.compile(BaseTest.createSourceCode(expression), dal.getRuntimeContextBuilder().build(null)).get(0).inspect())
                .isEqualTo(inspect);
    }

    public void setArrayFirstIndex(String type, int index) {
        firstIndexes.put(type, index);
    }

    public void shouldPass(String dalExpression) {
        Assertions.expect(input).should(dalExpression);
    }

    public void should(String dalExpression) {
        try {
            Assertions.expect(input).should(dalExpression);
        } catch (Throwable e) {
            bizException = e;
        }
    }

    public void exact(String equalExpression) {
        try {
            Assertions.expect(input).exact(equalExpression);
        } catch (Throwable e) {
            bizException = e;
        }
    }

    public void shouldAssertError(String message) {
        assertThat(bizException).isInstanceOf(AssertionError.class).hasMessage(message);
    }

    public void exactPass(String equalExpression) {
        Assertions.expect(input).exact(equalExpression);
    }

    public void matchPass(String matchingExpression) {
        Assertions.expect(input).match(matchingExpression);
    }

    public void match(String matchingExpression) {
        try {
            Assertions.expect(input).match(matchingExpression);
        } catch (Throwable e) {
            bizException = e;
        }
    }

}
