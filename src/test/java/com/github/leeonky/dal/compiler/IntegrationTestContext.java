package com.github.leeonky.dal.compiler;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.leeonky.dal.Assertions;
import com.github.leeonky.dal.BaseTest;
import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.dal.runtime.inspector.DumpingBuffer;
import com.github.leeonky.dal.runtime.inspector.ValueDumper;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.interpreter.NodeParser;
import com.github.leeonky.interpreter.SyntaxException;
import com.github.leeonky.util.Converter;
import com.github.leeonky.util.JavaCompiler;
import com.github.leeonky.util.JavaCompilerPool;
import com.github.leeonky.util.Suppressor;
import lombok.SneakyThrows;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.RuntimeException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.dal.Assertions.expect;
import static com.github.leeonky.dal.cucumber.TestTask.threadsCount;
import static com.github.leeonky.util.JavaCompiler.guessClassName;
import static java.util.Arrays.asList;
import static java.util.stream.Stream.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

public class IntegrationTestContext {
    private final DAL dal;
    private ByteArrayOutputStream waringOutput;
    private Object input = null;
    private Object result;
    private InterpreterException exception;
    private Throwable bizException;
    private String expression;
    private final List<String> schemas = new ArrayList<>();
    private final List<String> javaClasses = new ArrayList<>();
    private final Map<String, String> propertyAccessors = new LinkedHashMap<>();
    private final Map<String, String> textFormatters = new LinkedHashMap<>();
    private final Map<String, String> dALCollectionFactories = new LinkedHashMap<>();
    private final Compiler compiler = new Compiler();
    private final Map<String, NodeParser<DALNode,
            DALProcedure>> parserMap = new HashMap<String, NodeParser<DALNode,
            DALProcedure>>() {{
        put("symbol", compiler.SYMBOL);
        put("number", compiler.NUMBER);
        put("integer", compiler.INTEGER);
        put("regex", compiler.REGEX);
        put("schema", optional(compiler.SCHEMA_COMPOSE));
    }};
    private DALNode dalNode = null;
    private final Map<String, Integer> firstIndexes = new HashMap<>();
    private final List<Class<?>> classes = new ArrayList<>();
    private final JavaCompiler javaCompiler;
    private static final JavaCompilerPool JAVA_COMPILER_POOL =
            new JavaCompilerPool(threadsCount("COMPILER_THREAD_SIZE", 8) * 2, "src.test.generate.ws");
    private String registerCode = "";
    private String inputCode;

    public IntegrationTestContext() {
        dal = new DAL().extend();
        waringOutput = new ByteArrayOutputStream();
        dal.getRuntimeContextBuilder().setWarningOutput(new PrintStream(waringOutput));
        javaCompiler = JAVA_COMPILER_POOL.take();
        TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    }

    public void release() {
        JAVA_COMPILER_POOL.giveBack(javaCompiler);
        assertThat(waringOutput.toString()).isEqualTo("");
    }

    private static NodeParser<DALNode, DALProcedure> optional(
            NodeParser.Mandatory<DALNode, DALProcedure> nodeFactory) {
        return procedure -> Optional.ofNullable(nodeFactory.parse(procedure));
    }

    public void evaluate(String expression) {
        compileAll();
        givenDALExpression(expression);
        exception = null;
        result = null;
        try {
            javaCompiler.compileToClasses(schemas.stream().map(s ->
                                    "import com.github.leeonky.dal.type.*;\n" +
                                            "import com.github.leeonky.dal.runtime.*;\n" +
                                            "import java.util.*;\n" + s)
                            .collect(Collectors.toList()))
                    .forEach(schema -> {
                        dal.getRuntimeContextBuilder().registerSchema(NameStrategy.SIMPLE_NAME_WITH_PARENT, (Class) schema);
                        Arrays.stream(schema.getDeclaredClasses()).forEach(c ->
                                dal.getRuntimeContextBuilder().registerSchema(NameStrategy.SIMPLE_NAME_WITH_PARENT, (Class) c));
                    });

            propertyAccessors.forEach((type, accessor) -> {
                Class<?> beanType = classes.stream().filter(c -> c.getSimpleName().equals(type)).findFirst().get();
                Class<?> accessorType = classes.stream().filter(c -> c.getSimpleName().equals(accessor)).findFirst().get();
                Suppressor.run(() -> dal.getRuntimeContextBuilder().registerPropertyAccessor(beanType,
                        (PropertyAccessor) accessorType.newInstance()));
            });
            dALCollectionFactories.forEach((type, factory) -> {
                Class<?> beanType = classes.stream().filter(c -> c.getSimpleName().equals(type)).findFirst().get();
                Class<?> factoryType = classes.stream().filter(c -> c.getSimpleName().equals(factory)).findFirst().get();
                Suppressor.run(() -> dal.getRuntimeContextBuilder()
                        .registerDALCollectionFactory(beanType, (DALCollectionFactory) factoryType.newInstance())
                );
            });
            textFormatters.forEach((formatter, name) -> {
                Class<?> formatterClass = classes.stream().filter(c -> c.getSimpleName().equals(name)).findFirst().get();
                dal.getRuntimeContextBuilder().registerTextFormatter(formatter,
                        (TextFormatter) Suppressor.get(formatterClass::newInstance));
            });
            classes.stream().filter(t -> t.getSimpleName().equals("_DALRegister")).forEach(r ->
                    ((Consumer) Suppressor.get(() -> r.newInstance())).accept(dal));
            result = dal.evaluate(input, expression);
        } catch (InterpreterException e) {
            exception = e;
        }
    }

    public void verifyLastEvaluated(String verification) {
        if (exception != null)
            throw exception;
        expect(result).should("\n" + verification);
    }

    public void registerJavaClass(String classCode) {
        javaClasses.add(classCode);
    }

    @SneakyThrows
    public void givenJavaDataByClassName(String className) {
        compileAll();
        Class type = getType(className);
        firstIndexes.forEach((t, i) -> dal.getRuntimeContextBuilder().registerDALCollectionFactory(getType(t), (instance) ->
                new IterableDALCollection<Object>((Iterable<Object>) instance) {
                    @Override
                    public int firstIndex() {
                        return i;
                    }
                }));
        input = type.newInstance();
    }

    private Class getType(String className) {
        Class type = classes.stream().filter(clazz -> clazz.getSimpleName().equals(className))
                .findFirst().orElseThrow(() -> new IllegalArgumentException
                        ("cannot find bean class: " + className + "\nclasses: " + classes));
        return type;
    }

    private void compileAll() {
        if (classes.isEmpty()) {
            String code = String.format("public class _DALRegister implements Consumer<DAL> {\n" +
                    "    @Override\n" +
                    "    public void accept(DAL dal) {\n" +
                    "        %s\n" +
                    "    }\n" +
                    "}\n", registerCode);
            classes.addAll(javaCompiler.compileToClasses(Stream.concat(javaClasses.stream(), of(code)).map(s ->
                    "import com.github.leeonky.dal.*;\n" +
                            "import com.github.leeonky.dal.type.*;\n" +
                            "import com.github.leeonky.util.*;\n" +
                            "import com.github.leeonky.dal.runtime.*;\n" +
                            "import java.util.*;\n" +
                            "import java.util.function.*;\n" +
                            "import java.math.*;\n" + s).collect(Collectors.toList())));
            classes.forEach(dal.getRuntimeContextBuilder()::registerStaticMethodExtension);
        }
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
        assertThat(exception.getMessage()).isEqualTo(message.replace("#package#", javaCompiler.packagePrefix()));
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
                    dal.getRuntimeContextBuilder().build(null))).orElse(null);
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
        expect(input).should(dalExpression);
    }

    public void should(String expression) {
        try {
            expect(input).should(expression.replace("#package#", javaCompiler.packagePrefix()));
        } catch (Throwable e) {
            bizException = e;
        }
    }

    public void exact(String equalExpression) {
        try {
            expect(input).exact(equalExpression);
        } catch (Throwable e) {
            bizException = e;
        }
    }

    public void shouldAssertError(String message) {
        assertThat(bizException).hasMessageContaining(message);
    }

    public void exactPass(String equalExpression) {
        expect(input).exact(equalExpression);
    }

    public void matchPass(String matchingExpression) {
        expect(input).match(matchingExpression);
    }

    public void match(String matchingExpression) {
        try {
            expect(input).match(matchingExpression);
        } catch (Throwable e) {
            bizException = e;
        }
    }

    @SneakyThrows
    public void verifyDumpedData(String verification) {
        RuntimeContextBuilder.DALRuntimeContext runtimeContext = dal.getRuntimeContextBuilder().build(null);

        String dump = runtimeContext.wrap(input).dumpValue();
        assertThat(dump).isEqualTo(verification.replace("#package#", javaCompiler.packagePrefix()));
    }

    public void verifyDumpedData(String verification, int maxCount) {
        RuntimeContextBuilder builder = dal.getRuntimeContextBuilder();
        builder.setMaxDumpingLineSize(maxCount);
        RuntimeContextBuilder.DALRuntimeContext runtimeContext = builder.build(null);
        String dump = runtimeContext.wrap(input).dumpValue();
        assertThat(dump).isEqualTo(verification.replace("#package#", javaCompiler.packagePrefix()));
    }

    public void setCurryingStaticMethodArgRange(String type, String methodType, String method, List<String> range) {
        compileAll();
        dal.getRuntimeContextBuilder().registerCurryingMethodAvailableParameters(
                Arrays.stream(getType(methodType).getMethods()).filter(m -> m.getName().equals(method)
                        && m.getParameters()[0].getType().equals(getType(type))).findFirst().get(),
                (instance, args) -> new ArrayList<>(range));
    }

    @SneakyThrows
    public void setCurryingMethodArgRange2(String type, String methodName, List<Map<String, List<?>>> rangeList) {
        compileAll();
        Method method = getType(type).getMethod(methodName, rangeList.stream().map(m -> m.keySet().iterator().next())
                .map(s -> getClass(s)).toArray(Class[]::new));
        dal.getRuntimeContextBuilder().registerCurryingMethodAvailableParameters(method, (instance, args) -> {
            Map<String, List<?>> stringListMap = rangeList.get(args.size());
            return stringListMap.values().iterator().next().stream().map(a -> Converter.getInstance()
                    .convert(getClass(stringListMap.keySet().iterator().next()), a)).collect(Collectors.toList());
        });
    }

    private Class<?> getClass(String s) {
        try {
            return Class.forName(s);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    public void givenDalData(String expression) {
        input = dal.evaluate(null, expression);
    }

    public void givenDalDataList(String expression) {
        input = dal.evaluateAll(null, expression);
    }

    public void registerEmptyValueDumper() {
        dal.getRuntimeContextBuilder().registerUserDefinedLiterals(token -> {
                    if (token.equals("Empty"))
                        return Result.of(new Empty());
                    return Result.empty();
                })
                .registerDumper(Empty.class, data -> new ValueDumper() {
                    @Override
                    protected void inspectValue(Data data, DumpingBuffer dumpingBuffer) {
                    }
                });
    }

    public void givenPropertyAccessor(String type, String code) {
        javaClasses.add(code);
        propertyAccessors.put(type, guessClassName(code));
    }

    public void shouldAssertException(String expression) {
        expect(bizException).should(expression.replace("#package#", javaCompiler.packagePrefix()));
    }

    public void givenTextFormatter(String name, String code) {
        javaClasses.add(code);
        textFormatters.put(name, guessClassName(code));
    }

    public void registerDAL(String code) {
        registerCode += code + "\n";
    }

    public void givenInputCode(String code) {
        inputCode = code;
    }

    public void shouldPassByInputCode(String expression) {
        getAssertions().should(expression);
    }

    private Assertions getAssertions() {
        String assertionCode = String.format("import java.util.function.*;\n" +
                "import com.github.leeonky.dal.*;\n" +
                "public class ExpectRun implements Supplier<Assertions> {\n" +
                "        @Override\n" +
                "        public Assertions get() {\n" +
                "            return Assertions.expectRun(()-> {\n" +
                "                %s\n" +
                "            });\n" +
                "        }\n" +
                "    }", inputCode);

        return ((Supplier<Assertions>) Suppressor.get(() ->
                javaCompiler.compileToClasses(asList(assertionCode)).get(0).newInstance())).get();
    }

    public void runShould(String code) {
        try {
            getAssertions().should(code);
        } catch (Throwable e) {
            bizException = e;
        }
    }

    public void givenDALCollectionFactory(String type, String code) {
        javaClasses.add(code);
        dALCollectionFactories.put(type, guessClassName(code));
    }

    @SneakyThrows
    public void shouldHaveWarning(String verification) {
        String message = waringOutput.toString();
        waringOutput.close();
        waringOutput = new ByteArrayOutputStream();
        dal.getRuntimeContextBuilder().setWarningOutput(new PrintStream(waringOutput));
        assertThat("\n" + message).isEqualTo("\n" + verification);
    }

    public static class Empty {
    }
}
