package com.github.leeonky.dal.cucumber;

import lombok.SneakyThrows;

import javax.tools.*;
import java.io.File;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toCollection;
import static javax.tools.ToolProvider.getSystemJavaCompiler;

public class Compiler {
    private final URLClassLoader loader = getUrlClassLoader();
    private final String packageName;

    private static final BlockingDeque<Compiler> compilers = initCompilers();

    private static BlockingDeque<Compiler> initCompilers() {
        return IntStream.range(0, 0).mapToObj(Compiler::new).collect(toCollection(LinkedBlockingDeque::new));
    }

    public static Compiler take() {
        return compilers.pop();
    }

    public void giveBack() {
        compilers.push(this);
    }

    public Compiler(int index) {
        packageName = "src.test" + index;
    }

    @SneakyThrows
    private URLClassLoader getUrlClassLoader() {
        return URLClassLoader.newInstance(new URL[]{new File("").toURI().toURL()});
    }

    private String guessClassName(String schemaCode) {
        String s = Stream.of(schemaCode.split("\n")).filter(l -> l.contains("class") || l.contains("interface"))
                .findFirst().orElse(null);
        Matcher matcher = Pattern.compile(".* class\\s(.*)\\sextends.*", Pattern.DOTALL).matcher(s);
        if (matcher.matches())
            return matcher.group(1).trim();
        matcher = Pattern.compile(".* class\\s(.*)\\simplements.*", Pattern.DOTALL).matcher(s);
        if (matcher.matches())
            return matcher.group(1).trim();
        matcher = Pattern.compile(".* class\\s([^{]*)\\s\\{.*", Pattern.DOTALL).matcher(s);
        if (matcher.matches())
            return matcher.group(1).trim();
        matcher = Pattern.compile(".* interface\\s([^{]*)\\s\\{.*", Pattern.DOTALL).matcher(s);
        if (matcher.matches())
            return matcher.group(1).trim();
        matcher = Pattern.compile(".* interface\\s(.*)\\sextends.*", Pattern.DOTALL).matcher(s);
        if (matcher.matches())
            return matcher.group(1).trim();
        throw new IllegalStateException("Can not guess class name");
    }

    @SneakyThrows
    public List<Class<?>> compileToClasses(List<String> classCodes) {
        if (classCodes.isEmpty())
            return emptyList();
        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
        List<JavaSourceFromString> files = classCodes.stream().map(code ->
                        new JavaSourceFromString(guessClassName(code).replaceAll("<.*>", ""), declarePackage() + code))
                .collect(Collectors.toList());
        JavaCompiler systemJavaCompiler = getSystemJavaCompiler();
        StandardJavaFileManager standardFileManager = systemJavaCompiler.getStandardFileManager(diagnostics, null, null);
        standardFileManager.setLocation(StandardLocation.CLASS_OUTPUT, asList(new File("./")));
        boolean success = systemJavaCompiler.getTask(null, standardFileManager, diagnostics, null, null, files).call();
        if (!success) {
            System.out.println(diagnostics.getDiagnostics().stream().collect(groupingBy(Diagnostic::getSource))
                    .entrySet().stream().map(this::compileResults).collect(Collectors.joining("\n")));
            throw new IllegalStateException("Failed to compile java code: \n");
        }
        return files.stream().map(f -> f.name).map(this::loadClass).collect(Collectors.toList());
    }

    private String declarePackage() {
        return packageName.isEmpty() ? "" : "package " + packageName + ";";
    }

    @SneakyThrows
    private String compileResults(Map.Entry<? extends JavaFileObject, List<Diagnostic<? extends JavaFileObject>>> e) {
        String sourceCode = String.valueOf(e.getKey().getCharContent(true));
        Object[] codeBase = sourceCode.chars().mapToObj(c -> c == '\n' ? (char) c : ' ').map(String::valueOf).toArray();
        List<String> result = new ArrayList<>();
        result.add(e.getKey().toString());
        for (Diagnostic<?> diagnostic : e.getValue()) {
            result.add(diagnostic.getMessage(null));
            if (diagnostic.getPosition() >= 0 && diagnostic.getPosition() < codeBase.length)
                codeBase[(int) diagnostic.getPosition()] = '^';
        }
        String[] codes = sourceCode.split("\n");
        String[] codeMarks = Stream.of(codeBase).map(String::valueOf).collect(Collectors.joining()).split("\n");
        for (int i = 0; i < codes.length; i++) {
            result.add(codes[i]);
            if (i < codeMarks.length && !codeMarks[i].trim().isEmpty())
                result.add(codeMarks[i]);
        }
        return String.join("\n", result);
    }

    @SneakyThrows
    private Class<?> loadClass(String name) {
        return Class.forName(packagePrefix() + name, true, loader);
    }

    public String packagePrefix() {
        return packageName.isEmpty() ? "" : packageName + ".";
    }
}

class JavaSourceFromString extends SimpleJavaFileObject {
    final String name;
    private final String code;

    JavaSourceFromString(String name, String code) {
        super(URI.create("string:///" + name.replace('.', '/') + Kind.SOURCE.extension), Kind.SOURCE);
        this.name = name;
        this.code = code;
    }

    @Override
    public CharSequence getCharContent(boolean ignoreEncodingErrors) {
        return code;
    }
}
