package com.github.leeonky.dal.cucumber;

import lombok.SneakyThrows;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import java.io.File;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.groupingBy;
import static javax.tools.ToolProvider.getSystemJavaCompiler;

public class Compiler {
    private URLClassLoader loader = getUrlClassLoader();

    @SneakyThrows
    private URLClassLoader getUrlClassLoader() {
        return URLClassLoader.newInstance(new URL[]{new File("").toURI().toURL()});
    }

    private String guessClassName(String schemaCode) {
        Matcher matcher = Pattern.compile(".* class\\s(.*)\\s\\{.*", Pattern.DOTALL).matcher(schemaCode);
        if (matcher.matches())
            return matcher.group(1).trim();
        throw new IllegalStateException("Can not guess class name");
    }

    public List<Class<?>> compileToClasses(List<String> classCodes) {
        if (classCodes.isEmpty())
            return emptyList();
        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
        List<JavaSourceFromString> files = classCodes.stream().map(code ->
                new JavaSourceFromString(guessClassName(code), code)).collect(Collectors.toList());
        boolean success = getSystemJavaCompiler().getTask(null, null, diagnostics, null, null, files).call();
        if (!success) {
            System.out.println(diagnostics.getDiagnostics().stream().collect(groupingBy(Diagnostic::getSource))
                    .entrySet().stream().map(this::compileResults).collect(Collectors.joining("\n")));
            throw new IllegalStateException("Failed to compile java code: \n");
        }
        return files.stream().map(f -> f.name).map(this::loadClass).collect(Collectors.toList());
    }

    @SneakyThrows
    private String compileResults(Map.Entry<? extends JavaFileObject, List<Diagnostic<? extends JavaFileObject>>> e) {
        String sourceCode = String.valueOf(e.getKey().getCharContent(true));
        Object[] codeBase = sourceCode.chars().mapToObj(c -> c == '\n' ? (char) c : ' ').map(String::valueOf).toArray();
        List<String> result = new ArrayList<>();
        result.add(e.getKey().toString());
        for (Diagnostic<?> diagnostic : e.getValue()) {
            result.add(diagnostic.getMessage(null));
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
        return Class.forName(name, true, loader);
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