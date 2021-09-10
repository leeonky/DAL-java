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
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Collections.singletonList;
import static javax.tools.ToolProvider.getSystemJavaCompiler;

public class Compiler {
    @SneakyThrows
    public Class<?> compileToClass(String schemaCode) {
        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
        String className = guessClassName(schemaCode);
        boolean success = getSystemJavaCompiler().getTask(null, null, diagnostics,
                null, null, singletonList(new JavaSourceFromString(className, schemaCode))).call();
        Object[] codeBase = schemaCode.chars().mapToObj(c -> c == '\n' ? (char) c : ' ').map(String::valueOf).toArray();
        for (Diagnostic<?> diagnostic : diagnostics.getDiagnostics()) {
            System.out.println(diagnostic.getSource());
            System.out.println(diagnostic.getMessage(null));
            codeBase[(int) diagnostic.getPosition()] = '^';
        }
        String[] codes = schemaCode.split("\n");
        String[] codeMarks = Stream.of(codeBase).map(String::valueOf).collect(Collectors.joining()).split("\n");
        List<String> result = new ArrayList<>();
        for (int i = 0; i < codes.length; i++) {
            result.add(codes[i]);
            if (i < codeMarks.length && !codeMarks[i].trim().isEmpty())
                result.add(codeMarks[i]);
        }
        if (!success) {
            System.out.println(String.join("\n", result));
            throw new IllegalStateException("Failed to compile java code: \n" + schemaCode);
        }
        return Class.forName(className, true,
                URLClassLoader.newInstance(new URL[]{new File("").toURI().toURL()}));
    }

    private String guessClassName(String schemaCode) {
        Matcher matcher = Pattern.compile(".* class\\s(.*)\\s\\{.*", Pattern.DOTALL).matcher(schemaCode);
        if (matcher.matches())
            return matcher.group(1).trim();
        throw new IllegalStateException("Can not guess class name");
    }
}

class JavaSourceFromString extends SimpleJavaFileObject {
    final String code;

    JavaSourceFromString(String name, String code) {
        super(URI.create("string:///" + name.replace('.', '/') + Kind.SOURCE.extension), Kind.SOURCE);
        this.code = code;
    }

    @Override
    public CharSequence getCharContent(boolean ignoreEncodingErrors) {
        return code;
    }
}
