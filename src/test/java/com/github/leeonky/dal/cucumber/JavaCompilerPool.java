package com.github.leeonky.dal.cucumber;

import com.github.leeonky.util.Suppressor;

import java.util.concurrent.BlockingDeque;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.toCollection;

public class JavaCompilerPool {
    private final BlockingDeque<Integer> workspaces;
    private final String generatePackage;

    public JavaCompilerPool(int maxCount, String generatePackage) {
        workspaces = IntStream.range(0, maxCount).boxed().collect(toCollection(LinkedBlockingDeque::new));
        this.generatePackage = generatePackage;
    }

    public JavaCompiler take() {
        return new JavaCompiler(generatePackage, Suppressor.get(workspaces::takeFirst));
    }

    public void giveBack(JavaCompiler compiler) {
        Suppressor.run(() -> workspaces.putLast(compiler.getId()));
    }
}
