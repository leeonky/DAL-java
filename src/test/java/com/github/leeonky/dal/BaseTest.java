package com.github.leeonky.dal;

import com.github.leeonky.interpreter.SourceCode;

import static com.github.leeonky.interpreter.Notation.notation;
import static java.util.Arrays.asList;

public class BaseTest {
    public static SourceCode createSourceCode(String code) {
        return new SourceCode(code, asList(notation("#"), notation("//")));
    }
}
