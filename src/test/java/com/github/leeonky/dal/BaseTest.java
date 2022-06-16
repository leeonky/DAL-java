package com.github.leeonky.dal;

import com.github.leeonky.interpreter.Notation;
import com.github.leeonky.interpreter.SourceCode;

import static com.github.leeonky.interpreter.Notation.notation;
import static java.util.Arrays.asList;

public class BaseTest {
    public static SourceCode createSourceCode(String code) {
        return SourceCode.createSourceCode(code, asList(notation("#"), notation("//")));
    }

    public SourceCode createSourceCodeWithBlank(String s) {
        SourceCode sourceCode = createSourceCode("blabla" + s);
        sourceCode.popWord(Notation.notation("blabla"));
        return sourceCode;
    }
}
