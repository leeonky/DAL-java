package com.github.leeonky.interpreter.wip;

import com.github.leeonky.interpreter.Notation;
import com.github.leeonky.interpreter.Procedure;

import static java.lang.String.format;

public interface Closing<P extends Procedure<?, ?, ?, ?, ?>> {

    static Closing<Procedure<?, ?, ?, ?, ?>> endWith(Notation notation) {
        return procedure -> {
            if (!procedure.getSourceCode().popWord(notation.getLabel()).isPresent())
                throw procedure.getSourceCode().syntaxError(format("should end with `%s`", notation.getLabel()), 0);
        };
    }

    void doClose(P procedure);
}
