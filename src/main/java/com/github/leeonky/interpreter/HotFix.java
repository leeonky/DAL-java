package com.github.leeonky.interpreter;

import com.github.leeonky.dal.compiler.DALProcedure;

public class HotFix {
    public static String getCode(DALProcedure procedure) {
        return procedure.getSourceCode().charStream().getCode();
    }
}
