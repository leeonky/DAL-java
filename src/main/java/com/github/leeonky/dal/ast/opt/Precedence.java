package com.github.leeonky.dal.ast.opt;

public class Precedence {
    static final int WHICH = 100;
    static final int LOGICAL = 200;
    static final int VERIFICATION = 210;
    static final int COMPARISON = 220;
    static final int PLUS_SUB = 300;
    static final int MUL_DIV = 400;
    static final int UNARY_OPERATION = 500;
    static final int PROPERTY = 501;
    static final int REMARK_EXCLAMATION = 501;
}
