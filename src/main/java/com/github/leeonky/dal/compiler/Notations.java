package com.github.leeonky.dal.compiler;

import com.github.leeonky.interpreter.Notation;

public class Notations {
    public static final String WHICH_S = "which";
    public static final String IS_s = "is";
    public static final String AND_s = "and";
    public static final String OR_s = "or";

    public static final Notation WHICH = new Notation("which");
    public static final Notation TRUE = new Notation("true");
    public static final Notation FALSE = new Notation("false");
    public static final Notation NULL = new Notation("null");
    public static final Notation WILDCARD = new Notation("*");
    public static final Notation ROW_WILDCARD = new Notation("***");
}
