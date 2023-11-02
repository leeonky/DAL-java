package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Notation;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static com.github.leeonky.interpreter.Notation.notation;
import static java.util.Arrays.asList;

public class Notations {


    public static class Keywords {
        public static final Notation<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression>
                WHICH = notation("which"),
                IS = notation("is"),
                TRUE = notation("true"),
                FALSE = notation("false"),
                NULL = notation("null"),
                AND = notation("and"),
                OR = notation("or");

        public static final Set<Notation<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression>>
                ALL = new HashSet<>(asList(WHICH, IS, TRUE, FALSE, NULL, AND, OR));
        public static final Set<String> ALL_STRING = ALL.stream().map(Notation::getLabel).collect(Collectors.toSet());
    }

    public static class Operators {
        public static final Notation<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression>
                WILDCARD = notation("*"),
                ROW_WILDCARD = notation("***"),
                ELEMENT_ELLIPSIS = notation("..."),
                AND = notation("&&"),
                OR = notation("||"),
                COMMA = notation(","),
                GREATER_OR_EQUAL = notation(">="),
                LESS_OR_EQUAL = notation("<="),
                GREATER = notation(">"),
                LESS = notation("<"),
                PLUS = notation("+"),
                SUBTRACTION = notation("-"),
                MULTIPLICATION = notation("*"),
                DIVISION = notation("/"),
                NOT_EQUAL = notation("!="),
                MINUS = notation("-"),
                NOT = notation("!"),
                MATCHER = notation(":"),
                EQUAL = notation("="),
                DOT = notation("."),
                SLASH = notation("/"),
                META = notation("::"),
                IS = Keywords.IS,
                WHICH = Keywords.WHICH,
                DATA_REMARK = notation("("),
                EXCLAMATION = notation("!");
    }

    public static final Notation<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression>
            SINGLE_QUOTED = notation("'"),
            DOUBLE_QUOTED = notation("\""),
            OPENING_BRACKET = notation("["),
            CLOSING_BRACKET = notation("]"),
            OPENING_PARENTHESES = notation("("),
            CLOSING_PARENTHESES = notation(")"),
            SCHEMA_AND = notation("/"),
            OPEN_REGEX = notation("/"),
            CLOSE_REGEX = notation("/"),
            OPENING_BRACES = notation("{"),
            CLOSING_BRACES = notation("}"),
            COMMA = notation(","),
            COLUMN_SPLITTER = notation("|"),
            MATRIX_COLUMN_SPLITTER = notation("^"),
            SEQUENCE_AZ = notation("+"),
            SEQUENCE_ZA = notation("-"),
            SEQUENCE_AZ_2 = notation("￪"),
            SEQUENCE_ZA_2 = notation("￬"),
            TRANSPOSE_MARK = notation(">>"),
            LINE_COMMENT1 = notation("#"),
            LINE_COMMENT2 = notation("//"),
            LIST_MAPPING = notation("[]"),
            OPENING_GROUP = notation("<<"),
            CLOSING_GROUP = notation(">>"),
            TEXT_BLOCK = notation("`"),
            EMPTY = notation(""),
            THIS = notation("{}");

    public final static List<Notation<?, ?, ?, ?, ?>> LINE_COMMENTS = asList(LINE_COMMENT1, LINE_COMMENT2);
}
