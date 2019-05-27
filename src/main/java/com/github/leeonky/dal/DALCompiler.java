package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.ast.opt.Operator;
import com.github.leeonky.dal.ast.opt.*;
import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

import java.util.Comparator;
import java.util.List;

import static com.github.leeonky.dal.ast.Operator.EQUAL;
import static java.util.Arrays.asList;

public class DALCompiler {
    public static final String WHICH = "which";
    public static final String IS = "is";
    public static final String NULL = "null";
    public static final String TRUE = "true";
    public static final String FALSE = "false";
    private static final List<Operator> OPERATOR_DP_LIST;

    static {
        OPERATOR_DP_LIST = asList(
                new Equal(),
                new NotEqual(),
                new MoreThan(),
                new MoreThanOrEqual(),
                new SmallThan(),
                new SmallThanOrEqual(),
                new Is());
        OPERATOR_DP_LIST.sort(Comparator.comparingInt(Operator::length).reversed());
    }

    private Scanner scanner = new Scanner();

    public static boolean isSpliter(SourceCode sourceCode, int position) {
        char c = sourceCode.charAt(position);
        try {
            return Character.isWhitespace(c);
        } catch (Exception e) {
            return false;
        }
    }

    public Node compile(SourceCode content) {
        Operator operator = takeOperator(content.trimLeft());
        return new Expression(InputNode.INSTANCE, compileNode(content), operator);
    }

    private Operator takeOperator(SourceCode content) {
        return OPERATOR_DP_LIST.stream()
                .filter(opt -> opt.getFrom(content))
                .findFirst().get();
    }

    private ConstNode compileNode(SourceCode content) {
        return new ConstNode(Integer.valueOf(content.trimLeft().toString()));
    }

    //=======================
    public Node compile2(SourceCode sourceCode) {
        TokenStream tokenStream = scanner.scan(sourceCode);
        tokenStream.insertFirst(Token.rootValueToken());
        return compileTokenStream(tokenStream);
    }

    private Node compileTokenStream(TokenStream tokenStream) {
        tokenStream.pop();//ROOT
        Node node = InputNode.INSTANCE;
        while (tokenStream.hasTokens()) {
            Token token = tokenStream.pop();
            node = new PropertyNode(node, token.getProperties());
        }
        return node;
    }

    private com.github.leeonky.dal.ast.Operator toOperator(Token token) {
        switch (token.getOperator()) {
            case "=":
                return EQUAL;
        }
        return null;
    }
}
