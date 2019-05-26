package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.ast.opt.*;
import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

import java.util.Comparator;
import java.util.List;

import static java.util.Arrays.asList;

public class DALCompiler {
    public static final String WHICH = "which";
    public static final String IS = "is";
    public static final String NULL = "null";
    private static final List<Operator> operatorList;

    static {
        operatorList = asList(
                new Equal(),
                new NotEqual(),
                new MoreThan(),
                new MoreThanOrEqual(),
                new SmallThan(),
                new SmallThanOrEqual(),
                new Is());
        operatorList.sort(Comparator.comparingInt(Operator::length).reversed());
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
        return new Expression(new InputNode(), compileNode(content), operator);
    }

    private Operator takeOperator(SourceCode content) {
        return operatorList.stream()
                .filter(opt -> opt.getFrom(content))
                .findFirst().get();
    }

    private ConstNode compileNode(SourceCode content) {
        return new ConstNode(Integer.valueOf(content.trimLeft().toString()));
    }

    //=======================
    public Node compile2(Object input, SourceCode sourceCode) {
        TokenStream tokenStream = scanner.scan(sourceCode);
        String type;
        if (tokenStream.matchAndTakeKeyWord(IS)) {
            type = tokenStream.pop().getWord();
            tokenStream.matchAndTakeKeyWord(WHICH);
        } else
            type = "Object";
        Node assertionExpression = tokenStream.hasTokens() ? compile(tokenStream) : new ConstNode(true);
        return new TypeAssertionExpression(new ConstNode(input), type, assertionExpression);
    }

    private Node compile(TokenStream tokenStream) {
        Token token1 = tokenStream.pop();
        Token tokenOpt = tokenStream.pop();
        Token token2 = tokenStream.pop();
        return new Expression(new ConstNode(token1.getNumber()), new ConstNode(token2.getNumber()), toOperator(tokenOpt));
    }

    private Operator toOperator(Token token) {
        switch (token.getOperator()) {
            case "=":
                return new Equal();
        }
        return null;
    }
}
