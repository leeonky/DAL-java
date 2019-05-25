package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.ast.opt.*;
import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import static java.util.Arrays.asList;

public class DALCompiler {
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
        LinkedList<Token> tokens = new LinkedList<>(scanner.scan(sourceCode));
        Token is = tokens.pop();
        Token type = tokens.pop();
        Node assertionExpression = tokens.isEmpty() ? new ConstNode(true) : compile(tokens);
        return new TypeAssertionExpression(new ConstNode(input), type.getWord(), assertionExpression);
    }

    private Node compile(LinkedList<Token> tokens) {
        return new Expression(new ConstNode(1), new ConstNode(1), new Equal());
    }
}
