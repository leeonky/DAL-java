package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

import java.util.Optional;

import static com.github.leeonky.dal.ast.Operator.*;
import static java.util.Optional.ofNullable;

public class DALCompiler {
    public static final String WHICH = "which";
    public static final String IS = "is";
    public static final String NULL = "null";
    public static final String TRUE = "true";
    public static final String FALSE = "false";
    private Scanner scanner = new Scanner();

    public Node compile(SourceCode sourceCode) {
        return compileTokenStream(scanner.scan(sourceCode));
    }

    private Node compileTokenStream(TokenStream tokenStream) {
        Node node = compileValueNode(tokenStream).orElse(InputNode.INSTANCE);
        if (tokenStream.hasTokens()) {
            Operator operator = toOperator(tokenStream.pop());
            Node node2 = compileValueNode(tokenStream).orElseThrow(() -> new SyntexException(1, "expression not finished"));
            return new Expression(node, node2, operator);
        } else
            return node;
    }

    private Optional<Node> compileValueNode(TokenStream tokenStream) {
        Node node = null;
        if (tokenStream.hasTokens()) {
            if (tokenStream.currentType() == Token.Type.CONST_VALUE)
                node = new ConstNode(tokenStream.pop().getConstValue());
            else if (tokenStream.isCurrentSingleEvaluateNode())
                node = InputNode.INSTANCE;
        }
        if (node != null)
            while (tokenStream.hasTokens() && tokenStream.isCurrentSingleEvaluateNode())
                node = new PropertyNode(node, tokenStream.pop().getProperties());
        return ofNullable(node);
    }

    private Operator toOperator(Token token) {
        String operator = token.getOperator();
        switch (operator) {
            case "=":
                return EQUAL;
            case ">":
                return GREATER;
            case "<":
                return LESS;
            case ">=":
                return GREATER_OR_EQUAL;
            case "<=":
                return LESS_OR_EQUAL;
            case "!=":
                return NOT_EQUAL;
        }
        throw new SyntexException(-1, "not support operator " + operator + " yet");
    }
}
