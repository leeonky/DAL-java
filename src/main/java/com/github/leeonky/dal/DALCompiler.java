package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

import static com.github.leeonky.dal.ast.Operator.EQUAL;

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
        if (tokenStream.hasTokens()) {
            return compileValueNode(tokenStream);
        }
        return InputNode.INSTANCE;
    }

    private Node compileValueNode(TokenStream tokenStream) {
        Node node;
        if (tokenStream.currentType() == Token.Type.CONST_VALUE)
            node = new ConstNode(tokenStream.pop().getConstValue());
        else
            node = InputNode.INSTANCE;
        while (tokenStream.hasTokens())
            node = new PropertyNode(node, tokenStream.pop().getProperties());
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
