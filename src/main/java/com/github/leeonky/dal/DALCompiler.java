package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.opt.*;
import com.github.leeonky.dal.token.SourceCode;

import java.util.Comparator;
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
}
