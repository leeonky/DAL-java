package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.opt.*;

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

    public static boolean isSpliter(String content, int position) {
        char c = content.charAt(position);
        try {
            return Character.isWhitespace(c);
        } catch (Exception e) {
            return false;
        }
    }

    public Node compile(String content) {
        final String trim = content.trim();
        return operatorList.stream()
                .filter(opt -> opt.isMatch(trim))
                .map(operator -> new Expression(new InputNode(), compileNode(trim.substring(operator.length())), operator))
                .findFirst().get();
    }

    private ConstNode compileNode(String content) {
        return new ConstNode(Integer.valueOf(content.trim()));
    }
}
