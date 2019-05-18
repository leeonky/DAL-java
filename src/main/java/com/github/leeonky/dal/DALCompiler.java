package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.NotEqual;
import com.github.leeonky.dal.ast.opt.Operator;

import java.util.List;

import static java.util.Arrays.asList;

public class DALCompiler {
    private final List<Operator> operatorList = asList(new Equal(), new NotEqual());

    public Node compile(String expressionContent) {
        final String trim = expressionContent.trim();
        return operatorList.stream()
                .filter(opt -> opt.isMatch(trim))
                .map(operator -> new Expression(new InputNode(),
                        new ConstNode(Integer.valueOf(trim.substring(operator.length()).trim())), operator))
                .findFirst().get();
    }
}
