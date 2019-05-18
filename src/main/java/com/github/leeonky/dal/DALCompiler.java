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
