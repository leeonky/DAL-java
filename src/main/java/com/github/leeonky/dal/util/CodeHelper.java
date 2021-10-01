package com.github.leeonky.dal.util;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.compiler.SourceCode;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static com.github.leeonky.dal.compiler.NodeParser.PROPERTY;

public class CodeHelper {
    public static List<Object> toChainNodes(String s) {
        SourceCode sourceCode = new SourceCode(s);
        Optional<Node> fetch = PROPERTY.fetch(sourceCode);
        List<Object> result = new ArrayList<>();
        while (fetch.isPresent()) {
            result.add(((PropertyNode) fetch.get()).getName());
            fetch = PROPERTY.fetch(sourceCode);
        }
        return result;
    }
}
