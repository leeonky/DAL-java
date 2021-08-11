package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.ast.RegexNode;
import com.github.leeonky.dal.token.Token;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;

import static com.github.leeonky.dal.compiler.NodeFactory.createConstNodeFactory;
import static com.github.leeonky.dal.compiler.NodeFactory.createPropertyNodeFactory;
import static java.util.Arrays.asList;

public interface NodeFactory {

    static NodeFactory createConstNodeFactory() {
        return new SingleTokenNodeFactory(Token.Type.CONST_VALUE, ConstNode::new);
    }

    static NodeFactory createEvaluableNodeFactory() {
        return new EvaluableNodeFactory();
    }

    static NodeFactory createRegexNodeFactory() {
        return new SingleTokenNodeFactory(Token.Type.REGEX, regex -> new RegexNode((String) regex));
    }

    static NodeFactory createPropertyNodeFactory() {
        return nodeParser -> {
            if (nodeParser.tokenStream.currentType() == Token.Type.PROPERTY) {
                Token token = nodeParser.tokenStream.pop();
                return new PropertyNode(nodeParser.getThisNode(), token.getValue())
                        .setPositionBegin(token.getPositionBegin());
            }
            return null;
        };
    }

    Node fetchNode(NodeParser nodeParser);
}

class EvaluableNodeFactory implements NodeFactory {
    private final NodeFactory propertyNodeFactory = createPropertyNodeFactory();
    private final List<NodeFactory> nodeFactories = asList(
            createConstNodeFactory(),
            propertyNodeFactory
    );

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        Node node = nodeFactories.stream()
                .map(factory -> factory.fetchNode(nodeParser))
                .filter(Objects::nonNull).findFirst().orElse(null);
        while (nodeParser.setThis(node) != null && nodeParser.tokenStream.hasTokens())
            node = propertyNodeFactory.fetchNode(nodeParser);
        return node;
    }
}

class SingleTokenNodeFactory implements NodeFactory {
    private final Token.Type tokenType;
    private final Function<Object, Node> creator;

    public SingleTokenNodeFactory(Token.Type constValue, Function<Object, Node> creator) {
        tokenType = constValue;
        this.creator = creator;
    }

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        if (nodeParser.tokenStream.currentType() == tokenType) {
            Token token = nodeParser.tokenStream.pop();
            return creator.apply(token.getValue()).setPositionBegin(token.getPositionBegin());
        }
        return null;
    }
}
