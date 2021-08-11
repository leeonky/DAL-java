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
import static com.github.leeonky.dal.compiler.SingleTokenNodeFactory.singleTokenNodeFactory;
import static java.util.Arrays.asList;

public interface NodeFactory {

    static NodeFactory createConstNodeFactory() {
        return singleTokenNodeFactory(Token.Type.CONST_VALUE, ConstNode::new);
    }

    static NodeFactory createEvaluableNodeFactory() {
        return new SingleEvaluableNodeFactory();
    }

    static NodeFactory createRegexNodeFactory() {
        return singleTokenNodeFactory(Token.Type.REGEX, regex -> new RegexNode((String) regex));
    }

    static NodeFactory createPropertyNodeFactory() {
        return new SingleTokenNodeFactory(Token.Type.PROPERTY) {
            @Override
            protected Node createNode(NodeParser nodeParser, Object value) {
                return new PropertyNode(nodeParser.getThisNode(), value);
            }
        };
    }

    Node fetchNode(NodeParser nodeParser);
}

class SingleEvaluableNodeFactory implements NodeFactory {
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

abstract class SingleTokenNodeFactory implements NodeFactory {
    private final Token.Type tokenType;

    public SingleTokenNodeFactory(Token.Type constValue) {
        tokenType = constValue;
    }

    public static SingleTokenNodeFactory singleTokenNodeFactory(Token.Type type, Function<Object, Node> creator) {
        return new SingleTokenNodeFactory(type) {
            @Override
            protected Node createNode(NodeParser nodeParser, Object value) {
                return creator.apply(value);
            }
        };
    }

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        if (nodeParser.tokenStream.currentType() == tokenType) {
            Token token = nodeParser.tokenStream.pop();
            return createNode(nodeParser, token.getValue()).setPositionBegin(token.getPositionBegin());
        }
        return null;
    }

    protected abstract Node createNode(NodeParser nodeParser, Object value);
}
