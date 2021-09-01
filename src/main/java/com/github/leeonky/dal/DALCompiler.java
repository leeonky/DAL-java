package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.NodeFactory;
import com.github.leeonky.dal.ast.NodeParser;
import com.github.leeonky.dal.parser.TokenParser;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.TokenStream;

import static com.github.leeonky.dal.token.TokenFactory.createDALTokenFactory;

public class DALCompiler {
    public Node compile(SourceCode sourceCode) {
        TokenStream tokenStream = createDALTokenFactory().fetchToken(new TokenParser(sourceCode)).getTokenStream();
        return NodeFactory.EXPRESSION.fetchNode(new NodeParser(tokenStream));
    }
}
