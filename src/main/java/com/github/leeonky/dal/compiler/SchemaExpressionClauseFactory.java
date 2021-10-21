package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.SchemaExpression;
import com.github.leeonky.dal.ast.SchemaNode;

import java.util.List;

public class SchemaExpressionClauseFactory implements ExpressionClauseFactory {
    public static final NodeFactory SCHEMA = TokenParser.SCHEMA.map(token -> new SchemaNode(token.getContent()));

    @Override
    public ExpressionClause fetch(TokenParser tokenParser) {
        List<SchemaNode> schemaNodes = tokenParser.fetchNodes("/", SCHEMA);
        return input -> {
//            Optional<List<Node>> nodes = tokenParser.fetchOne('[', ']', () -> tokenParser.fetchNodes("/", SCHEMA));
//            if (nodes.isPresent())
//                return new SchemaExpression(input, tokenParser.fetchNodes("/", SCHEMA), true);
            return new SchemaExpression(input, schemaNodes, false);
        };
    }
}
