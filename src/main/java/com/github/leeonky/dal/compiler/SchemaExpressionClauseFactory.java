package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.SchemaExpression;
import com.github.leeonky.dal.ast.SchemaNode;

import java.util.List;
import java.util.Optional;

public class SchemaExpressionClauseFactory implements ExpressionClauseFactory {
    public static final NodeFactory SCHEMA = TokenParser.SCHEMA.map(token -> new SchemaNode(token.getContent()));

    @Override
    public ExpressionClause fetch(TokenParser tokenParser) {
//        TODO refactor
        Optional<List<SchemaNode>> nodes = tokenParser.fetchOne('[', ']', () -> tokenParser.fetchNodes("/", SCHEMA));
        if (nodes.isPresent()) {
            return input -> {
                return new SchemaExpression(input, nodes.get(), true);
            };
        }
        List<SchemaNode> schemaNodes = tokenParser.fetchNodes("/", SCHEMA);
        return input -> {
            return new SchemaExpression(input, schemaNodes, false);
        };
    }
}
