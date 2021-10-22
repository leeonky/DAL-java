package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.SchemaExpression;
import com.github.leeonky.dal.ast.SchemaNode;

import java.util.List;

public class SchemaExpressionClauseFactory implements ExpressionClauseFactory {
    public static final NodeFactory SCHEMA = TokenParser.SCHEMA.map(token -> new SchemaNode(token.getContent()));

    @Override
    public ExpressionClause fetch(TokenParser tokenParser) {
        return tokenParser.<List<SchemaNode>>fetchOne('[', ']', () -> tokenParser.fetchNodes("/", SCHEMA))
                .<ExpressionClause>map(schemaNodes -> input -> new SchemaExpression(input, schemaNodes, true))
                .orElseGet(() -> {
                    List<SchemaNode> schemaNodes = tokenParser.fetchNodes("/", SCHEMA);
                    return input -> new SchemaExpression(input, schemaNodes, false);
                });
    }
}
