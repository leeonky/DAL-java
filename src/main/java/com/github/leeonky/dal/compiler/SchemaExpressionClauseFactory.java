package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.SchemaExpression;
import com.github.leeonky.dal.ast.SchemaNode;

import java.util.List;

public class SchemaExpressionClauseFactory implements ExpressionClauseFactory<DALNode> {
    public static final NodeFactory<DALNode> SCHEMA;

    static {
        SCHEMA = TokenParser.SCHEMA.map(token -> new SchemaNode(token.getContent()));
    }

    @Override
    public ExpressionClause<DALNode> fetch(TokenParser<DALNode> tokenParser) {
        return fetchSchemaClause(tokenParser, 0);
    }

    private ExpressionClause<DALNode> fetchSchemaClause(TokenParser<DALNode> tokenParser, int dimension) {
        if (dimension > 1)
            throw tokenParser.getSourceCode().syntaxError("Not support multidimensional schema", 0);
        return tokenParser.fetchBetween("[", "]", () -> fetchSchemaClause(tokenParser, dimension + 1)).orElseGet(() -> {
            List<SchemaNode> schemaNodes = tokenParser.fetchNodes("/", SCHEMA);
            return input -> new SchemaExpression(input, schemaNodes, dimension);
        });
    }
}
