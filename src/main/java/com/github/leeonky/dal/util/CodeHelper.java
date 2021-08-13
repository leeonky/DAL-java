package com.github.leeonky.dal.util;

import com.github.leeonky.dal.parser.TokenParser;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;

import java.util.List;
import java.util.stream.Collectors;

public class CodeHelper {
    public static final CodeHelper INSTANCE = new CodeHelper();

    private TokenFactory chainNodeTokenFactory = TokenFactory.createPropertyChainFactory();

    private CodeHelper() {
    }

    public List<Object> toChainNodes(String s) {
        s = s.trim();
        if (!s.startsWith("["))
            s = "." + s;
        return chainNodeTokenFactory.fetchToken(new TokenParser(new SourceCode(s))).getTokenStream().tokens()
                .map(Token::getValue).collect(Collectors.toList());
    }
}
