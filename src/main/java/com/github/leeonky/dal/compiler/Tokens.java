package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.SourceCode;
import com.github.leeonky.interpreter.Token;
import com.github.leeonky.interpreter.TokenScanner;

import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.interpreter.FunctionUtil.not;
import static com.github.leeonky.interpreter.SourceCode.tokenScanner;
import static java.util.Collections.emptySet;

public class Tokens {
    public static final TokenScanner<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            NUMBER = tokenScanner(DIGITAL::contains, emptySet(), false, Tokens::supportDecimalWithPower, Token::isNumber),
            INTEGER = SourceCode.tokenScanner(DIGITAL_OR_MINUS::contains, emptySet(), false, DELIMITER, Token::isNumber),
            SYMBOL = SourceCode.tokenScanner(not(DELIMITER::contains), ALL_KEY_WORDS, false, DELIMITER_OR_DOT, not(Token::isNumber)),
            DOT_SYMBOL = SourceCode.tokenScanner(not(DELIMITER::contains), emptySet(), false, DELIMITER_OR_DOT, not(Token::isNumber)),
            SCHEMA = SourceCode.tokenScanner(not(DELIMITER::contains), ALL_KEY_WORDS, false, DELIMITER, not(Token::isNumber));

    private static boolean supportDecimalWithPower(Character lastChar, Character nextChar) {
        return ((lastChar != 'e' && lastChar != 'E') || (nextChar != '-' && nextChar != '+')) && DELIMITER.contains(nextChar);
    }

    public static final TokenScanner.Mandatory SCHEMA_BK = SourceCode.tokenScanner(not(DELIMITER::contains), ALL_KEY_WORDS,
            false, DELIMITER, not(Token::isNumber)).mandatory("expect a schema");

}
