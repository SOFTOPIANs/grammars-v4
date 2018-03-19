using Antlr4.Runtime;
using static PT.PM.JavaScriptParseTreeUst.Parser.JavaScriptParser;

/// <summary>
/// All parser methods that used in grammar (p, prev, notLineTerminator, etc.)
/// should start with lower case char similar to parser rules.
/// </summary>
public abstract class JavaScriptBaseParser : Parser
{
    public JavaScriptBaseParser(ITokenStream input)
        : base(input)
    {
    }

#if ANTLR_STANDARD
    public JavaScriptBaseParser(ITokenStream input, System.IO.TextWriter output, System.IO.TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }
#endif

    /// <summary>
    /// hort form for prev(String str)
    /// </summary>
    protected bool p(string str)
    {
        return prev(str);
    }

    /// <summary>
    /// Whether the previous token value equals to str
    /// </summary>
    protected bool prev(string str)
    {
        return Lt(-1).Text.Equals(str);
    }

    protected bool notLineTerminator()
    {
        return !here(LineTerminator);
    }

    protected bool notOpenBraceAndNotFunction()
    {
        int nextTokenType = Lt(1).Type;
        return nextTokenType != OpenBrace && nextTokenType != Function;
    }

    protected bool closeBrace()
    {
        return Lt(1).Type == CloseBrace;
    }

    /// <summary>Returns true if on the current index of the parser's
    /// token stream a token of the given type exists on the
    /// Hidden channel.
    /// </summary>
    /// <param name="type">
    /// The type of the token on the Hidden channel to check.
    /// </param>
    protected bool here(int type)
    {
        // Get the token ahead of the current index.
        int possibleIndexEosToken = CurrentToken.TokenIndex - 1;
        IToken ahead = GetToken(possibleIndexEosToken);

        // Check if the token resides on the Hidden channel and if it's of the
        // provided type.
        return ahead.Channel == Lexer.Hidden && ahead.Type == type;
    }

    /// <summary>
    /// Returns true if on the current index of the parser's
    /// token stream a token exists on the Hidden channel which
    /// either is a line terminator, or is a multi line comment that
    /// contains a line terminator.
    /// </summary>
    protected bool lineTerminatorAhead()
    {
        // Get the token ahead of the current index.
        int possibleIndexEosToken = CurrentToken.TokenIndex - 1;
        IToken ahead = GetToken(possibleIndexEosToken);

        if (ahead.Channel != Lexer.Hidden)
        {
            // We're only interested in tokens on the Hidden channel.
            return false;
        }

        if (ahead.Type == LineTerminator)
        {
            // There is definitely a line terminator ahead.
            return true;
        }

        if (ahead.Type == WhiteSpaces)
        {
            // Get the token ahead of the current whitespaces.
            possibleIndexEosToken = CurrentToken.TokenIndex - 2;
            ahead = GetToken(possibleIndexEosToken);
        }

        // Get the token's text and type.
        string text = ahead.Text;
        int type = ahead.Type;

        // Check if the token is, or contains a line terminator.
        return (type == MultiLineComment && (text.Contains("\r") || text.Contains("\n"))) ||
                (type == LineTerminator);
    }

    private IToken Lt(int pos)
    {
#if ANTLR_STANDARD
        return TokenStream.LT(pos);
#else
        return _input.Lt(pos);
#endif
    }

    private IToken GetToken(int pos)
    {
#if ANTLR_STANDARD
        return TokenStream.Get(pos);
#else
        return _input.Get(pos);
#endif
    }
}