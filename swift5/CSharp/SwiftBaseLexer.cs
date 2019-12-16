using Antlr4.Runtime;

public abstract class SwiftBaseLexer : Lexer
{
    private bool _insideString;
    private int lParenCounter;
    protected SwiftBaseLexer(ICharStream input) : base(input)
    {
    }

    protected void CheckIfInsideInterpolatedExpression()
    {
        if (_insideString)
        {
            lParenCounter++;
        }
    }

    protected void PopModeOnInterpolatedExpressionClose()
    {
        if (_insideString && lParenCounter == 0)
        {
            _insideString = false;
            Channel = Hidden;
            PopMode();
        }

        if (lParenCounter > 0)
        {
            lParenCounter--;
        }
    }

    protected void SetInsideString()
    {
        _insideString = true;
        lParenCounter = 0;
    }
    
}