type LexerState* = object
    startPos*: int # needed for lexim  
    endPosExcl*: int # needed for lexim
    strBody*: string
    commentDepth*: int

type UserActionState* = object 
    dummy*: int