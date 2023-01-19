template nimyaccAssert*(cond: untyped, msg:string = "") = 
    when defined(nimydevel):
        doAssert cond, msg
