% This file contain 4 macro used to delimit
% crashable thread from uncrashable ones.
%

% Used in the code, when a state is changed,
% it should be surrounded with this two macro
-define( TRANSACTIONBEGIN, try ).
-define( TRANSACTIONEND,
        catch
            throw:{'FATAL_IRC_TRANSAC', C, Ex} -> throw( {'FATAL_IRC_TRANSAC', C, Ex} );
            throw:Ex -> throw( {'FATAL_IRC_TRANSAC',   'THROW', Ex} );
             exit:Ex -> throw( {'FATAL_IRC_TRANSAC',    'EXIT', Ex} );
            error:Ex -> throw( {'FATAL_IRC_TRANSAC',   'ERROR', Ex} )
         end ).

% Using in father thread in case of crash.
%
-define( TRANSACTION_SENTINEL_BEGIN, try ).
-define( TRANSACTION_SENTINEL_END(Returned),
        catch
            {'FATAL_IRC_TRANSAC', C, Ex} -> Txt = "FATAL \n" ++ io_lib:write({C, Ex}),
                                            irc_log:logFatal( Txt ),
                                            halt();
            Reason -> Errmsg = "Error (YOU must debug) " ++ io_lib:write( Reason ),
                      irc_log:logError( Errmsg ),
                      Returned
        end ).
