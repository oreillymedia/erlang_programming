-module(dp).
-compile(export_all).

process_msg() ->
    case ets:first(msgQ) of
	'$end_of_table' ->
	    ok;
	Key ->
	    case ets:lookup(msgQ, Key) of
		[{_, {event, Sender, Msg}}] ->
		    event(Sender, Msg);
		[{_, {ping, Sender}}] ->
		    ping(Sender)
	    end,
	    ets:delete(msgQ, Key),
	    Key
    end.

event(_,_) -> ok.
ping(_) -> ok.

fill() ->
    catch ets:new(msgQ, [named_table, ordered_set]),
    dp:handle_msg(<<2,3,0,2,0>>).
