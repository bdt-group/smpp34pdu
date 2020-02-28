-module(smpp34pdu_submit_sm_resp).
-include("smpp34pdu.hrl").
-include("smpp34pdu_types.hrl").
-include("smpp34pdu_tlv_macros.hrl").
-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_integer/2]).

-spec pack(submit_sm_resp()) -> binary().
-spec unpack(binary()) -> submit_sm_resp().

pack(#submit_sm_resp{message_id=MessageId,
                     ussd_session_id=UssdSessionId}) ->
    L = [cstring_to_bin(MessageId, 65),
         tlv:pack(?USSD_SESSION_ID, UssdSessionId)],
    list_to_binary(L).


unpack(Bin0) ->
    {MessageId, Bin1} = bin_to_cstring(Bin0, 65),
    unpack_tlv_fields(Bin1, #submit_sm_resp{message_id=MessageId}).

?TLV_UNPACK_EMPTY_BIN();
?TLV_UNPACK_FIELD(submit_sm_resp, ussd_session_id, ?USSD_SESSION_ID);
?TLV_UNPACK_UNEXPECTED().
