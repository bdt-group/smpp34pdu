-module(smpp34pdu_deliver_sm_resp_tests).
-include("../include/smpp34pdu.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(deliver_sm_resp_custom_tlv, {message_id=?DEFAULT_CSTRING, custom_field=?DEFAULT_CSTRING}).

deliver_sm_resp_test_() ->
    Payload = #deliver_sm_resp{message_id="abcdefghij"},

    Bin = <<97,98,99,100,101,102,103,104,105,106,0>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_deliver_sm_resp:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_deliver_sm_resp:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload",
			?_assertEqual(Payload,
					smpp34pdu_deliver_sm_resp:unpack(smpp34pdu_deliver_sm_resp:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin",
			?_assertEqual(Bin,
					smpp34pdu_deliver_sm_resp:pack(smpp34pdu_deliver_sm_resp:unpack(Bin)))}
	].

deliver_sm_resp_custom_tlv_test_() ->
    Payload0 = #deliver_sm_resp_custom_tlv{message_id="abcdefghij", custom_field = "212312"},
    Payload = #deliver_sm_resp{message_id="abcdefghij"},
    Bin = pack(Payload0),

    [
		{"Unpacking Bin with custom TLV will give Payload without custom TLV",
            ?_assertEqual(Payload, smpp34pdu_deliver_sm_resp:unpack(Bin))}
    ].

pack(#deliver_sm_resp_custom_tlv{message_id=MessageId, custom_field = CustomValue}) ->
	L = [pdu_data:cstring_to_bin(MessageId, 65), pdu_data:cstring_to_bin(CustomValue, 65)],
	list_to_binary(L).
