-module(smpp34pdu_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


pack_test_() ->
	[
		{"Packing #generic_nack{} PDU",
			?_assertEqual(<<0,0,0,16,128,0,0,0,0,0,0,0,0,0,0,4>>,
					   smpp34pdu:pack(4, #generic_nack{}))},
		{"Packing #bind_receiver{} PDU",
			?_assertEqual(<<0,0,0,37,0,0,0,1,0,0,0,0,0,0,0,1,
					   97,98,99,100,101,102,103,104,105,
					   106,0,97,98,99,100,0,0,52,2,1,0>>,
					   smpp34pdu:pack(1, #bind_receiver{system_id="abcdefghij", 
						   password="abcd", system_type="", 
						   interface_version=?VERSION, addr_ton=2, 
						   addr_npi=1,address_range=""}))},
		{"Packing #bind_transmitter{} PDU",
			?_assertEqual(<<0,0,0,37,0,0,0,2,0,0,0,0,0,0,0,1,
					   97,98,99,100,101,102,103,104,105,
					   106,0,97,98,99,100,0,0,52,2,1,0>>,
					   smpp34pdu:pack(1, #bind_transmitter{system_id="abcdefghij", 
						   password="abcd", system_type="", 
						   interface_version=?VERSION, addr_ton=2, 
						   addr_npi=1,address_range=""}))},
		{"Packing #bind_transmitter_resp{} PDU",
			?_assertEqual(<<0,0,0,32,128,0,0,2,0,0,0,0,0,0,0,1,
					   97,98,99,100,101,102,103,104,105,
					   106,0,2,16,0,1,52>>,
					   smpp34pdu:pack(1, #bind_transmitter_resp{system_id="abcdefghij", 
						   sc_interface_version=?VERSION}))},
		{"Packing #query_sm{} PDU",
			?_assertEqual(<<0,0,0,34,0,0,0,3,0,0,0,0,0,0,0,1,
					   97,98,99,100,101,102,103,104,105,
					   106,0,2,1,97,98,99,100,0>>,
					   smpp34pdu:pack(1, #query_sm{message_id="abcdefghij",
						   source_addr_ton=2, source_addr_npi=1,
						   source_addr="abcd"}))},
		{"Packing #query_sm_resp{} PDU",
			?_assertEqual(<<0,0,0,46,128,0,0,3,0,0,0,0,0,0,0,1,
					     97,98,99,100,101,102,103,104,105,
					     106,0,49,48,48,55,48,50,49,49,53,53,
						 48,48,48,48,48,43,0,3,5>>,
					   smpp34pdu:pack(1,#query_sm_resp{message_id="abcdefghij", 
						   final_date="100702115500000+", message_state=3, 
						   error_code=5}))},
		{"Packing #submit_sm_resp{} PDU",
			?_assertEqual(<<0,0,0,27,128,0,0,4,0,0,0,0,0,0,0,1,
					   97,98,99,100,101,102,103,104,105,
					   106,0>>,
					   smpp34pdu:pack(1, #submit_sm_resp{message_id="abcdefghij"}))},
		{"Packing #deliver_sm_resp{} PDU",
			?_assertEqual(<<0,0,0,27,128,0,0,5,0,0,0,0,0,0,0,1,
					   97,98,99,100,101,102,103,104,105,
					   106,0>>,
					   smpp34pdu:pack(1, #deliver_sm_resp{message_id="abcdefghij"}))},
		{"Packing #unbind_resp{} PDU",
			?_assertEqual(<<0,0,0,16,128,0,0,6,0,0,0,0,0,0,0,4>>,
					   smpp34pdu:pack(4, #unbind_resp{}))},
		{"Packing #replace_sm{} PDU",
			?_assertEqual(<<0,0,0,82,0,0,0,7,0,0,0,0,0,0,0,4,
					        97,98,99,100,101,102,103,104,105,
							106,0,2,1,97,98,99,100,0,$1,$0,$0,
							$7,$1,$6,$1,$3,$3,$0,$5,$9,$0,$0,$1,
							$+,0,$0,$0,$0,$0,$1,$4,$0,$0,$0,$0,
							$0,$0,$0,$0,$0,$R,0,17,1,11,104,101,
							108,108,111,32,119,111,114,108,100>>,
					   smpp34pdu:pack(4,#replace_sm{message_id="abcdefghij", 
						   source_addr_ton=2, source_addr_npi=1, source_addr="abcd", 
						   schedule_delivery_time="100716133059001+", validity_period="000014000000000R", 
						   registered_delivery=?DELIVERY_SMSC_ANY bor ?DELIVERY_SME_NONE bor ?DELIVERY_INTM_ANY, 
						   sm_default_msg_id=1, sm_length=11, short_message="hello world"}))},
		{"Packing #replace_sm_resp{} PDU",
			?_assertEqual(<<0,0,0,16,128,0,0,7,0,0,0,0,0,0,0,4>>,
					   smpp34pdu:pack(4, #replace_sm_resp{}))},
		{"Packing #cancel_sm{} PDU",
			?_assertEqual(<<0,0,0,45,0,0,0,8,0,0,0,0,0,0,0,4,
					   87,65,80,0,97,98,99,100,101,102,103,104,
					   105,106,0,2,1,97,98,99,100,0,2,1,101,102,
					   103,104,0>>,
					   smpp34pdu:pack(4, #cancel_sm{service_type="WAP", 
						   message_id="abcdefghij", 
						   source_addr_ton=2, 
						   source_addr_npi=1, 
						   source_addr="abcd", 
						   dest_addr_ton=2, 
						   dest_addr_npi=1, 
						   destination_addr="efgh"}))},
		{"Packing #cancel_sm_resp{} PDU",
			?_assertEqual(<<0,0,0,16,128,0,0,8,0,0,0,0,0,0,0,4>>,
					   smpp34pdu:pack(4, #cancel_sm_resp{}))},
		{"Packing #bind_transceiver{} PDU",
			?_assertEqual(<<0,0,0,37,0,0,0,9,0,0,0,0,0,0,0,1,
					   97,98,99,100,101,102,103,104,105,
					   106,0,97,98,99,100,0,0,52,2,1,0>>,
					   smpp34pdu:pack(1, #bind_transceiver{system_id="abcdefghij", 
						   password="abcd", system_type="", 
						   interface_version=?VERSION, addr_ton=2, 
						   addr_npi=1,address_range=""}))},
		{"Packing #outbind{} PDU",
			?_assertEqual(<<0,0,0,32,0,0,0,11,0,0,0,0,0,0,0,1,
					   97,98,99,100,101,102,103,104,105,
					   106,0,97,98,99,100,0>>,
					   smpp34pdu:pack(1, #outbind{system_id="abcdefghij", 
						   password="abcd"}))},
		{"Packing #enquire_link{} PDU",
			?_assertEqual(<<0,0,0,16,0,0,0,21,0,0,0,0,0,0,0,4>>,
					   smpp34pdu:pack(4, #enquire_link{}))},
		{"Packing #enquire_link_resp{} PDU",
			?_assertEqual(<<0,0,0,16,128,0,0,21,0,0,0,0,0,0,0,4>>,
					   smpp34pdu:pack(4, #enquire_link_resp{}))}
	].

unpack_test_() ->
	[
		{"Unpacking #generic_nack{} PDU",
			?_assertEqual({ok, [#pdu{command_length=16, 
					command_id=?GENERIC_NACK, command_status=0, 
					sequence_number=7, body=#generic_nack{}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,16,128,0,0,0,0,0,0,0,0,0,0,7>>))},
		{"Unpacking #bind_receiver{} PDU",
			?_assertEqual({ok, [#pdu{command_length=37, 
					command_id=?BIND_RECEIVER, command_status=0, 
					sequence_number=1, body=#bind_receiver{system_id="abcdefghij", 
								password="abcd", system_type="", interface_version=?VERSION, 
								addr_ton=2, addr_npi=1,address_range=""}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,37,0,0,0,1,0,0,0,0,0,0,0,
													1,97,98,99,100,101,102,103,104,
													105,106,0,97,98,99,100,0,0,52,2,
													1,0>>))},
		{"Unpacking #bind_transmitter{} PDU",
			?_assertEqual({ok, [#pdu{command_length=37, 
					command_id=?BIND_TRANSMITTER, command_status=0, 
					sequence_number=1, body=#bind_transmitter{system_id="abcdefghij", 
								password="abcd", system_type="", interface_version=?VERSION, 
								addr_ton=2, addr_npi=1,address_range=""}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,37,0,0,0,2,0,0,0,0,0,0,0,
													1,97,98,99,100,101,102,103,104,
													105,106,0,97,98,99,100,0,0,52,2,
													1,0>>))},
		{"Unpacking #bind_transmitter_resp{} PDU",
			?_assertEqual({ok, [#pdu{command_length=32, 
					command_id=?BIND_TRANSMITTER_RESP, command_status=0, 
					sequence_number=1, body=#bind_transmitter_resp{system_id="abcdefghij", 
								sc_interface_version=?VERSION}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,32,128,0,0,2,0,0,0,0,0,0,0,
													1,97,98,99,100,101,102,103,104,
													105,106,0,2,16,0,1,52>>))},
		{"Unpacking #query_sm{} PDU",
			?_assertEqual({ok, [#pdu{command_length=34, 
					command_id=?QUERY_SM, command_status=0, 
					sequence_number=1, body=#query_sm{message_id="abcdefghij",
					source_addr_ton=2, source_addr_npi=1, source_addr="abcd"}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,34,0,0,0,3,0,0,0,0,0,0,0,
													1,97,98,99,100,101,102,103,104,
													105,106,0,2,1,97,98,99,100,0>>))},
		{"Unpacking #query_sm_resp{} PDU",
			?_assertEqual({ok, [#pdu{command_length=46, 
					command_id=?QUERY_SM_RESP, command_status=0, 
					sequence_number=1, body=#query_sm_resp{message_id="abcdefghij", 
					final_date="100702115500000+", message_state=3,error_code=5}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,46,128,0,0,3,0,0,0,0,0,0,0,1, 
											       97,98,99,100,101,102,103,104,105, 
												   106,0,49,48,48,55,48,50,49,49,53,53, 
												   48,48,48,48,48,43,0,3,5>>))},
		{"Unpacking #submit_sm_resp{} PDU",
			?_assertEqual({ok, [#pdu{command_length=27, 
					command_id=?SUBMIT_SM_RESP, command_status=0, 
					sequence_number=1, body=#submit_sm_resp{message_id="abcdefghij"}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,27,128,0,0,4,0,0,0,0,0,0,0,
													1,97,98,99,100,101,102,103,104,
													105,106,0>>))},
		{"Unpacking #deliver_sm_resp{} PDU",
			?_assertEqual({ok, [#pdu{command_length=27, 
					command_id=?DELIVER_SM_RESP, command_status=0, 
					sequence_number=1, body=#deliver_sm_resp{message_id="abcdefghij"}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,27,128,0,0,5,0,0,0,0,0,0,0,
													1,97,98,99,100,101,102,103,104,
													105,106,0>>))},
		{"Unpacking #unbind{} PDU",
			?_assertEqual({ok, [#pdu{command_length=16, 
					command_id=?UNBIND, command_status=0, 
					sequence_number=7, body=#unbind{}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,16,0,0,0,6,0,0,0,0,0,0,0,7>>))},
		{"Unpacking #unbind_resp{} PDU",
			?_assertEqual({ok, [#pdu{command_length=16, 
					command_id=?UNBIND_RESP, command_status=0, 
					sequence_number=7, body=#unbind_resp{}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,16,128,0,0,6,0,0,0,0,0,0,0,7>>))},
		{"Unpacking #replace_sm{} PDU",
			?_assertEqual({ok, [#pdu{command_length=82, 
					command_id=?REPLACE_SM, command_status=0, 
					sequence_number=4, body=#replace_sm{message_id="abcdefghij", 
					source_addr_ton=2, source_addr_npi=1, source_addr="abcd", 
					schedule_delivery_time="100716133059001+", validity_period="000014000000000R", 
					registered_delivery=?DELIVERY_SMSC_ANY bor ?DELIVERY_SME_NONE bor ?DELIVERY_INTM_ANY, 
					sm_default_msg_id=1, sm_length=11, short_message="hello world"}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,82,0,0,0,7,0,0,0,0,0,0,0,4,
											       97,98,99,100,101,102,103,104,105,
												   106,0,2,1,97,98,99,100,0,$1,$0,$0,
												   $7,$1,$6,$1,$3,$3,$0,$5,$9,$0,$0,$1,
												   $+,0,$0,$0,$0,$0,$1,$4,$0,$0,$0,$0,
												   $0,$0,$0,$0,$0,$R,0,17,1,11,104,101,
												   108,108,111,32,119,111,114,108,100>>))},
		{"Unpacking #replace_sm_resp{} PDU",
			?_assertEqual({ok, [#pdu{command_length=16, 
					command_id=?REPLACE_SM_RESP, command_status=0, 
					sequence_number=7, body=#replace_sm_resp{}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,16,128,0,0,7,0,0,0,0,0,0,0,7>>))},
		{"Unpacking #cancel_sm{} PDU",
			?_assertEqual({ok, [#pdu{command_length=45, 
					command_id=?CANCEL_SM, command_status=0, 
					sequence_number=4, body=#cancel_sm{service_type="WAP", 
					message_id="abcdefghij", source_addr_ton=2, source_addr_npi=1, 
					source_addr="abcd", dest_addr_ton=2, dest_addr_npi=1, 
					destination_addr="efgh"}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,45,0,0,0,8,0,0,0,0,0,0,0,
											       4,87,65,80,0,97,98,99,100,101,
												   102,103,104,105,106,0,2,1,97,98,
												   99,100,0,2,1,101,102,103,104,0>>))},
		{"Unpacking #cancel_sm_resp{} PDU",
			?_assertEqual({ok, [#pdu{command_length=16, 
					command_id=?CANCEL_SM_RESP, command_status=0, 
					sequence_number=7, body=#cancel_sm_resp{}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,16,128,0,0,8,0,0,0,0,0,0,0,7>>))},
		{"Unpacking #bind_transceiver{} PDU",
			?_assertEqual({ok, [#pdu{command_length=37, 
					command_id=?BIND_TRANSCEIVER, command_status=0, 
					sequence_number=1, body=#bind_transceiver{system_id="abcdefghij", 
								password="abcd", system_type="", interface_version=?VERSION, 
								addr_ton=2, addr_npi=1,address_range=""}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,37,0,0,0,9,0,0,0,0,0,0,0,
													1,97,98,99,100,101,102,103,104,
													105,106,0,97,98,99,100,0,0,52,2,
													1,0>>))},
		{"Unpacking #outbind{} PDU",
			?_assertEqual({ok, [#pdu{command_length=32, 
					command_id=?OUTBIND, command_status=0, 
					sequence_number=1, body=#outbind{system_id="abcdefghij", 
								password="abcd"}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,32,0,0,0,11,0,0,0,0,0,0,0,
													1,97,98,99,100,101,102,103,104,
													105,106,0,97,98,99,100,0>>))},
		{"Unpacking #enquire_link{} PDU",
			?_assertEqual({ok, [#pdu{command_length=16, 
					command_id=?ENQUIRE_LINK, command_status=0, 
					sequence_number=7, body=#enquire_link{}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,16,0,0,0,21,0,0,0,0,0,0,0,7>>))},
		{"Unpacking #enquire_link_resp{} PDU",
			?_assertEqual({ok, [#pdu{command_length=16, 
					command_id=?ENQUIRE_LINK_RESP, command_status=0, 
					sequence_number=7, body=#enquire_link_resp{}}], <<>>}, 
								smpp34pdu:unpack(<<0,0,0,16,128,0,0,21,0,0,0,0,0,0,0,7>>))}
	].

unpack_multiple_test_() ->
	[
		{"Unpack PDUs WITH NO remainder",
			?_assertEqual({ok, [#pdu{command_length=37, command_id=?BIND_RECEIVER,
						command_status=0, sequence_number=1,
						body=#bind_receiver{system_id="abcdefghij", 
						password="abcd", system_type="", 
						interface_version=?VERSION, addr_ton=2, 
						addr_npi=1,address_range=""}},
					    #pdu{command_length=16, command_id=?UNBIND,
						command_status=0, sequence_number=2,
						body=#unbind{}},#pdu{command_length=37, 
						command_id=?BIND_TRANSCEIVER,
						command_status=0, sequence_number=3,
						body=#bind_transceiver{system_id="abcdefghij", 
						password="abcd", system_type="", 
						interface_version=?VERSION, addr_ton=2, 
						addr_npi=1,address_range=""}}], <<>>},
						smpp34pdu:unpack(<<0,0,0,37,0,0,0,1,0,0,0,0,0,
								  0,0,1,97,98,99,100,101,102,
								  103,104,105,106,0,97,98,99,
								  100,0,0,52,2,1,0,0,0,0,16,0,
								  0,0,6,0,0,0,0,0,0,0,2,0,0,0,
								  37,0,0,0,9,0,0,0,0,0,0,0,3,
								  97,98,99,100,101,102,103,104,
								  105,106,0,97,98,99,100,0,0,
								  52,2,1,0>>))},
		{"Unpack PDUs WITH incomplete header",
			?_assertEqual({header_length, [#pdu{command_length=16, command_id=?UNBIND_RESP,
						command_status=0, sequence_number=1,
						body=#unbind_resp{}},
					    #pdu{command_length=37, command_id=?BIND_TRANSCEIVER,
						command_status=0, sequence_number=2,
						body=#bind_transceiver{system_id="abcdefghij", 
						password="abcd", system_type="", 
						interface_version=?VERSION, addr_ton=2, 
						addr_npi=1,address_range=""}},
					    #pdu{command_length=37, command_id=?BIND_TRANSMITTER,
						command_status=0, sequence_number=3,
						body=#bind_transmitter{system_id="abcdefghij", 
						password="abcd", system_type="", 
						interface_version=?VERSION, addr_ton=2, 
						addr_npi=1,address_range=""}}], <<0,0,0,37>>},
						smpp34pdu:unpack(<<0,0,0,16,128,0,0,6,0,0,0,0,0,
								  0,0,1,0,0,0,37,0,0,0,9,0,0,
								  0,0,0,0,0,2,97,98,99,100,
								  101,102,103,104,105,106,0,
								  97,98,99,100,0,0,52,2,1,0,
								  0,0,0,37,0,0,0,2,0,0,0,0,
								  0,0,0,3,97,98,99,100,101,102,
								  103,104,105,106,0,97,98,99,
								  100,0,0,52,2,1,0,0,0,0,37>>))}
	].
