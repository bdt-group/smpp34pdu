-module(smpp34pdu_submit_sm_tests).
-include("../include/smpp34pdu.hrl").
-include_lib("eunit/include/eunit.hrl").

submit_sm_no_tlv_test_() ->
    PayLoad = #submit_sm{service_type="CMT",
        source_addr_ton=2,
        source_addr_npi=1,
        source_addr="abcd",
        dest_addr_ton=2,
        dest_addr_npi=1,
        destination_addr="efgh",
        esm_class=1,
        protocol_id=2,
        priority_flag=1,
        schedule_delivery_time="100716133059001+",
        validity_period="000014000000000R",
        registered_delivery=1,
        replace_if_present_flag=1,
        data_coding=1,
        sm_default_msg_id=1,
        sm_length=11,
        short_message="hello world"},

    Bin = <<67,77,84,0,
            2,1,
            97,98,99,100,0,
            2,1,
            101,102,103,104,0,
            1,2,1,
            $1,$0,$0,$7,$1,$6,$1,$3,$3,$0,$5,$9,$0,$0,$1,$+,0,
            $0,$0,$0,$0,$1,$4,$0,$0,$0,$0,$0,$0,$0,$0,$0,$R,0,
            1,1,1,1,11,
            104,101,108,108,111,32,119,111,114,108,100>>,

    [
        {"Packing PayLoad will give Bin",
            ?_assertEqual(Bin, smpp34pdu_submit_sm:pack(PayLoad))},
        {"Unpacking Bin will give PayLoad",
            ?_assertEqual(PayLoad, smpp34pdu_submit_sm:unpack(Bin))},
        {"Packing and Unpacking PayLoad will give you PayLoad",
            ?_assertEqual(PayLoad,
                        smpp34pdu_submit_sm:unpack(smpp34pdu_submit_sm:pack(PayLoad)))},
        {"Unpacking and Packing Bin will give you Bin",
            ?_assertEqual(Bin,
                        smpp34pdu_submit_sm:pack(smpp34pdu_submit_sm:unpack(Bin)))}
    ].


submit_sm_tlv_usr_msg_ref_test_() ->
    PayLoad = #submit_sm{service_type="CMT",
        source_addr_ton=2,
        source_addr_npi=1,
        source_addr="abcd",
        dest_addr_ton=2,
        dest_addr_npi=1,
        destination_addr="efgh",
        esm_class=1,
        protocol_id=2,
        priority_flag=1,
        schedule_delivery_time="100716133059001+",
        validity_period="000014000000000R",
        registered_delivery=1,
        replace_if_present_flag=1,
        data_coding=1,
        sm_default_msg_id=1,
        sm_length=11,
        short_message="hello world",
        user_message_reference=4},

    Bin = <<67,77,84,0,
            2,1,
            97,98,99,100,0,
            2,1,
            101,102,103,104,0,
            1,2,1,
            $1,$0,$0,$7,$1,$6,$1,$3,$3,$0,$5,$9,$0,$0,$1,$+,0,
            $0,$0,$0,$0,$1,$4,$0,$0,$0,$0,$0,$0,$0,$0,$0,$R,0,
            1,1,1,1,11,
            104,101,108,108,111,32,119,111,114,108,100,
            2,4,0,2,0,4>>,

    [
        {"Packing PayLoad will give Bin",
            ?_assertEqual(Bin, smpp34pdu_submit_sm:pack(PayLoad))},
        {"Unpacking Bin will give PayLoad",
            ?_assertEqual(PayLoad, smpp34pdu_submit_sm:unpack(Bin))},
        {"Packing and Unpacking PayLoad will give you PayLoad",
            ?_assertEqual(PayLoad,
                        smpp34pdu_submit_sm:unpack(smpp34pdu_submit_sm:pack(PayLoad)))},
        {"Unpacking and Packing Bin will give you Bin",
            ?_assertEqual(Bin,
                        smpp34pdu_submit_sm:pack(smpp34pdu_submit_sm:unpack(Bin)))}
    ].

submit_sm_tlv_src_port_test_() ->
    PayLoad = #submit_sm{service_type="CMT",
        source_addr_ton=2,
        source_addr_npi=1,
        source_addr="abcd",
        dest_addr_ton=2,
        dest_addr_npi=1,
        destination_addr="efgh",
        esm_class=1,
        protocol_id=2,
        priority_flag=1,
        schedule_delivery_time="100716133059001+",
        validity_period="000014000000000R",
        registered_delivery=1,
        replace_if_present_flag=1,
        data_coding=1,
        sm_default_msg_id=1,
        sm_length=11,
        short_message="hello world",
        source_port=11},

    Bin = <<67,77,84,0,
            2,1,
            97,98,99,100,0,
            2,1,
            101,102,103,104,0,
            1,2,1,
            $1,$0,$0,$7,$1,$6,$1,$3,$3,$0,$5,$9,$0,$0,$1,$+,0,
            $0,$0,$0,$0,$1,$4,$0,$0,$0,$0,$0,$0,$0,$0,$0,$R,0,
            1,1,1,1,11,
            104,101,108,108,111,32,119,111,114,108,100,
            2,10,0,2,0,11>>,

    [
        {"Packing PayLoad will give Bin",
            ?_assertEqual(Bin, smpp34pdu_submit_sm:pack(PayLoad))},
        {"Unpacking Bin will give PayLoad",
            ?_assertEqual(PayLoad, smpp34pdu_submit_sm:unpack(Bin))},
        {"Packing and Unpacking PayLoad will give you PayLoad",
            ?_assertEqual(PayLoad,
                        smpp34pdu_submit_sm:unpack(smpp34pdu_submit_sm:pack(PayLoad)))},
        {"Unpacking and Packing Bin will give you Bin",
            ?_assertEqual(Bin,
                        smpp34pdu_submit_sm:pack(smpp34pdu_submit_sm:unpack(Bin)))}
    ].

submit_sm_custom_tlv_unpacking_test_() ->
    PayLoad = #submit_sm{service_type="CMT",
        source_addr_ton=2,
        source_addr_npi=1,
        source_addr="abcd",
        dest_addr_ton=2,
        dest_addr_npi=1,
        destination_addr="efgh",
        esm_class=1,
        protocol_id=2,
        priority_flag=1,
        schedule_delivery_time="100716133059001+",
        validity_period="000014000000000R",
        registered_delivery=1,
        replace_if_present_flag=1,
        data_coding=1,
        sm_default_msg_id=1,
        sm_length=11,
        short_message="hello world",
        source_port=11,
        vendor_specific = #{
            16#3FFF => <<254, 253>>
        }
    },

    Bin = <<67,77,84,0,
            2,1,
            97,98,99,100,0,
            2,1,
            101,102,103,104,0,
            1,2,1,
            $1,$0,$0,$7,$1,$6,$1,$3,$3,$0,$5,$9,$0,$0,$1,$+,0,
            $0,$0,$0,$0,$1,$4,$0,$0,$0,$0,$0,$0,$0,$0,$0,$R,0,
            1,1,1,1,11,
            104,101,108,108,111,32,119,111,114,108,100,
            2,10,0,2,0,11,
            16#3F, 16#FF,0,2,254,253>>,

    [
        {"Unpacking Bin with custom TLV will give PayLoad with custom vendor-specific TLV",
            ?_assertEqual(PayLoad, smpp34pdu_submit_sm:unpack(Bin))}
    ].

submit_sm_custom_tlv_packing_test_() ->
    PayLoad = #submit_sm{service_type="CMT",
        source_addr_ton=2,
        source_addr_npi=1,
        source_addr="abcd",
        dest_addr_ton=2,
        dest_addr_npi=1,
        destination_addr="efgh",
        esm_class=1,
        protocol_id=2,
        priority_flag=1,
        schedule_delivery_time="100716133059001+",
        validity_period="000014000000000R",
        registered_delivery=1,
        replace_if_present_flag=1,
        data_coding=1,
        sm_default_msg_id=1,
        sm_length=11,
        short_message="hello world",
        source_port=11,
        vendor_specific = #{
            16#3FFF => <<254, 253>>
        }
    },

    Bin = <<67,77,84,0,
            2,1,
            97,98,99,100,0,
            2,1,
            101,102,103,104,0,
            1,2,1,
            $1,$0,$0,$7,$1,$6,$1,$3,$3,$0,$5,$9,$0,$0,$1,$+,0,
            $0,$0,$0,$0,$1,$4,$0,$0,$0,$0,$0,$0,$0,$0,$0,$R,0,
            1,1,1,1,11,
            104,101,108,108,111,32,119,111,114,108,100,
            2,10,0,2,0,11,
            16#3F, 16#FF,0,2,254,253>>,

    [
        {"Packing with custom TLV will honor custom TLVs",
            ?_assertEqual(Bin, smpp34pdu_submit_sm:pack(PayLoad))}
    ].
