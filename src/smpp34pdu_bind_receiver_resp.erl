-module(smpp34pdu_bind_receiver_resp).
-include("smpp34pdu.hrl").
-include("smpp34pdu_types.hrl").
-include("smpp34pdu_tlv_macros.hrl").
-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2]).

-spec pack(bind_receiver_resp()) -> binary().
-spec unpack(binary()) -> bind_receiver_resp().
-spec unpack_tlv_fields(binary(), bind_receiver_resp()) -> bind_receiver_resp().

pack(#bind_receiver_resp{system_id=SystemId,
        sc_interface_version=ScIntVersion}) ->

        L = [cstring_to_bin(SystemId, 16),
                       tlv:pack(?SC_INTERFACE_VERSION, ScIntVersion)],

        list_to_binary(L).


unpack(Bin0) ->
    {SystemId, Bin1} = bin_to_cstring(Bin0, 16),
    unpack_tlv_fields(Bin1, #bind_receiver_resp{system_id=SystemId}).


?TLV_UNPACK_EMPTY_BIN();
?TLV_UNPACK_FIELD(bind_receiver_resp, sc_interface_version, ?SC_INTERFACE_VERSION);
?TLV_UNPACK_UNEXPECTED().
