cmn = require "common"

switch2hid_protocol = Proto("sw2_hid",  "Nintendo Switch 2 HID")

local inputType =          ProtoField.uint8("sw2_hid.inputType",          "InputType",          base.HEX)
local packetId =           ProtoField.uint8("sw2_hid.packetId",           "PacketId",           base.HEX)
local status =             ProtoField.uint8("sw2_hid.status",             "Status",             base.HEX)
local buttons =            ProtoField.uint8("sw2_hid.buttons",            "Buttons",            base.HEX)
local leftStick =          ProtoField.uint8("sw2_hid.leftStick",          "LeftStick",          base.HEX)
local rightStick =         ProtoField.uint8("sw2_hid.rightStick",         "RightStick",         base.HEX)
local vibrationCode =      ProtoField.uint8("sw2_hid.vibrationCode",      "VibrationCode",      base.HEX)
local leftAnalogTrigger =  ProtoField.uint8("sw2_hid.leftAnalogTrigger",  "LeftAnalogTrigger",  base.HEX)
local rightAnalogTrigger = ProtoField.uint8("sw2_hid.rightAnalogTrigger", "RightAnalogTrigger", base.HEX)
local imuLength =          ProtoField.uint8("sw2_hid.imuLength",          "ImuLength",          base.HEX)
local imuSample =          ProtoField.uint16("sw2_hid.imuSample",         "ImuSample",          base.HEX)
local motion =             ProtoField.bytes("sw2_hid.motion",             "Motion",             base.SPACE)

switch2hid_protocol.fields = {inputType, packetId, status, buttons, leftStick, rightStick, vibrationCode, leftAnalogTrigger, rightAnalogTrigger,
                              imuLength, imuSample, motion}

-- Input report types
local NullInputReport =   0x00 -- Empty no data defined
local InputReport02 =     0x02 -- Unknown
local SimpleInputReport = 0x09 -- 4ms updates, status, button, sticks, triggers and motion

-- Buttons
local B_BUTTON_BIT =        0x000001
local A_BUTTON_BIT =        0x000002
local Y_BUTTON_BIT =        0x000004
local X_BUTTON_BIT =        0x000008
local R_BUTTON_BIT =        0x000010
local ZR_BUTTON_BIT =       0x000020
local PLUS_BUTTON_BIT =     0x000040
local STICK_R_BUTTON_BIT =  0x000080

local DOWN_BUTTON_BIT =     0x000100
local RIGHT_BUTTON_BIT =    0x000200
local LEFT_BUTTON_BIT =     0x000400
local UP_BUTTON_BIT =       0x000800
local L_BUTTON_BIT =        0x001000
local ZL_BUTTON_BIT =       0x002000
local MINUS_BUTTON_BIT =    0x004000
local STICK_L_BUTTON_BIT =  0x008000

local HOME_BUTTON_BIT =     0x010000
local CAPTURE_BUTTON_BIT =  0x020000
local GR_BUTTON_BIT =       0x040000
local GL_BUTTON_BIT =       0x080000
local C_BUTTON_BIT =        0x100000
local LEFT_SL_BUTTON_BIT =  0x000000 -- Not confirmed
local LEFT_SR_BUTTON_BIT =  0x000000 -- Not confirmed
local RIGHT_SL_BUTTON_BIT = 0x000000 -- Not confirmed
local RIGHT_SR_BUTTON_BIT = 0x000000 -- Not confirmed

local function parse_buttons(buttons_value)
    -- byte & (1 << n) > 0
    local function is_bit_set(byte, mask)
    return bit.band(byte, mask) > 0
 end

    local buttons_array = {}

    if is_bit_set(buttons_value, DOWN_BUTTON_BIT) then     table.insert(buttons_array, "down") end
    if is_bit_set(buttons_value, UP_BUTTON_BIT) then       table.insert(buttons_array, "up") end
    if is_bit_set(buttons_value, RIGHT_BUTTON_BIT) then    table.insert(buttons_array, "right") end
    if is_bit_set(buttons_value, LEFT_BUTTON_BIT) then     table.insert(buttons_array, "left") end
    if is_bit_set(buttons_value, LEFT_SR_BUTTON_BIT) then  table.insert(buttons_array, "left SR") end
    if is_bit_set(buttons_value, LEFT_SL_BUTTON_BIT) then  table.insert(buttons_array, "left SL") end
    if is_bit_set(buttons_value, L_BUTTON_BIT) then        table.insert(buttons_array, "L") end
    if is_bit_set(buttons_value, ZL_BUTTON_BIT) then       table.insert(buttons_array, "ZL") end
    if is_bit_set(buttons_value, Y_BUTTON_BIT) then        table.insert(buttons_array, "Y") end
    if is_bit_set(buttons_value, X_BUTTON_BIT) then        table.insert(buttons_array, "X") end
    if is_bit_set(buttons_value, B_BUTTON_BIT) then        table.insert(buttons_array, "B") end
    if is_bit_set(buttons_value, A_BUTTON_BIT) then        table.insert(buttons_array, "A") end
    if is_bit_set(buttons_value, RIGHT_SR_BUTTON_BIT) then table.insert(buttons_array, "right SR") end
    if is_bit_set(buttons_value, RIGHT_SL_BUTTON_BIT) then table.insert(buttons_array, "right SL") end
    if is_bit_set(buttons_value, R_BUTTON_BIT) then        table.insert(buttons_array, "R") end
    if is_bit_set(buttons_value, ZR_BUTTON_BIT) then       table.insert(buttons_array, "ZR") end
    if is_bit_set(buttons_value, MINUS_BUTTON_BIT) then    table.insert(buttons_array, "minus") end
    if is_bit_set(buttons_value, PLUS_BUTTON_BIT) then     table.insert(buttons_array, "plus") end
    if is_bit_set(buttons_value, STICK_R_BUTTON_BIT) then  table.insert(buttons_array, "stick R") end
    if is_bit_set(buttons_value, STICK_L_BUTTON_BIT) then  table.insert(buttons_array, "stick L") end
    if is_bit_set(buttons_value, HOME_BUTTON_BIT) then     table.insert(buttons_array, "home") end
    if is_bit_set(buttons_value, CAPTURE_BUTTON_BIT) then  table.insert(buttons_array, "capture") end
    if is_bit_set(buttons_value, C_BUTTON_BIT) then        table.insert(buttons_array, "C") end
    if is_bit_set(buttons_value, GR_BUTTON_BIT) then       table.insert(buttons_array, "GR") end
    if is_bit_set(buttons_value, GL_BUTTON_BIT) then       table.insert(buttons_array, "GL") end

    local buttons_text = " (none)"

    if #buttons_array ~= 0 then
        buttons_text = " (" .. table.concat(buttons_array, ", ") .. ")"
 end

    return buttons_text
end

local function parse_stick(stick_value)
    local raw_axis_x = stick_value:get_index(0) + bit.lshift(bit.band(stick_value:get_index(1), 0xF), 8)
    local raw_axis_y = bit.lshift(stick_value:get_index(2), 4) + bit.rshift(stick_value:get_index(1), 4)

    return " (" .. raw_axis_x .. ", " .. raw_axis_y .. ")"
end

local function parse_imu_sample(sample_value)
    return sample_value:get_index(0) + bit.lshift(bit.band(sample_value:get_index(1), 0xF), 8)
end

local function parse_motion(buffer, tree)
    local imu_sample_value = buffer(0, 2)
    local imu_sample_text = parse_imu_sample(imu_sample_value:bytes())
    
    tree:add_le(imuSample, imu_sample_value):append_text(" ("..imu_sample_text..")")
    return ", Motion timestamp " .. imu_sample_text
end

local function parse_input_report(buffer, pinfo, tree)
    local packet_id_value =      buffer(1, 1)
    local status_value =         buffer(2, 1)
    local buttons_value =        buffer(3, 3)
    local stick_l_value =        buffer(6, 3)
    local stick_r_value =        buffer(9, 3)
    local vibration_code_value = buffer(12, 1)
    local analog_l_value =       buffer(13, 1)
    local analog_r_value =       buffer(14, 1)
    local imu_length_value =     buffer(15, 1)
    local motion_buffer =        buffer(16, imu_length_value:le_uint())
    
    local buttons_text = parse_buttons(buttons_value:le_uint())
    local stick_l_text = parse_stick(stick_l_value:bytes())
    local stick_r_text = parse_stick(stick_r_value:bytes())

    tree:add_le(packetId, packet_id_value)
    tree:add_le(status, status_value)
    tree:add_le(buttons, buttons_value):append_text(buttons_text)
    tree:add_le(leftStick, stick_l_value):append_text(stick_l_text)
    tree:add_le(rightStick, stick_r_value):append_text(stick_r_text)
    tree:add_le(vibrationCode, vibration_code_value)
    tree:add_le(leftAnalogTrigger, analog_l_value)
    tree:add_le(rightAnalogTrigger, analog_r_value)
    tree:add_le(imuLength, imu_length_value)

    local info = "Input report: Buttons" .. buttons_text .. " LStick" .. stick_l_text .. " RStick" .. stick_r_text
    info = info .. " LTrigger 0x" .. analog_l_value .. " RTrigger 0x" .. analog_r_value

    if imu_length_value:le_uint() > 0 then
        tree:add_le(motion, motion_buffer)
        info = info .. parse_motion(motion_buffer, tree)
 end

    pinfo.cols.info = info
end

function switch2hid_protocol.dissector(buffer, pinfo, tree)
    length = buffer:len()
    if length == 0 then return end

    pinfo.cols.protocol = switch2hid_protocol.name

    local subtree = tree:add(switch2hid_protocol, buffer(), "Switch2 HID Data")
    local input_type_value = buffer(0, 1)

    subtree:add_le(inputType, input_type_value)

    if     input_type_value:le_uint() == NullInputReport then   pinfo.cols.info = "Empty input report"
    elseif input_type_value:le_uint() == SimpleInputReport then parse_input_report(buffer, pinfo, subtree)
    else pinfo.cols.info = "Unknown input report type " .. input_type_value end
end

DissectorTable.get("usb.interrupt"):add(0x03, switch2hid_protocol)