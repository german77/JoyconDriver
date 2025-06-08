switch2hid_protocol = Proto("sw2_hid",  "Nintendo Switch 2 HID")

local inputType  = ProtoField.uint8("sw2_hid.inputType", "InputType", base.HEX)
local packetId  = ProtoField.uint8("sw2_hid.packetId", "PacketId", base.DEC)
local buttons   = ProtoField.uint8("sw2_hid.buttons",  "Buttons",  base.HEX)
local leftStick   = ProtoField.uint8("sw2_hid.leftStick",  "LeftStick",  base.HEX)
local rightStick   = ProtoField.uint8("sw2_hid.rightStick",  "RightStick",  base.HEX)
local vibrationCode   = ProtoField.uint8("sw2_hid.vibrationCode",  "VibrationCode",  base.HEX)
local motion   = ProtoField.bytes("sw2_hid.motion",  "Motion",  base.SPACE)

switch2hid_protocol.fields = {inputType, packetId, buttons, leftStick, rightStick, vibrationCode, motion}


switch2_protocol = Proto("switch2",  "Nintendo Switch 2 controller Protocol")

local reportType  = ProtoField.uint8("switch2.reportType", "reportType", base.HEX)
local reportMode  = ProtoField.uint8("switch2.reportMode", "reportMode", base.HEX)
local command  = ProtoField.uint8("switch2.command", "Command", base.HEX)
local result  = ProtoField.uint8("switch2.result", "Result", base.HEX)
-- spi
local spiLength  = ProtoField.uint8("switch2.spiLength", "SpiLength", base.DEC)
local spiCommand  = ProtoField.uint8("switch2.spiCommand", "SpiCommand", base.HEX)
local spiAddress  = ProtoField.uint8("switch2.spiAddress", "SpiAddress", base.HEX)
local spiData  = ProtoField.bytes("switch2.spiData", "SpiData", base.NONE)
-- led
local ledPattern  = ProtoField.uint8("switch2.ledPattern", "LedPattern", base.HEX)

switch2_protocol.fields = {reportType, reportMode, command, result, spiLength, spiCommand, spiAddress, spiData, ledPattern}

local function parse_buttons(buttons_value)
    -- byte & (1 << n) > 0
    local function is_bit_set(byte, n)
    return bit.band(byte, bit.lshift(1, n)) > 0
    end

    local DOWN_BUTTON_BIT = 0
    local UP_BUTTON_BIT = 1
    local RIGHT_BUTTON_BIT = 2
    local LEFT_BUTTON_BIT = 3
    local LEFT_SR_BUTTON_BIT = 4
    local LEFT_SL_BUTTON_BIT = 5
    local L_BUTTON_BIT = 6
    local ZL_BUTTON_BIT = 7
    local Y_BUTTON_BIT = 8
    local X_BUTTON_BIT = 9
    local B_BUTTON_BIT = 10
    local A_BUTTON_BIT = 11
    local RIGHT_SR_BUTTON_BIT = 12
    local RIGHT_SL_BUTTON_BIT = 13
    local R_BUTTON_BIT = 14
    local ZR_BUTTON_BIT = 15
    local MINUS_BUTTON_BIT = 16
    local PLUS_BUTTON_BIT = 17
    local STICK_R_BUTTON_BIT = 18
    local STICK_L_BUTTON_BIT = 19
    local HOME_BUTTON_BIT = 20
    local CAPTURE_BUTTON_BIT = 21

    local buttons_array = {}

    if is_bit_set(buttons_value, DOWN_BUTTON_BIT)    then table.insert(buttons_array, "down")    end
    if is_bit_set(buttons_value, UP_BUTTON_BIT)    then table.insert(buttons_array, "up")    end
    if is_bit_set(buttons_value, RIGHT_BUTTON_BIT)    then table.insert(buttons_array, "right")    end
    if is_bit_set(buttons_value, LEFT_BUTTON_BIT)    then table.insert(buttons_array, "left")    end
    if is_bit_set(buttons_value, LEFT_SR_BUTTON_BIT)    then table.insert(buttons_array, "left SR")    end
    if is_bit_set(buttons_value, LEFT_SL_BUTTON_BIT)    then table.insert(buttons_array, "left SL")    end
    if is_bit_set(buttons_value, L_BUTTON_BIT)    then table.insert(buttons_array, "L")    end
    if is_bit_set(buttons_value, ZL_BUTTON_BIT)    then table.insert(buttons_array, "ZL")    end
    if is_bit_set(buttons_value, Y_BUTTON_BIT)    then table.insert(buttons_array, "Y")    end
    if is_bit_set(buttons_value, X_BUTTON_BIT)    then table.insert(buttons_array, "X")    end
    if is_bit_set(buttons_value, B_BUTTON_BIT)    then table.insert(buttons_array, "B")    end
    if is_bit_set(buttons_value, A_BUTTON_BIT)    then table.insert(buttons_array, "A")    end
    if is_bit_set(buttons_value, RIGHT_SR_BUTTON_BIT)    then table.insert(buttons_array, "right SR")    end
    if is_bit_set(buttons_value, RIGHT_SL_BUTTON_BIT)    then table.insert(buttons_array, "right SL")    end
    if is_bit_set(buttons_value, R_BUTTON_BIT)    then table.insert(buttons_array, "R")    end
    if is_bit_set(buttons_value, ZR_BUTTON_BIT)    then table.insert(buttons_array, "ZR")    end
    if is_bit_set(buttons_value, MINUS_BUTTON_BIT)    then table.insert(buttons_array, "minus")    end
    if is_bit_set(buttons_value, PLUS_BUTTON_BIT)    then table.insert(buttons_array, "plus")    end
    if is_bit_set(buttons_value, STICK_R_BUTTON_BIT)    then table.insert(buttons_array, "stick R")    end
    if is_bit_set(buttons_value, STICK_L_BUTTON_BIT)    then table.insert(buttons_array, "stick L")    end
    if is_bit_set(buttons_value, HOME_BUTTON_BIT)    then table.insert(buttons_array, "home")    end
    if is_bit_set(buttons_value, CAPTURE_BUTTON_BIT)    then table.insert(buttons_array, "capture")    end

    local buttons_text = " (none)"

    if #buttons_array ~= 0 then
        buttons_text = " (" .. table.concat(buttons_array, ", ") .. ")"
    end

    return buttons_text
end

local function parse_stick(stick_value)
    local raw_axis_x = stick_value:get_index(0) + bit.lshift(bit.band(stick_value:get_index(1),0xF),8)
    local raw_axis_y = bit.lshift(stick_value:get_index(2),4) + bit.rshift(stick_value:get_index(1),4)

    return " (" .. raw_axis_x .. ", " .. raw_axis_y .. ")"
end


local function parse_input_report(buffer, pinfo, subtree)
    local packet_id_value = buffer(1,1)
    local packet_id_text = packet_id_value:le_uint()
    local buttons_value = buffer(3,3)
    local buttons_text   = parse_buttons(buttons_value:le_uint())
    local stick_l_value = buffer(6,3)
    local stick_l_text   = parse_stick(stick_l_value:bytes())
    local stick_r_value = buffer(9,3)
    local stick_r_text   = parse_stick(stick_r_value:bytes())

    subtree:add_le(packetId, packet_id_value)
    subtree:add_le(buttons, buttons_value):append_text(buttons_text)
    subtree:add_le(leftStick, stick_l_value):append_text(stick_l_text)
    subtree:add_le(rightStick, stick_r_value):append_text(stick_r_text)
    subtree:add_le(vibrationCode, buffer(12,1))
    subtree:add_le(motion, buffer(13,12*3))

    pinfo.cols.info = "Input report "..packet_id_text..":"..buttons_text..stick_l_text..stick_r_text
end

function switch2hid_protocol.dissector(buffer, pinfo, tree)
    length = buffer:len()
    if length == 0 then return end

    pinfo.cols.protocol = switch2hid_protocol.name

    local subtree = tree:add(switch2hid_protocol, buffer(), "Switch2 HID Data")

    local input_type_value = buffer(0,1):le_uint()
    subtree:add_le(inputType,   buffer(0,1))
    
    if input_type_value == 0x09 then parse_input_report(buffer, pinfo, subtree) end

end

local function hex(value)
    return  string.format('%02x',value)
end

local function getBytes(buffer)
    length = buffer:len() -1
    local buttons_array = {}
    local buttons_text = " (none)"

    if length > 16 then length=16 end
    for i=0,length do 
        table.insert(buttons_array, hex(buffer:bytes():get_index(i)))
    end

    if #buttons_array ~= 0 then
        buttons_text = " (" .. table.concat(buttons_array, ",") .. ")"
    end
    return buttons_text
end

local function parse_result(result_value)
    if result_value == 0xf8 then return " (ACK)" end
    return " (Unknown)"
end

local function parse_spi_address(address)
    return " (Unknown)"
end

local function parse_spi_command(buffer, pinfo, tree)
    local length_value = buffer(8,1)
    local sub_command_value = buffer(9,1)
    local address_value = buffer(0xc,4)
    
    tree:add_le(spiLength, length_value)
    tree:add_le(spiCommand, sub_command_value)
    tree:add_le(spiAddress,  address_value)

    pinfo.cols.info = "Request SPI: address 0x" .. hex(address_value:le_uint()) .. " size 0x"..length_value
    return " (SPI)"
end

local function parse_player_lights_command(buffer, pinfo, tree)
    local led_pattern_value = buffer(8,1)

    tree:add_le(ledPattern, led_pattern_value)

    pinfo.cols.info = "Request Player lights: led pattern 0x" .. hex(led_pattern_value:le_uint())
    return " (Player lights)"
end

local function parse_imu_command(buffer, pinfo, tree)
    pinfo.cols.info = "Request IMU: Enable motion"
    return " (IMU)"
end

local function parse_request(buffer, pinfo, tree)
    local report_type_value = buffer(0,1):le_uint()
    local report_type_text = "(Unknown)"

    if report_type_value == 0x02 then report_type_text = parse_spi_command(buffer, pinfo, tree)
    elseif report_type_value == 0x09 then report_type_text = parse_player_lights_command(buffer, pinfo, tree)
    elseif report_type_value == 0x0c then report_type_text = parse_imu_command(buffer, pinfo, tree)
    else pinfo.cols.info = "Request(0x"..hex(report_type_value)..") ->".. getBytes(buffer) end

    tree:add_le(reportType,   buffer(0,1)):append_text(report_type_text)

    return " (Request)"
end

local function parse_spi_reply(buffer, pinfo, tree)
    local length_value = buffer(8,1)
    local result_value = buffer(5,1)
    local address_value = buffer(0xc,4)
    local data_value = buffer(0x10,length_value:le_uint())

    tree:add_le(spiLength, length_value)
    tree:add_le(result, result_value):append_text(parse_result(result_value:le_uint()))
    tree:add_le(spiAddress,  address_value):append_text(parse_spi_address(result_value:le_uint()))
    tree:add_le(spiData,  data_value)

    pinfo.cols.info = "Reply SPI: address 0x" .. hex(address_value:le_uint()) .. " size 0x"..length_value
    return " (SPI)"
end

local function parse_player_lights_reply(buffer, pinfo, tree)
    local result_value = buffer(5,1)
    local result_text = parse_result(result_value:le_uint())

    tree:add_le(result, result_value):append_text(result_text)

    pinfo.cols.info = "Reply Player lights:" .. result_text
    return " (Player lights)"
end

local function parse_imu_reply(buffer, pinfo, tree)
    local result_value = buffer(5,1)
    local result_text = parse_result(result_value:le_uint())

    tree:add_le(result, result_value):append_text(result_text)

    pinfo.cols.info = "Reply IMU:" .. result_text
    return " (IMU)"
end

local function parse_reply(buffer, pinfo, tree)
    local report_type_value = buffer(0,1):le_uint()
    local report_type_text = "(Unknown)"

    if report_type_value == 0x02 then report_type_text = parse_spi_reply(buffer, pinfo, tree)
    elseif report_type_value == 0x09 then report_type_text = parse_player_lights_reply(buffer, pinfo, tree)
    elseif report_type_value == 0x0c then report_type_text = parse_imu_reply(buffer, pinfo, tree)
    else pinfo.cols.info = "Reply(0x"..hex(report_type_value)..") ->".. getBytes(buffer) end

    tree:add_le(reportType,   buffer(0,1)):append_text(report_type_text)

    return " (Reply)"
end

function switch2_protocol.dissector(buffer, pinfo, tree)
    length = buffer:len()
    if length == 0 then return end

    pinfo.cols.protocol = switch2_protocol.name

    local subtree = tree:add(switch2_protocol, buffer(), "Switch2 Protocol Data")
    
    local report_mode_value = buffer(1,1):le_uint()
    local report_mode_text = " (Unknown)"

    if report_mode_value == 0x01 then report_mode_text = parse_reply(buffer, pinfo, subtree)
    elseif report_mode_value == 0x91 then report_mode_text = parse_request(buffer, pinfo, subtree) end

    subtree:add_le(reportMode,   buffer(1,1)):append_text(report_mode_text)
    
end

DissectorTable.get("usb.interrupt"):add(0x03, switch2hid_protocol)
DissectorTable.get("usb.bulk"):add(0xFF, switch2_protocol)