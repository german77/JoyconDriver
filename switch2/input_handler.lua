cmn = require "common"

switch2hid_protocol = Proto("sw2_hid",  "Nintendo Switch 2 HID")
switch2hidble_protocol = Proto("sw2_hid_ble",  "Nintendo Switch 2 HID BLE")
switch2hidble_protocol2 = Proto("sw2_hid_ble2",  "Nintendo Switch 2 HID BLE2")

usb_payload = Field.new("usbll.data")

-- Input report types
local InputReport = {
    Null =     0x00, -- Empty no data defined
    Report02 = 0x02, -- Unknown
    Left =     0x07, -- 4ms updates, status, button, sticks, triggers and motion
    Right =    0x08, -- 4ms updates, status, button, sticks, triggers and motion
    Pro =      0x09, -- 4ms updates, status, button, sticks, triggers and motion
    Gc =       0x0a, -- 4ms updates, status, button, sticks, triggers and motion
}

local InputReportNames = {
    [InputReport.Null] =     "Null Input Report",
    [InputReport.Report02] = "Input Report 02",
    [InputReport.Left] =     "Left Input Report",
    [InputReport.Right] =    "Right Input Report",
    [InputReport.Pro] =      "Pro Input Report",
    [InputReport.Gc] =       "Gc Input Report"
}

local inputType =          ProtoField.uint8("sw2_hid.inputType",          "InputType",          base.HEX, InputReportNames)
local packetId =           ProtoField.uint8("sw2_hid.packetId",           "PacketId",           base.HEX)
local status =             ProtoField.uint8("sw2_hid.status",             "Status",             base.HEX)
local audioStatus =        ProtoField.uint8("sw2_hid.audioStatus",        "AudioStatus",        base.HEX)
local battery =            ProtoField.uint16("sw2_hid.battery",           "Battery",            base.DEC)
local current =            ProtoField.uint16("sw2_hid.current",           "Current",            base.DEC)
local buttons =            ProtoField.uint8("sw2_hid.buttons",            "Buttons",            base.HEX)
local leftStick =          ProtoField.uint8("sw2_hid.leftStick",          "LeftStick",          base.HEX)
local rightStick =         ProtoField.uint8("sw2_hid.rightStick",         "RightStick",         base.HEX)
local vibrationCode =      ProtoField.uint8("sw2_hid.vibrationCode",      "VibrationCode",      base.HEX)
local leftAnalogTrigger =  ProtoField.uint8("sw2_hid.leftAnalogTrigger",  "LeftAnalogTrigger",  base.HEX)
local rightAnalogTrigger = ProtoField.uint8("sw2_hid.rightAnalogTrigger", "RightAnalogTrigger", base.HEX)
local mouseX =             ProtoField.uint16("sw2_hid.mouseX",            "MouseX",             base.HEX)
local mouseY =             ProtoField.uint16("sw2_hid.mouseY",            "mouseY",             base.HEX)
local mouseUnkA =          ProtoField.uint16("sw2_hid.mouseUnkA",         "mouseUnkA",          base.HEX)
local mouseDistance =      ProtoField.uint16("sw2_hid.mouseDistance",     "mouseDistance",      base.HEX)
local imuLength =          ProtoField.uint8("sw2_hid.imuLength",          "ImuLength",          base.HEX)
local imuSample =          ProtoField.uint16("sw2_hid.imuSample",         "ImuSample",          base.DEC)
local motion =             ProtoField.bytes("sw2_hid.motion",             "Motion",             base.SPACE)
local temperature =        ProtoField.int16("sw2_hid.temperature",        "Temperature",        base.DEC)
local motionAccelX =       ProtoField.int16("sw2_hid.motionGyroX",        "MotionGyroX",        base.DEC)
local motionAccelY =       ProtoField.int16("sw2_hid.motionAccelY",       "motionAccelY",       base.DEC)
local motionAccelZ =       ProtoField.int16("sw2_hid.motionAccelZ",       "motionAccelZ",       base.DEC)
local motionGyroX =        ProtoField.int16("sw2_hid.motionGyroX",        "MotionGyroX",        base.DEC)
local motionGyroY =        ProtoField.int16("sw2_hid.motionGyroY",        "MotionGyroY",        base.DEC)
local motionGyroZ =        ProtoField.int16("sw2_hid.motionGyroZ",        "MotionGyroZ",        base.DEC)
local magnetometerX =      ProtoField.int16("sw2_hid.magnetometerX",      "MagnetometerX",      base.DEC)
local magnetometerY =      ProtoField.int16("sw2_hid.magnetometerY",      "MagnetometerY",      base.DEC)
local magnetometerZ =      ProtoField.int16("sw2_hid.magnetometerZ",      "MagnetometerZ",      base.DEC)

switch2hid_protocol.fields = {inputType, packetId, status, audioStatus, buttons, leftStick, rightStick, vibrationCode, leftAnalogTrigger,
                              rightAnalogTrigger, imuLength, imuSample, motion, mouseX, mouseY, temperature, motionAccelX, motionAccelY, motionAccelZ,
                              motionGyroX, motionGyroY, motionGyroZ, battery, mouseUnkA, mouseDistance, current ,magnetometerX, magnetometerY,
                              magnetometerZ}


-- Buttons
local B_BUTTON_BIT =       0x000001
local A_BUTTON_BIT =       0x000002
local Y_BUTTON_BIT =       0x000004
local X_BUTTON_BIT =       0x000008
local R_BUTTON_BIT =       0x000010
local ZR_BUTTON_BIT =      0x000020
local PLUS_BUTTON_BIT =    0x000040
local STICK_R_BUTTON_BIT = 0x000080

local DOWN_BUTTON_BIT =    0x000100
local RIGHT_BUTTON_BIT =   0x000200
local LEFT_BUTTON_BIT =    0x000400
local UP_BUTTON_BIT =      0x000800
local L_BUTTON_BIT =       0x001000
local ZL_BUTTON_BIT =      0x002000
local MINUS_BUTTON_BIT =   0x004000
local STICK_L_BUTTON_BIT = 0x008000

local HOME_BUTTON_BIT =    0x010000
local CAPTURE_BUTTON_BIT = 0x020000
local GR_BUTTON_BIT =      0x040000
local GL_BUTTON_BIT =      0x080000
local C_BUTTON_BIT =       0x100000
local SR_BUTTON_BIT =      0x400000
local SL_BUTTON_BIT =      0x800000


-- Buttons Format A
local Y_BUTTON_BIT_A =        0x00000001
local X_BUTTON_BIT_A =        0x00000002
local B_BUTTON_BIT_A =        0x00000004
local A_BUTTON_BIT_A =        0x00000008
local SR_RIGHT_BUTTON_BIT_A = 0x00000010
local SL_RIGHT_BUTTON_BIT_A = 0x00000020
local R_BUTTON_BIT_A =        0x00000040
local ZR_BUTTON_BIT_A =       0x00000080

local MINUS_BUTTON_BIT_A =   0x00000100
local PLUS_BUTTON_BIT_A =    0x00000200
local STICK_R_BUTTON_BIT_A = 0x00000400
local STICK_L_BUTTON_BIT_A = 0x00000800
local HOME_BUTTON_BIT_A =    0x00001000
local CAPTURE_BUTTON_BIT_A = 0x00002000
local C_BUTTON_BIT_A =       0x00004000

local DOWN_BUTTON_BIT_A =    0x00010000
local UP_BUTTON_BIT_A =      0x00020000
local RIGHT_BUTTON_BIT_A =   0x00040000
local LEFT_BUTTON_BIT_A =    0x00080000
local SR_LEFT_BUTTON_BIT_A = 0x00100000
local SL_LEFT_BUTTON_BIT_A = 0x00200000
local L_BUTTON_BIT_A =       0x00400000
local ZL_BUTTON_BIT_A =      0x00800000

local GR_BUTTON_BIT_A = 0x01000000
local GL_BUTTON_BIT_A = 0x02000000
local HEADSET_BIT_A =   0x10000000

-- TODO implement this per controller type
local function parse_buttons(buttons_value)
    local buttons_array = {}

    if cmn.isBitSet(buttons_value, DOWN_BUTTON_BIT) then    table.insert(buttons_array, "down") end
    if cmn.isBitSet(buttons_value, UP_BUTTON_BIT) then      table.insert(buttons_array, "up") end
    if cmn.isBitSet(buttons_value, RIGHT_BUTTON_BIT) then   table.insert(buttons_array, "right") end
    if cmn.isBitSet(buttons_value, LEFT_BUTTON_BIT) then    table.insert(buttons_array, "left") end
    if cmn.isBitSet(buttons_value, SR_BUTTON_BIT) then      table.insert(buttons_array, "SR") end
    if cmn.isBitSet(buttons_value, SL_BUTTON_BIT) then      table.insert(buttons_array, "SL") end
    if cmn.isBitSet(buttons_value, L_BUTTON_BIT) then       table.insert(buttons_array, "L") end
    if cmn.isBitSet(buttons_value, ZL_BUTTON_BIT) then      table.insert(buttons_array, "ZL") end
    if cmn.isBitSet(buttons_value, Y_BUTTON_BIT) then       table.insert(buttons_array, "Y") end
    if cmn.isBitSet(buttons_value, X_BUTTON_BIT) then       table.insert(buttons_array, "X") end
    if cmn.isBitSet(buttons_value, B_BUTTON_BIT) then       table.insert(buttons_array, "B") end
    if cmn.isBitSet(buttons_value, A_BUTTON_BIT) then       table.insert(buttons_array, "A") end
    if cmn.isBitSet(buttons_value, R_BUTTON_BIT) then       table.insert(buttons_array, "R") end
    if cmn.isBitSet(buttons_value, ZR_BUTTON_BIT) then      table.insert(buttons_array, "ZR") end
    if cmn.isBitSet(buttons_value, MINUS_BUTTON_BIT) then   table.insert(buttons_array, "minus") end
    if cmn.isBitSet(buttons_value, PLUS_BUTTON_BIT) then    table.insert(buttons_array, "plus") end
    if cmn.isBitSet(buttons_value, STICK_R_BUTTON_BIT) then table.insert(buttons_array, "stick R") end
    if cmn.isBitSet(buttons_value, STICK_L_BUTTON_BIT) then table.insert(buttons_array, "stick L") end
    if cmn.isBitSet(buttons_value, HOME_BUTTON_BIT) then    table.insert(buttons_array, "home") end
    if cmn.isBitSet(buttons_value, CAPTURE_BUTTON_BIT) then table.insert(buttons_array, "capture") end
    if cmn.isBitSet(buttons_value, C_BUTTON_BIT) then       table.insert(buttons_array, "C") end
    if cmn.isBitSet(buttons_value, GR_BUTTON_BIT) then      table.insert(buttons_array, "GR") end
    if cmn.isBitSet(buttons_value, GL_BUTTON_BIT) then      table.insert(buttons_array, "GL") end

    local buttons_text = " (none)"

    if #buttons_array ~= 0 then
        buttons_text = " (" .. table.concat(buttons_array, ", ") .. ")"
    end

    return buttons_text
end

local function parse_buttons2(buttons_value)
    local buttons_array = {}

    if cmn.isBitSet(buttons_value, DOWN_BUTTON_BIT_A) then     table.insert(buttons_array, "down") end
    if cmn.isBitSet(buttons_value, UP_BUTTON_BIT_A) then       table.insert(buttons_array, "up") end
    if cmn.isBitSet(buttons_value, RIGHT_BUTTON_BIT_A) then    table.insert(buttons_array, "right") end
    if cmn.isBitSet(buttons_value, LEFT_BUTTON_BIT_A) then     table.insert(buttons_array, "left") end
    if cmn.isBitSet(buttons_value, SR_LEFT_BUTTON_BIT_A) then  table.insert(buttons_array, "SR") end
    if cmn.isBitSet(buttons_value, SL_LEFT_BUTTON_BIT_A) then  table.insert(buttons_array, "SL") end
    if cmn.isBitSet(buttons_value, SR_RIGHT_BUTTON_BIT_A) then table.insert(buttons_array, "SR") end
    if cmn.isBitSet(buttons_value, SL_RIGHT_BUTTON_BIT_A) then table.insert(buttons_array, "SL") end
    if cmn.isBitSet(buttons_value, L_BUTTON_BIT_A) then        table.insert(buttons_array, "L") end
    if cmn.isBitSet(buttons_value, ZL_BUTTON_BIT_A) then       table.insert(buttons_array, "ZL") end
    if cmn.isBitSet(buttons_value, Y_BUTTON_BIT_A) then        table.insert(buttons_array, "Y") end
    if cmn.isBitSet(buttons_value, X_BUTTON_BIT_A) then        table.insert(buttons_array, "X") end
    if cmn.isBitSet(buttons_value, B_BUTTON_BIT_A) then        table.insert(buttons_array, "B") end
    if cmn.isBitSet(buttons_value, A_BUTTON_BIT_A) then        table.insert(buttons_array, "A") end
    if cmn.isBitSet(buttons_value, R_BUTTON_BIT_A) then        table.insert(buttons_array, "R") end
    if cmn.isBitSet(buttons_value, ZR_BUTTON_BIT_A) then       table.insert(buttons_array, "ZR") end
    if cmn.isBitSet(buttons_value, MINUS_BUTTON_BIT_A) then    table.insert(buttons_array, "minus") end
    if cmn.isBitSet(buttons_value, PLUS_BUTTON_BIT_A) then     table.insert(buttons_array, "plus") end
    if cmn.isBitSet(buttons_value, STICK_R_BUTTON_BIT_A) then  table.insert(buttons_array, "stick R") end
    if cmn.isBitSet(buttons_value, STICK_L_BUTTON_BIT_A) then  table.insert(buttons_array, "stick L") end
    if cmn.isBitSet(buttons_value, HOME_BUTTON_BIT_A) then     table.insert(buttons_array, "home") end
    if cmn.isBitSet(buttons_value, CAPTURE_BUTTON_BIT_A) then  table.insert(buttons_array, "capture") end
    if cmn.isBitSet(buttons_value, C_BUTTON_BIT_A) then        table.insert(buttons_array, "C") end
    if cmn.isBitSet(buttons_value, GR_BUTTON_BIT_A) then       table.insert(buttons_array, "GR") end
    if cmn.isBitSet(buttons_value, GL_BUTTON_BIT_A) then       table.insert(buttons_array, "GL") end
    if cmn.isBitSet(buttons_value, HEADSET_BIT_A) then         table.insert(buttons_array, "Headset") end

    local buttons_text = " (none)"

    if #buttons_array ~= 0 then
        buttons_text = " (" .. table.concat(buttons_array, ", ") .. ")"
    end

    return buttons_text
end

local function parse_raw_axis(raw_value, center_value, max_value, min_value)
    local value = raw_value - center_value
    if value > 0.0 then return value * 1.0 / max_value end
    return value * 1.0 / min_value
end

local function parse_left_stick(stick_value)
    local raw_axis_x = stick_value:get_index(0) + bit.lshift(bit.band(stick_value:get_index(1), 0xF), 8)
    local raw_axis_y = bit.lshift(stick_value:get_index(2), 4) + bit.rshift(stick_value:get_index(1), 4)

    -- TODO: Use constants
    local axis_x = parse_raw_axis(raw_axis_x,1968,1626,1695)
    local axis_y = parse_raw_axis(raw_axis_y,2194,1534,1527)
    return string.format(" (%.2f, %.2f)",axis_x, axis_y)
end

local function parse_right_stick(stick_value)
    local raw_axis_x = stick_value:get_index(0) + bit.lshift(bit.band(stick_value:get_index(1), 0xF), 8)
    local raw_axis_y = bit.lshift(stick_value:get_index(2), 4) + bit.rshift(stick_value:get_index(1), 4)

    -- TODO: Use constants
    local axis_x = parse_raw_axis(raw_axis_x,2253,1457,1612)
    local axis_y = parse_raw_axis(raw_axis_y,2104,1574,1629)
    return string.format(" (%.2f, %.2f)",axis_x, axis_y)
end

local function parse_imu_sample(sample_value)
    return sample_value:get_index(0) + bit.lshift(bit.band(sample_value:get_index(1), 0xF), 8)
end

local function parse_temperature(temperature_value)
    return 25 + (temperature_value / 127.0)
end

local function parse_motion(buffer, tree)
    local imu_sample_value = buffer(0, 2)
    local imu_sample_text = parse_imu_sample(imu_sample_value:bytes())
    
    tree:add_le(imuSample, imu_sample_value):append_text(" ("..imu_sample_text..")")
    return ", Motion timestamp " .. imu_sample_text .. " buffer" .. cmn.getBytes(buffer)
end

local function parse_motion2(buffer, tree)
    local imu_sample_value =  buffer(0x0, 4)
    local temperature_value = buffer(0x4, 2)
    local accel_x_value =     buffer(0x6, 2)
    local accel_y_value =     buffer(0x8, 2)
    local accel_z_value =     buffer(0xa, 2)
    local gyro_x_value =      buffer(0xc, 2)
    local gyro_y_value =      buffer(0xe, 2)
    local gyro_z_value =      buffer(0x10, 2)

    local temperature_text = string.format("%.2f°C",parse_temperature(temperature_value:le_int()))
    local imu_sample_text = string.format("%.3fs",imu_sample_value:le_int()/1000000.0)

    tree:add_le(imuSample, imu_sample_value):append_text(" ("..imu_sample_text..")")
    tree:add_le(temperature, temperature_value):append_text(" ("..temperature_text..")")
    tree:add_le(motionAccelX, accel_x_value)
    tree:add_le(motionAccelY, accel_y_value)
    tree:add_le(motionAccelZ, accel_z_value)
    tree:add_le(motionGyroX, gyro_x_value)
    tree:add_le(motionGyroY, gyro_y_value)
    tree:add_le(motionGyroZ, gyro_z_value)
    return ", Motion timestamp " .. imu_sample_text.. " temperature ".. temperature_text .. " Accel("..accel_x_value:int()..", "..accel_y_value:int()..", "..accel_z_value:int()..") gyro("..gyro_x_value:int()..", "..gyro_y_value:int()..", "..gyro_z_value:int()..")"
end

local function parse_left_input_report(buffer, pinfo, tree)
    local packet_id_value =      buffer(1, 1)
    local status_value =         buffer(2, 1)
    local buttons_value =        buffer(3, 3)
    local stick_l_value =        buffer(6, 3)
    local vibration_code_value = buffer(9, 1)
    local mouse_x_value =        buffer(10, 2)
    local mouse_y_value =        buffer(12, 2)
    local imu_length_value =     buffer(15, 1)
    local motion_buffer_value =  buffer(16, imu_length_value:le_uint())
    local motion_buffer = motion_buffer_value:bytes():tvb("Motion buffer")

    local buttons_text = parse_buttons(buttons_value:le_uint())
    local stick_l_text = parse_left_stick(stick_l_value:bytes())

    tree:add_le(packetId, packet_id_value)
    tree:add_le(status, status_value)
    tree:add_le(buttons, buttons_value):append_text(buttons_text)
    tree:add_le(leftStick, stick_l_value):append_text(stick_l_text)
    tree:add_le(vibrationCode, vibration_code_value)
    tree:add_le(mouseX, mouse_x_value)
    tree:add_le(mouseY, mouse_y_value)
    tree:add_le(imuLength, imu_length_value)

    local info = "Input report: Buttons" .. buttons_text .. " LStick" .. stick_l_text
    info = info .. " mouse (0x" .. mouse_x_value .. ", " .. mouse_y_value .. ")"

    if imu_length_value:le_uint() > 0 then
        tree:add_le(motion, motion_buffer_value)
        info = info .. parse_motion(motion_buffer, tree)
    end

    pinfo.cols.info = info
end

local function parse_right_input_report(buffer, pinfo, tree)
    local packet_id_value =      buffer(1, 1)
    local status_value =         buffer(2, 1)
    local buttons_value =        buffer(3, 3)
    local stick_r_value =        buffer(6, 3)
    local vibration_code_value = buffer(9, 1)
    local mouse_x_value =        buffer(10, 2)
    local mouse_y_value =        buffer(12, 2)
    local imu_length_value =     buffer(15, 1)
    local motion_buffer_value =  buffer(16, imu_length_value:le_uint())
    local motion_buffer = motion_buffer_value:bytes():tvb("Motion buffer")

    local buttons_text = parse_buttons(buttons_value:le_uint())
    local stick_r_text = parse_right_stick(stick_r_value:bytes())

    tree:add_le(packetId, packet_id_value)
    tree:add_le(status, status_value)
    tree:add_le(buttons, buttons_value):append_text(buttons_text)
    tree:add_le(rightStick, stick_r_value):append_text(stick_r_text)
    tree:add_le(vibrationCode, vibration_code_value)
    tree:add_le(mouseX, mouse_x_value)
    tree:add_le(mouseY, mouse_y_value)
    tree:add_le(imuLength, imu_length_value)

    local info = "Input report: Buttons" .. buttons_text .. " RStick" .. stick_l_text
    info = info .. " mouse (0x" .. mouse_x_value .. ", " .. mouse_y_value .. ")"

    if imu_length_value:le_uint() > 0 then
        tree:add_le(motion, motion_buffer_value)
        info = info .. parse_motion(motion_buffer, tree)
    end

    pinfo.cols.info = info
end

local function parse_gc_input_report(buffer, pinfo, tree)
    local packet_id_value =      buffer(1, 1)
    local status_value =         buffer(2, 1)
    local buttons_value =        buffer(3, 3)
    local stick_l_value =        buffer(6, 3)
    local stick_r_value =        buffer(9, 3)
    local vibration_code_value = buffer(12, 1)
    local analog_l_value =       buffer(13, 1)
    local analog_r_value =       buffer(14, 1)
    local imu_length_value =     buffer(15, 1)
    local motion_buffer_value =  buffer(16, imu_length_value:le_uint())
    local motion_buffer = motion_buffer_value:bytes():tvb("Motion buffer")

    local buttons_text = parse_buttons(buttons_value:le_uint())
    local stick_l_text = parse_left_stick(stick_l_value:bytes())
    local stick_r_text = parse_right_stick(stick_r_value:bytes())

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
        tree:add_le(motion, motion_buffer_value)
        info = info .. parse_motion(motion_buffer, tree)
    end

    pinfo.cols.info = info
end

local function parse_pro_input_report(buffer, pinfo, tree)
    local packet_id_value =      buffer(1, 1)
    local status_value =         buffer(2, 1)
    local buttons_value =        buffer(3, 3)
    local stick_l_value =        buffer(6, 3)
    local stick_r_value =        buffer(9, 3)
    local vibration_code_value = buffer(12, 1)
    local audio_value =          buffer(14, 1)
    local imu_length_value =     buffer(15, 1)
    local motion_buffer_value =  buffer(16, imu_length_value:le_uint())
    local motion_buffer = motion_buffer_value:bytes():tvb("Motion buffer")

    local buttons_text = parse_buttons(buttons_value:le_uint())
    local stick_l_text = parse_left_stick(stick_l_value:bytes())
    local stick_r_text = parse_right_stick(stick_r_value:bytes())

    tree:add_le(packetId, packet_id_value)
    tree:add_le(status, status_value)
    tree:add_le(buttons, buttons_value):append_text(buttons_text)
    tree:add_le(leftStick, stick_l_value):append_text(stick_l_text)
    tree:add_le(rightStick, stick_r_value):append_text(stick_r_text)
    tree:add_le(vibrationCode, vibration_code_value)
    tree:add_le(audioStatus, audio_value)
    tree:add_le(imuLength, imu_length_value)

    local info = "Input report: Buttons" .. buttons_text .. " LStick" .. stick_l_text .. " RStick" .. stick_r_text

    if imu_length_value:le_uint() > 0 then
        tree:add_le(motion, motion_buffer_value)
        info = info .. parse_motion(motion_buffer, tree)
    end

    pinfo.cols.info = info
end

local function parse_wireless_input_report(buffer, pinfo, tree)
    local packet_id_value =      buffer(0, 1)
    local status_value =         buffer(1, 1)
    local buttons_value =        buffer(2, 3)
    local stick_l_value =        buffer(5, 3)
    local stick_r_value =        buffer(8, 3)
    local vibration_code_value = buffer(11, 1)
    local analog_l_value =       buffer(12, 1)
    local analog_r_value =       buffer(13, 1)
    local imu_length_value =     buffer(14, 1)
    local motion_buffer_value =  buffer(15, imu_length_value:le_uint())
    local motion_buffer = motion_buffer_value:bytes():tvb("Motion buffer")

    local buttons_text = parse_buttons(buttons_value:le_uint())
    local stick_l_text = parse_left_stick(stick_l_value:bytes())
    local stick_r_text = parse_right_stick(stick_r_value:bytes())

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
        tree:add_le(motion, motion_buffer_value)
        info = info .. parse_motion(motion_buffer, tree)
    end

    pinfo.cols.info = info
end

local function parse_wireless_input_report2(buffer, pinfo, tree)
    local packet_id_value =      buffer(0, 1)
    local status_value =         buffer(1, 1)
    local buttons_value =        buffer(2, 2)
    local stick_l_value =        buffer(5, 3)
    local vibration_code_value = buffer(8, 1)
    local mouse_x_value =        buffer(9, 2)
    local mouse_y_value =        buffer(11, 2)
    local mouse_distance_value = buffer(13, 1)
    local imu_length_value =     buffer(15, 1)
    local motion_buffer_value =  buffer(16, imu_length_value:le_uint())
    local motion_buffer = motion_buffer_value:bytes():tvb("Motion buffer")

    local buttons_text = parse_buttons(buttons_value:le_uint())
    local stick_l_text = parse_left_stick(stick_l_value:bytes())

    tree:add_le(packetId, packet_id_value)
    tree:add_le(status, status_value)
    tree:add_le(buttons, buttons_value):append_text(buttons_text)
    tree:add_le(leftStick, stick_l_value):append_text(stick_l_text)
    tree:add_le(vibrationCode, vibration_code_value)
    tree:add_le(mouseX, mouse_x_value)
    tree:add_le(mouseY, mouse_y_value)
    tree:add_le(mouseDistance, mouse_distance_value)
    tree:add_le(imuLength, imu_length_value)

    local info = "Input report: Buttons" .. buttons_text .. " LStick" .. stick_l_text
    info = info .. " mouse (0x" .. mouse_x_value .. ", " .. mouse_y_value .. ")"

    if imu_length_value:le_uint() > 0 then
        tree:add_le(motion, motion_buffer_value)
        info = info .. parse_motion(motion_buffer, tree)
    end

    pinfo.cols.info = info
end

local function parse_wireless_input_reportA(buffer, pinfo, tree)
    local packet_id_value =      buffer(0x0, 4)
    local buttons_value =        buffer(0x4, 4)
    local stick_l_value =        buffer(0xa, 3)
    local stick_r_value =        buffer(0xd, 3)
    local mouse_x_value =        buffer(0x10, 2)
    local mouse_y_value =        buffer(0x12, 2)
    local mouse_unkA_value =     buffer(0x14, 2)
    local mouse_distance_value = buffer(0x16, 2)
    local magnet_x_value =       buffer(0x19, 2)
    local magnet_y_value =       buffer(0x1b, 2)
    local magnet_z_value =       buffer(0x1d, 2)
    local battery_value =        buffer(0x1f, 2)
    local current_value =        buffer(0x22, 2)
    local motion_buffer_value =  buffer(0x2a, 18)
    local analog_l_value =       buffer(0x3c, 1)
    local analog_r_value =       buffer(0x3d, 1)
    local motion_buffer = motion_buffer_value:bytes():tvb("Motion buffer")

    local buttons_text = parse_buttons2(buttons_value:le_uint())
    local stick_l_text = parse_left_stick(stick_l_value:bytes())
    local stick_r_text = parse_right_stick(stick_r_value:bytes())

    local info = "Input report: Buttons" .. buttons_text .. " LStick" .. stick_l_text

    tree:add_le(packetId, packet_id_value)
    tree:add_le(buttons, buttons_value):append_text(buttons_text)
    tree:add_le(leftStick, stick_l_value):append_text(stick_l_text)
    tree:add_le(rightStick, stick_r_value):append_text(stick_r_text)
    tree:add_le(mouseX, mouse_x_value)
    tree:add_le(mouseY, mouse_y_value)
    tree:add_le(mouseUnkA, mouse_unkA_value)
    tree:add_le(mouseDistance, mouse_distance_value)
    tree:add_le(magnetometerX, magnet_x_value)
    tree:add_le(magnetometerY, magnet_y_value)
    tree:add_le(magnetometerZ, magnet_z_value)
    tree:add_le(battery, battery_value)
    tree:add_le(current, current_value)
    tree:add_le(motion, motion_buffer_value)

    info = info .. parse_motion2(motion_buffer, tree)

    tree:add_le(leftAnalogTrigger, analog_l_value)
    tree:add_le(rightAnalogTrigger, analog_r_value)

    pinfo.cols.info = info
end

function switch2hid_protocol_dissector(buffer, pinfo, tree)
    length = buffer:len()
    if length == 0 then return end

    pinfo.cols.protocol = switch2hid_protocol.name

    local subtree = tree:add(switch2hid_protocol, buffer(), "Switch2 HID Data")
    local input_type_value = buffer(0, 1)

    subtree:add_le(inputType, input_type_value)

    if     input_type_value:le_uint() == InputReport.Null then  pinfo.cols.info = "Empty input report"
    elseif input_type_value:le_uint() == InputReport.Left then  parse_left_input_report(buffer, pinfo, subtree)
    elseif input_type_value:le_uint() == InputReport.Right then parse_right_input_report(buffer, pinfo, subtree)
    elseif input_type_value:le_uint() == InputReport.Pro then   parse_pro_input_report(buffer, pinfo, subtree)
    elseif input_type_value:le_uint() == InputReport.Gc then    parse_gc_input_report(buffer, pinfo, subtree)
    else pinfo.cols.info = "Unknown input report type " .. input_type_value end
end

function switch2hid_protocol.dissector(buffer, pinfo, tree)
    payload = { usb_payload() }
    if #payload <= 0 then return end

    for k, field in pairs(payload) do
        local payload_buffer = field.range:bytes():tvb("Payload")
        if buffer:len() < 8 then return end
        switch2hid_protocol_dissector(payload_buffer, pinfo, tree)
    end
end

function switch2hidble_protocol.dissector(buffer, pinfo, tree)
    length = buffer:len()
    if length == 0 then return end

    pinfo.cols.protocol = switch2hid_protocol.name
    local payload_buffer = buffer:bytes():tvb("Payload")

    local subtree = tree:add(switch2hid_protocol, payload_buffer(), "Switch2 HID BLE Data")

    -- TODO: Find how to spot which format is using
    parse_wireless_input_report(payload_buffer, pinfo, subtree)
    --parse_wireless_input_report2(payload_buffer, pinfo, subtree)
end

function switch2hidble_protocol2.dissector(buffer, pinfo, tree)
    length = buffer:len()
    if length == 0 then return end

    pinfo.cols.protocol = switch2hid_protocol.name
    local payload_buffer = buffer:bytes():tvb("Payload")

    local subtree = tree:add(switch2hid_protocol, payload_buffer(), "Switch2 HID BLE Data")

    parse_wireless_input_reportA(payload_buffer, pinfo, subtree)
end

--register_postdissector(switch2hid_protocol)
DissectorTable.get("usb.interrupt"):add(0x03, switch2hid_protocol)
DissectorTable.get("btatt.handle"):add(0x000a, switch2hidble_protocol2) -- BLE Common input report
DissectorTable.get("btatt.handle"):add(0x000e, switch2hidble_protocol) -- BLE Simple input report
