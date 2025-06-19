cmn = require "common"

switch2_protocol = Proto("switch2", "Nintendo Switch 2 controller Protocol")
switch2ble_protocol = Proto("switch2_ble", "Nintendo Switch 2 controller Protocol BLE")

local reportType =    ProtoField.uint8("switch2.reportType",    "ReportType",    base.HEX)
local reportMode =    ProtoField.uint8("switch2.reportMode",    "ReportMode",    base.HEX)
local command =       ProtoField.uint8("switch2.command",       "Command",       base.HEX)
local commandLength = ProtoField.uint8("switch2.commandLength", "CommandLength", base.HEX)
local commandBuffer = ProtoField.bytes("switch2.commandBuffer", "CommandBuffer", base.NONE)
local result =        ProtoField.uint8("switch2.result",        "Result",        base.HEX)
-- rumble
local rumblePacketId = ProtoField.uint8("switch2.rumblePacketId", "RumblePacketId", base.HEX)
local rumbleEnabled =  ProtoField.bool("switch2.rumbleEnabled",   "rumbleEnabled")
-- spi
local spiLength =  ProtoField.uint8("switch2.spiLength",  "SpiLength",  base.DEC)
local spiCommand = ProtoField.uint8("switch2.spiCommand", "SpiCommand", base.HEX)
local spiAddress = ProtoField.uint8("switch2.spiAddress", "SpiAddress", base.HEX)
local spiData =    ProtoField.bytes("switch2.spiData",    "SpiData",    base.NONE)
local spiMagic =   ProtoField.uint16("switch2.spiMagic",  "SpiMagic",   base.HEX)
local spiMax =     ProtoField.uint16("switch2.spiMax",    "SpiMax",     base.HEX)
local spiCenter =  ProtoField.uint16("switch2.spiCenter", "SpiCenter",  base.HEX)
local spiMin =     ProtoField.uint16("switch2.spiMin",    "SpiMin",     base.HEX)
-- led
local ledPattern = ProtoField.uint8("switch2.ledPattern", "LedPattern", base.HEX)
-- pairing
local pairingBuffer =  ProtoField.bytes("switch2.pairingBuffer",  "PairingBuffer",  base.NONE)
local pairingEntries = ProtoField.uint8("switch2.pairingEntries", "PairingEntries", base.DEC)
local pairingAddress = ProtoField.bytes("switch2.pairingAddress", "PairingAddress", base.NONE)
-- firmware
local firmwareLength = ProtoField.uint8("switch2.firmwareLength", "FirmwareLength", base.DEC)
local firmwareData =   ProtoField.bytes("switch2.firmwareData",   "FirmwareData",   base.NONE)
-- mcu
local mcuBlockCount = ProtoField.uint8("switch2.mcuBlockCount",  "McuBlockCount", base.DEC)
local mcuReadBlock =  ProtoField.bytes("switch2.mcuReadBlock",   "McuReadBlock",  base.NONE)
local mcuWriteBlock = ProtoField.string("switch2.mcuWriteBlock", "McuWriteBlock")
local mcuBlock0Data = ProtoField.bytes("switch2.mcuBlock0Data",  "McuBlock0Data", base.NONE)
local mcuBlock1Data = ProtoField.bytes("switch2.mcuBlock1Data",  "McuBlock1Data", base.NONE)
local mcuBlock2Data = ProtoField.bytes("switch2.mcuBlock2Data",  "McuBlock2Data", base.NONE)
local mcuBlock3Data = ProtoField.bytes("switch2.mcuBlock3Data",  "McuBlock3Data", base.NONE)
local mcuTagType =    ProtoField.uint8("switch2.mcuTagType",     "McuTagType",    base.DEC)
local mcuUID =        ProtoField.bytes("switch2.mcuUID",         "McuUID",        base.NONE)
local mcuUIDLength =  ProtoField.uint8("switch2.mcuUIDLength",   "McuUIDLength",  base.DEC)
local mcuUnk =        ProtoField.uint8("switch2.mcuUnk",         "McuUnk",        base.HEX)
local mcuDataOffset = ProtoField.uint16("switch2.mcuDataOffset", "McuDataOffset", base.HEX)
local mcuDataLength = ProtoField.uint16("switch2.mcuDataLength", "McuDataLength", base.HEX)
local mcuDataType =   ProtoField.uint8("switch2.mcuDataType",    "McuDataType",   base.HEX)
local mcuBuffer =     ProtoField.bytes("switch2.mcuBuffer",      "McuBuffer",     base.NONE)

-- Hack to read mcu buffer
local mcuDataBuffer = {}
local mcuDataBufferSize = 0

switch2_protocol.fields = {reportType, reportMode, command, result, spiLength, spiCommand, spiAddress, spiData, spiMagic,
                           ledPattern, firmwareLength, firmwareData, mcuBlockCount, pairingBuffer,
                           mcuReadBlock, mcuTagType, mcuUID, mcuUIDLength, mcuUnk, mcuDataOffset, mcuDataLength, mcuDataType,
                           mcuBuffer, mcuBlock0Data, mcuBlock1Data, mcuBlock2Data, mcuBlock3Data, mcuWriteBlock, spiMax,
                           spiCenter,spiMin,rumblePacketId,rumbleEnabled, pairingEntries ,pairingAddress,commandLength,
                           commandBuffer}

-- Input report mode
local Reply =   0x01 -- Reply from controller
local Request = 0x91 -- Request from console

-- PID/VID
local VidNintendo =         0x057e
local PidJoyconLeft =       0x2006
local PidJoyconRight =      0x2007
local PidExtGripDfu =       0x2008
local PidProController =    0x2009
local PidExtGrip =          0x200E
local PidProControllerDfu = 0x200F
local PidLucia =            0x2017
local PidLuciaDfu =         0x2018
local PidLagon =            0x2019
local PidLagonDfu =         0x201A
local PidLager =            0x201E
local PidLagerDfu =         0x201F
local PidJoycon2Right =     0x2066
local PidJoycon2Left =      0x2067
local PidProController2 =   0x2069
local PidGCController2 =    0x2073

-- SPI addresss, 2MB
local SpiFirmwareA =            0x000000 -- 0x30000+ bytes, SYS
local SpiUnknown11000 =         0x011000 -- 0x4 bytes, update version? 0x00500700
local SpiUnknown12000 =         0x012000 -- 0x2 bytes, Unknown 0xEFBE or 0xFFFF
local SpiDeviceInfo =           0x013000 -- 0x40 bytes
local SpiSerialNumber =         0x013002 -- 0xe bytes, HBW, HEJ, HEW
local SpiVendorId =             0x013012 -- 0x2 bytes
local SpiProductId =            0x013014 -- 0x2 bytes
local SpiColorA =               0x013019 -- 0x3 bytes RGB
local SpiColorB =               0x01301c -- 0x3 bytes RGB
local SpiColorC =               0x01301f -- 0x3 bytes RGB
local SpiColorD =               0x013022 -- 0x3 bytes RGB
local SpiUnknown13040 =         0x013040 -- 0x10 bytes
local SpiUnknown13060 =         0x013060 -- 0x20 bytes
local SpiCalibrationA =         0x013080 -- 0x40 bytes
local SpiCalibrationB =         0x0130C0 -- 0x40 bytes
local SpiUnknown13100 =         0x013100 -- 0x18 bytes
local SpiUnknown13140 =         0x013140 -- 0x9 bytes
local SpiUnknown13e00 =         0x013e00 -- 0x20 bytes, serial like number
local SpiUnknown13e20 =         0x013e20 -- 0x4 bytes
local SpiUnknown13e30 =         0x013e30 -- 0xa bytes
local SpiUnknown13e60 =         0x013e60 -- 0x2 bytes
local SpiUnknown13e80 =         0x013e80 -- 0x9 bytes
local SpiUnknown13efb =         0x013efb -- 0x4 bytes
local SpiFirmwareB =            0x015000 -- 0x30000+ bytes, SYS
local SpiFirmwareC =            0x075000 -- 0x30000+ bytes, SYS
local SpiFirmwareD =            0x175000 -- 0x30000+ bytes, DSPH MT3616A0DSP
local SpiPairingInfo =          0x1fa000 -- 0x58 bytes
local SpiPairingEntries =       0x1fa000 -- 0x1 byte. Each entry is 0x1c bytes
local SpiConsoleMacA =          0x1fa008 -- 0x6 bytes
local SpiLtkA =                 0x1fa01a -- 0x10 bytes
local SpiConsoleMacB =          0x1fa030 -- 0x6 bytes
local SpiLtkB =                 0x1fa042 -- 0x10 bytes
local SpiUnknown1fe000 =        0x1fe000 -- 0x100 bytes
local SpiCalibrationMotion =    0x1fc000 -- 0x40 bytes, 0xFF...FF. no calibration
local SpiCalibrationJoystickL = 0x1fc040 -- 0xb bytes, 0xFF...FF. no calibration
local SpiCalibrationJoystickR = 0x1fc060 -- 0xb bytes, 0xFF...FF no calibration
local SpiShipmentFlagA =        0x1fd000 -- 0x4 bytes, zero if virgin otherwise 0xFFFFFFFF
local SpiShipmentFlagB =        0x1fd010 -- 0x4 bytes, zero if virgin otherwise 0xFFFFFFFF
local SpiUnknown1ff000 =        0x1ff000 -- 0x58 bytes
local SpiUnknown1ff400 =        0x1ff400 -- 0x490 bytes

-- SPI magic values
local SpiCalibrationMagic = 0xb2a1 -- If present user calibration data is set

-- Result codes
local ResultAck = 0xf8
local ResultAckBle = 0x78

-- Command report types
local McuReport =          0x01 -- MCU commands. NFC read/write
local SpiReport =          0x02 -- SPI commands. Read
local InitReport =         0x03 -- Unknown, First command with console serial
local Report07 =           0x07 -- Unknown
local PlayerLightsReport = 0x09 -- Controller leds. Write
local Report0a =           0x0a -- Unknown
local ImuReport =          0x0c -- IMU commands. Enable/Disable
local FirmwareReport =     0x0d -- Firmware update commands
local Report10 =           0x10 -- Unknown
local Report11 =           0x11 -- Unknown
local PairingReport =      0x15 -- Paring data transfers
local Report16 =           0x16 -- Unknown
local Report18 =           0x18 -- Unknown

-- MCU commands
local McuCommand02 =   0x02 -- Unknown
local McuCommand03 =   0x03 -- Unknown
local McuCommand04 =   0x04 -- Unknown
local McuState =       0x05 -- Return the current state of the MCU device
local McuReadDevice =  0x06 -- Send read properties
local McuWireDevice =  0x08 -- Send write properties
local McuCommand0c =   0x0c -- Unknown
local McuReadBuffer =  0x14 -- Read MCU buffer
local McuWriteBuffer = 0x15 -- Write MCU buffer

-- IMU commands
local ImuDisable = 0x02 -- Unknown, no motion output after this command
local ImuEnable =  0x04 -- Unknown, motion output after this command

-- SPI commands
local SpiRead = 0x04 -- Read up to 0x40 bytes?
local SpiWrite = 0x05 -- Write up to 0x40 bytes?

-- Player lights commands
local PlayerLightsSetLedPattern = 0x07 -- Set Led pattern

-- Firmware commands
local FirmwareCommand01 =  0x01 -- Unknown. Init?
local FirmwareCommand02 =  0x02 -- Unknown
local FirmwareProperties = 0x03 -- Contains info like the full FW size
local FirmwareData =       0x04 -- Sends the firmware data in 0x4c chunks
local FirmwareCommand04 =  0x05 -- Unknown
local FirmwareCommand05 =  0x06 -- Unknown. Finalize?

-- Pairing commands
local PairingSetAddress = 0x01
local PairingCommand02 =  0x02
local PairingCommand03 =  0x03
local PairingCommand04 =  0x04

local function parse_result(result_value)
    if result_value == ResultAck then return " (ACK)" end
    if result_value == ResultAckBle then return " (ACK BLE)" end
    return " (Unknown)"
end

local function parse_spi_address(address)
    if address == SpiFirmwareA then return " (Firmware A)" end
    if address == SpiDeviceInfo then return " (Device info)" end
    if address == SpiSerialNumber then return " (Serial Number)" end
    if address == SpiVendorId then return " (Vendor ID)" end
    if address == SpiProductId then return " (Product ID)" end
    if address == SpiColorA then return " (Color A)" end
    if address == SpiColorB then return " (Color B)" end
    if address == SpiColorC then return " (Color C)" end
    if address == SpiColorD then return " (Color D)" end
    if address == SpiCalibrationA then return " (Calibration A)" end
    if address == SpiCalibrationB then return " (Calibration B)" end
    if address == SpiFirmwareB then return " (Firmware B)" end
    if address == SpiPairingInfo then return " (Pairing Info)" end
    if address == SpiConsoleMacA then return " (Console MAC A)" end
    if address == SpiConsoleMacB then return " (Console MAC B)" end
    if address == SpiLtkA then return " (LTK A)" end
    if address == SpiLtkB then return " (LTK B)" end
    if address == SpiCalibrationMotion then return " (User Motion calibration)" end
    if address == SpiCalibrationJoystickL then return " (User Joystick L calibration)" end
    if address == SpiCalibrationJoystickR then return " (User Joystick R calibration)" end
    if address == SpiShipmentFlagA then return " (SpiShipmentFlagA)" end
    if address == SpiShipmentFlagB then return " (SpiShipmentFlagB)" end
    return " (Unknown)"
end


local function parse_mcu_tag_type(tag_type)
    if tag_type == 0x01 then return " (NTAG 215)" end
    return " (Unknown)"
end

local function parse_mcu_state(buffer, pinfo, tree)
    pinfo.cols.info = "Request MCU state: ->" .. cmn.getBytes(buffer)
    return " (MCU state)"
end

local function parse_mcu_read_device(buffer, pinfo, tree)
    local mcu_unknown_value = buffer(0, 1)
    local uid_length_value = buffer(1, 1)
    local uid_value = buffer(2, uid_length_value:le_uint())
    local tag_type_value = buffer(9, 1)
    local tag_type_text = parse_mcu_tag_type(tag_type_value:le_uint())
    local block_count_value = buffer(10, 1)
    local blocks_value = buffer(11, block_count_value:le_uint()*2)

    tree:add_le(mcuUnk, mcu_unknown_value)
    tree:add_le(mcuUIDLength, uid_length_value)
    tree:add_le(mcuUID, uid_value)
    tree:add_le(mcuTagType, tag_type_value):append_text(tag_type_text)
    tree:add_le(mcuBlockCount, block_count_value)
    tree:add_le(mcuReadBlock, blocks_value)

    pinfo.cols.info = "Request MCU read device:"..tag_type_text.." uid" .. cmn.getBytes(uid_value) .. " blocks" .. cmn.getBytes(blocks_value)

    return " (MCU read device)"
end

local function parse_mcu_write_device(buffer, pinfo, tree)
    local buffer_text= cmn.getBytes2(mcuDataBuffer, 0x1c6) .. "00";
    local mcu_buffer = ByteArray.new(buffer_text):tvb("MCU buffer")

    local uid_length_value = mcu_buffer(1, 1)
    local uid_value = mcu_buffer(2, uid_length_value:le_uint())
    local tag_type_value = mcu_buffer(9, 1)
    local tag_type_text = parse_mcu_tag_type(tag_type_value:le_uint())
    local block_count_value = mcu_buffer(0x15, 1)
    local blocks_text=""
    local block_offset =0

    if block_count_value:le_uint() >= 1 then
        local block_number_value = mcu_buffer(0x16+block_offset, 1)
        local block_size = mcu_buffer(0x17+block_offset, 1)
        local block_0_value = mcu_buffer(0x18+block_offset, block_size:le_uint())
        block_offset = block_offset + block_size:le_uint() + 2

        blocks_text = block_number_value .. "-" .. block_size
        tree:add_le(mcuBlock0Data, block_0_value)
    end
    if block_count_value:le_uint() >= 2 then
        local block_number_value = mcu_buffer(0x16+block_offset, 1)
        local block_size = mcu_buffer(0x17+block_offset, 1)
        local block_1_value = mcu_buffer(0x18+block_offset, block_size:le_uint())
        block_offset = block_offset + block_size:le_uint() + 2

        blocks_text = blocks_text .. ", " .. block_number_value .. "-" .. block_size
        tree:add_le(mcuBlock1Data, block_1_value)
    end
    if block_count_value:le_uint() >= 3 then
        local block_number_value = mcu_buffer(0x16+block_offset, 1)
        local block_size = mcu_buffer(0x17+block_offset, 1)
        local block_2_value = mcu_buffer(0x18+block_offset, block_size:le_uint())
        block_offset = block_offset + block_size:le_uint() + 2

        blocks_text = blocks_text .. ", " .. block_number_value .. "-" .. block_size
        tree:add_le(mcuBlock2Data, block_2_value)
    end
    if block_count_value:le_uint() >= 4 then
        local block_number_value = mcu_buffer(0x16+block_offset, 1)
        local block_size = mcu_buffer(0x17+block_offset, 1)
        local block_3_value = mcu_buffer(0x18+block_offset, block_size:le_uint())
        block_offset = block_offset + block_size:le_uint() + 2

        blocks_text = blocks_text .. ", " .. block_number_value .. "-" .. block_size
        tree:add_le(mcuBlock3Data, block_3_value)
    end


    pinfo.cols.info = "Request MCU write device: Buffer size 0x".. cmn.hex(mcuDataBufferSize).. " blocks ".. blocks_text

    tree:add_le(mcuWriteBlock, blocks_text)
    tree:add_le(mcuBlockCount, block_count_value)
    tree:add_le(mcuUIDLength, uid_length_value)
    tree:add_le(mcuUID, uid_value)
    tree:add_le(mcuTagType, tag_type_value):append_text(tag_type_text)
    tree:add_le(mcuDataLength, mcuDataBufferSize)

    mcuDataBufferSize =0;
    --mcuDataBuffer = {};

    return " (MCU write device)"
end

local function parse_mcu_write_buffer(buffer, pinfo, tree)
    local data_offset_value = buffer(0, 2)
    local data_length_value = buffer(2, 2)
    local buffer_value =      buffer(4, data_length_value:le_uint())

    tree:add_le(mcuDataOffset, data_offset_value)
    tree:add_le(mcuDataLength, data_length_value)
    tree:add_le(mcuBuffer, buffer_value)

    local offset=data_offset_value:le_uint() + 0
    for i=0,data_length_value:le_uint() - 1 do
        mcuDataBuffer[i+offset] = buffer_value:bytes():get_index(i)
    end

    if mcuDataBufferSize < data_offset_value:le_uint() + data_length_value:le_uint() then
        mcuDataBufferSize = data_offset_value:le_uint() + data_length_value:le_uint()
    end

    pinfo.cols.info = "Request MCU write buffer: offset 0x".. cmn.hex(data_offset_value:le_uint()).. " ->" .. cmn.getBytes(buffer_value)
    return " (MCU write buffer)"
end

local function parse_mcu_read_buffer(buffer, pinfo, tree)
    pinfo.cols.info = "Request MCU read buffer: ->".. cmn.getBytes(buffer)
    return " (MCU read buffer)"
end

local function parse_mcu_command(buffer, pinfo, tree, command_value, command_length_value)
    local command_text = " (MCU unknown)"

    if     command_value:le_uint() == McuState then       command_text = parse_mcu_state(buffer, pinfo, tree)
    elseif command_value:le_uint() == McuReadDevice then  command_text = parse_mcu_read_device(buffer, pinfo, tree)
    elseif command_value:le_uint() == McuWireDevice then  command_text = parse_mcu_write_device(buffer, pinfo, tree)
    elseif command_value:le_uint() == McuReadBuffer then  command_text = parse_mcu_write_buffer(buffer, pinfo, tree)
    elseif command_value:le_uint() == McuWriteBuffer then command_text = parse_mcu_read_buffer(buffer, pinfo, tree)
    else pinfo.cols.info = "Request MCU(0x"..command_value..") ->".. cmn.getBytes(buffer) end

    tree:add_le(command, command_value):append_text(command_text)

    return " (MCU)"
end

local function parse_stick(stick_value)
    local raw_axis_x = stick_value:get_index(0) + bit.lshift(bit.band(stick_value:get_index(1), 0xF), 8)
    local raw_axis_y = bit.lshift(stick_value:get_index(2), 4) + bit.rshift(stick_value:get_index(1), 4)

    return " (" .. raw_axis_x .. ", " .. raw_axis_y .. ")"
end

local function parse_spi_command(buffer, pinfo, tree, command_value)
    local length_value =      buffer(0, 1)
    local sub_command_value = buffer(1, 1)
    local address_value =     buffer(4, 4)
    local address_text = parse_spi_address(address_value:le_uint())
    local command_text = " (Unknown)"
    -- TODO: Fix SPI parsing
    if command_value:le_uint() == SpiRead then
        command_text = " (SPI read)"
        pinfo.cols.info = "Request SPI read: address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x"..length_value
    elseif command_value:le_uint() == SpiWrite then
        command_text = " (SPI write)"
        pinfo.cols.info = "Request SPI write: address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x"..length_value .. cmn.getBytes(buffer(0x8,length_value:le_uint()))
        tree:add_le(spiData, buffer(0x8,length_value:le_uint()))
        tree:add_le(spiMagic, buffer(0x8,2))
        tree:add_le(spiCenter, buffer(0xa,3)):append_text(parse_stick(buffer(0xa,3):bytes()))
        tree:add_le(spiMax, buffer(0xd,3)):append_text(parse_stick(buffer(0xd,3):bytes()))
        tree:add_le(spiMin, buffer(0x11,3)):append_text(parse_stick(buffer(0x11,3):bytes()))
    else pinfo.cols.info = "Request SPI 0x" .. sub_command_value .. ": address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x"..length_value end

    tree:add_le(spiLength, length_value)
    tree:add_le(spiCommand, sub_command_value)
    tree:add_le(spiAddress, address_value):append_text(address_text)
    tree:add_le(command, command_value):append_text(command_text)
    
    return " (SPI)"
end

local function parse_player_lights_command(buffer, pinfo, tree, command_value, command_length_value)
    local command_text = " (Player lights unknown)"
    local led_pattern_value = buffer(0, 1)

    if command_value:le_uint() == PlayerLightsSetLedPattern then
        pinfo.cols.info = "Request Player lights: set led pattern 0x" .. cmn.hex(led_pattern_value:le_uint())
        command_text = " (Player lights set pattern)"
    else
        pinfo.cols.info = "Request Player lights: unknown"
    end

    tree:add_le(ledPattern, led_pattern_value)
    tree:add_le(command, command_value):append_text(command_text)

    return " (Player lights)"
end

local function parse_imu_command(buffer, pinfo, tree, command_value, command_length_value)
    local command_text = " (IMU unknown)"

    if command_value:le_uint() == ImuDisable then
        pinfo.cols.info = "Request IMU: Disable motion"
        command_text = " (IMU disable motion)"
    elseif command_value:le_uint() == ImuEnable then
        pinfo.cols.info = "Request IMU: Enable motion"
        command_text = " (IMU enable motion)"
    else
        pinfo.cols.info = "Request IMU: unknown"
    end

    tree:add_le(command, command_value):append_text(command_text)

    return " (IMU)"
end

local function parse_firmware_properties(buffer, pinfo, tree)
    local length_value = buffer(5, 4)

    tree:add_le(firmwareLength, length_value)

    pinfo.cols.info = "Request Firmware properties: file size 0x"..cmn.hex(length_value:le_uint())
    return " (Firmware properties)"
end

local function parse_firmware_data(buffer, pinfo, tree)
    local length_value = buffer(0, 1)
    local data_value = buffer(0x4, length_value:le_uint())

    tree:add_le(firmwareLength, length_value)
    tree:add_le(firmwareData, data_value)

    pinfo.cols.info = "Request Firmware data: size 0x"..length_value.."->".. cmn.getBytes(data_value)
    return " (Firmware data)"
end

local function parse_firmware_command(buffer, pinfo, tree, command_value, command_length_value)
    local command_text = " (Firmware unknown)"

    if     command_value:le_uint() == FirmwareProperties then command_text = parse_firmware_properties(buffer, pinfo, tree)
    elseif command_value:le_uint() == FirmwareData then       command_text = parse_firmware_data(buffer, pinfo, tree)
    else pinfo.cols.info = "Request Firmware(0x"..command_value..") ->".. cmn.getBytes(buffer) end

    tree:add_le(command, command_value):append_text(command_text)

    return " (Firmware)"
end

local function parse_pairing_set_address(buffer, pinfo, tree)
    local entries_value = buffer(1,1)
    local address_text = ""

    tree:add_le(pairingEntries, entries_value)

    for i=0,entries_value:le_uint() -1 do
        local address_value = buffer(2+(i*6),6)
        address_text = address_text .. cmn.getBytes(address_value)
        tree:add_le(pairingAddress, address_value)
    end

    pinfo.cols.info = "Request Pairing set address:"..address_text

    return " (Pairing Set Address)"
end

local function parse_paring_command(buffer, pinfo, tree, command_value, command_length_value)
    local command_text = " (Pairing unknown)"

    if command_value:le_uint() == PairingSetAddress then command_text = parse_pairing_set_address(buffer, pinfo, tree)
    else pinfo.cols.info = "Request Pairing(" .. command_value .. "): size 0x" .. command_length_value .. " ->" .. cmn.getBytes(buffer) end

    tree:add_le(command, command_value):append_text(command_text)

    return " (Pairing)"
end

local function parse_request(buffer, pinfo, tree)
    local report_type_value = buffer(0,1)
    local report_type_text = " (Unknown)"
    local command_value = buffer(3,1)
    local command_length_value = buffer(5,1)
    local command_buffer_value = buffer(8,command_length_value:le_uint())
    local command_buffer = command_buffer_value:bytes():tvb("Command buffer")

    tree:add_le(commandLength, command_length_value)
    tree:add_le(commandBuffer, command_buffer_value)

    if     report_type_value:le_uint() == McuReport then report_type_text = parse_mcu_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == SpiReport then report_type_text = parse_spi_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == PlayerLightsReport then report_type_text = parse_player_lights_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == ImuReport then report_type_text = parse_imu_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == FirmwareReport then report_type_text = parse_firmware_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == PairingReport then report_type_text = parse_paring_command(command_buffer, pinfo, tree, command_value, command_length_value)
    else
        tree:add_le(command, command_value):append_text(" (0x" .. cmn.hex(report_type_value:le_uint()) .. " unknown)")
        pinfo.cols.info = "Request (0x" .. cmn.hex(report_type_value:le_uint()) .. ", 0x" .. command_value .. ") ->".. cmn.getBytes(command_buffer)
    end

    tree:add_le(reportType, report_type_value):append_text(report_type_text)

    return " (Request)"
end


local function parse_mcu_state_reply(buffer, pinfo, tree, result_value)
    -- TODO: Include the rest of the state data
    local result_text = parse_result(result_value:le_uint())
    local uid_length_value = buffer(0x10, 1)
    local uid_value = buffer(0x11, uid_length_value:le_uint())

    tree:add_le(mcuUIDLength, uid_length_value)
    tree:add_le(mcuUID, uid_value)

    pinfo.cols.info = "Reply   MCU state:" .. result_text.." uid".. cmn.getBytes(uid_value)
    return " (MCU state)"
end

local function parse_mcu_read_device_reply(buffer, pinfo, tree, result_value)
    local result_text = parse_result(result_value:le_uint())

    pinfo.cols.info = "Reply   MCU read device:" .. result_text

    return " (MCU read device)"
end

local function parse_mcu_write_device_reply(buffer, pinfo, tree, result_value)
    local result_text = parse_result(result_value:le_uint())

    pinfo.cols.info = "Reply   MCU write device:" .. result_text

    return " (MCU write device)"
end

local function parse_mcu_write_buffer_reply(buffer, pinfo, tree, result_value)
    local result_text = parse_result(result_value:le_uint())

    pinfo.cols.info = "Reply   MCU write buffer:" .. result_text

    return " (MCU write buffer)"
end

local function parse_mcu_nfc_data(buffer, pinfo, tree, result_value)
    -- TODO: Include the rest of the state data
    local result_text = parse_result(result_value:le_uint())
    local uid_length_value = buffer(7, 1)
    local uid_value = buffer(8, uid_length_value:le_uint())
    local block_count_value = buffer(0x33, 1)
    local blocks_value = buffer(0x34, block_count_value:le_uint()*2)
    local block_0_value = buffer(0x3C, 0);
    local block_1_value = buffer(0x3C, 0);
    local block_2_value = buffer(0x3C, 0);
    local block_3_value = buffer(0x3C, 0);
    local block_offset = 0

    tree:add_le(mcuUIDLength, uid_length_value)
    tree:add_le(mcuUID, uid_value)
    tree:add_le(mcuBlockCount, block_count_value)
    tree:add_le(mcuReadBlock, blocks_value)

    if block_count_value:le_uint() >= 1 then
        local blocks = blocks_value:bytes()
        local block_size = (blocks:get_index(1)-blocks:get_index(0)+1) * 4
        block_0_value = buffer(0x3C+block_offset, block_size)
        block_offset = block_offset + block_size

        tree:add_le(mcuBlock0Data, block_0_value)
    end
    if block_count_value:le_uint() >= 2 then
        local blocks = blocks_value:bytes()
        local block_size = (blocks:get_index(3)-blocks:get_index(2)+1) * 4
        block_1_value = buffer(0x3C+block_offset, block_size)
        block_offset = block_offset + block_size

        tree:add_le(mcuBlock1Data, block_1_value)
    end
    if block_count_value:le_uint() >= 3 then
        local blocks = blocks_value:bytes()
        local block_size = (blocks:get_index(5)-blocks:get_index(4)+1) * 4
        block_2_value = buffer(0x3C+block_offset, block_size)
        block_offset = block_offset + block_size

        tree:add_le(mcuBlock2Data, block_2_value)
    end
    if block_count_value:le_uint() >= 4 then
        local blocks = blocks_value:bytes()
        local block_size = (blocks:get_index(7)-blocks:get_index(6)+1) * 4
        block_3_value = buffer(0x3C+block_offset, block_size)
        block_offset = block_offset + block_size

        tree:add_le(mcuBlock3Data, block_3_value)
    end

    pinfo.cols.info = "Reply   MCU read nfc data:" .. result_text.." uid".. cmn.getBytes(uid_value) .. " blocks " .. cmn.getBytes(blocks_value) .. " ->" ..
                      cmn.getBytes(block_0_value) .. cmn.getBytes(block_1_value) .. cmn.getBytes(block_2_value) .. cmn.getBytes(block_3_value)

    return " (NFC)"
end

local function parse_mcu_read_buffer_reply(buffer, pinfo, tree, result_value)
    local result_text = parse_result(result_value:le_uint())
    local data_type_value = buffer(8, 1)
    local data_type_text = " (Unknown)"
    local data_length_value = buffer(9, 2)
    local buffer_value = buffer(0x0b, data_length_value:le_uint())
    local mcu_buffer = buffer_value:bytes():tvb("MCU buffer")

    if data_type_value:le_uint() == 0x01 then data_type_text = parse_mcu_nfc_data(mcu_buffer, pinfo, tree, result_value)
    else pinfo.cols.info = "Reply   MCU read buffer:" .. result_text .. " ->" .. cmn.getBytes(buffer_value) end

    tree:add_le(mcuDataLength, data_length_value)
    tree:add_le(mcuBuffer, buffer_value)
    tree:add_le(mcuDataType, data_type_value):append_text(data_type_text)

    return " (MCU read buffer)"
end

local function parse_mcu_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (MCU unknown)"
    local result_text = parse_result(result_value:le_uint())

    if command_value:le_uint() == McuState then command_text = parse_mcu_state_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == McuReadDevice then command_text = parse_mcu_read_device_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == McuWireDevice then command_text = parse_mcu_write_device_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == McuReadBuffer then command_text = parse_mcu_write_buffer_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == McuWriteBuffer then command_text = parse_mcu_read_buffer_reply(buffer, pinfo, tree, result_value)
    else pinfo.cols.info = "Reply   MCU(0x"..command_value.."):" .. result_text.. " ->" .. cmn.getBytes(buffer(8,buffer:len()-8)) end

    tree:add_le(command, command_value):append_text(command_text)

    return " (MCU)"
end

local function parse_spi_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (SPI unknown)"
    local length_value = buffer(8, 1)
    local address_value = buffer(0xc, 4)
    local address_text = parse_spi_address(address_value:le_uint())
    local data_value = buffer(0x10, length_value:le_uint())

    tree:add_le(spiLength, length_value)
    tree:add_le(spiAddress, address_value):append_text(address_text)

    if command_value:le_uint() == SpiRead then
        command_text = " (SPI Read)"
        tree:add_le(spiData, data_value)
        pinfo.cols.info = "Reply   SPI read: address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x" .. length_value .. " ->" .. cmn.getBytes(data_value)
    elseif command_value:le_uint() == SpiWrite then
        command_text = " (SPI Write)"
        pinfo.cols.info = "Reply   SPI write: address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x" .. length_value
    end

    tree:add_le(command, command_value):append_text(command_text)

     return " (SPI)"
end

local function parse_player_lights_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (Player lights unknown)"
    local result_text = parse_result(result_value:le_uint())

    pinfo.cols.info = "Reply   Player lights:" .. result_text

    if command_value:le_uint() == PlayerLightsSetLedPattern then command_text = " (Player lights set pattern)" end

    tree:add_le(command, command_value):append_text(command_text)

    return " (Player lights)"
end

local function parse_imu_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (IMU unknown)"
    local result_text = parse_result(result_value:le_uint())

    pinfo.cols.info = "Reply   IMU:" .. result_text

    if     command_value:le_uint() == ImuDisable then command_text = " (IMU disable motion)"
    elseif command_value:le_uint() == ImuEnable then  command_text = " (IMU enable motion)" end

    tree:add_le(command, command_value):append_text(command_text)

    return " (IMU)"
end

local function parse_firmware_properties_reply(buffer, pinfo, tree, result_value)
    local result_text = parse_result(result_value:le_uint())

    pinfo.cols.info = "Reply   Firmware data:" .. result_text
    return " (Firmware properties)"
end

local function parse_firmware_data_reply(buffer, pinfo, tree, result_value)
    local result_text = parse_result(result_value:le_uint())

    pinfo.cols.info = "Reply   Firmware data:" .. result_text
    return " (Firmware data)"
end

local function parse_firmware_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (Firmware unknown)"
    local result_text = parse_result(result_value:le_uint())

    if     command_value:le_uint() == FirmwareProperties then command_text = parse_firmware_properties_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == FirmwareData then       command_text = parse_firmware_data_reply(buffer, pinfo, tree, result_value)
    else pinfo.cols.info = "Reply   Firmware(0x"..command_value.."):" .. result_text end

    tree:add_le(command, command_value):append_text(command_text)

    return " (Firmware)"
end

local function parse_pairing_reply(buffer, pinfo, tree, command_value, result_value)
    local result_text = parse_result(result_value:le_uint())
    local buffer_value = buffer(8, buffer:len()-8)
    local buffer_length = buffer_value:len()
    local pairing_buffer = buffer_value:bytes():tvb("Pairing buffer")

    tree:add_le(pairingBuffer, buffer_value)
    tree:add_le(command, command_value):append_text(" (Pairing unknown)")

    pinfo.cols.info = "Reply   Pairing(" .. command_value .. "):".. result_text .. " size 0x".. cmn.hex(buffer_length) .. " ->" .. cmn.getBytes(buffer_value)
    return " (Pairing)"
end

local function parse_reply(buffer, pinfo, tree)
    local report_type_value = buffer(0, 1)
    local command_value =     buffer(3, 1)
    local result_value =      buffer(5, 1)

    local report_type_text = " (Unknown)"
    local result_text = parse_result(result_value:le_uint())

    tree:add_le(result, result_value):append_text(result_text)

    if     report_type_value:le_uint() == McuReport then          report_type_text = parse_mcu_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == SpiReport then          report_type_text = parse_spi_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == PlayerLightsReport then report_type_text = parse_player_lights_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == ImuReport then          report_type_text = parse_imu_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == FirmwareReport then     report_type_text = parse_firmware_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == PairingReport then       report_type_text = parse_pairing_reply(buffer, pinfo, tree, command_value, result_value)
    else
        tree:add_le(command, command_value):append_text(" (0x" .. cmn.hex(report_type_value:le_uint()) .. " unknown)")
        pinfo.cols.info = "Reply   (0x" .. cmn.hex(report_type_value:le_uint()) .. ", 0x" .. command_value .. ") ->" .. result_text .. cmn.getBytes(buffer(8, buffer:len()-8))
    end

    tree:add_le(reportType, report_type_value):append_text(report_type_text)

    return " (Reply)"
end

local function parse_rumble(buffer, pinfo, tree)
    local packet_id_A_value = buffer(1, 1)
    local packet_id_B_value = buffer(17, 1)
    local vibration_text="";

    if bit.band(packet_id_A_value:le_uint(), 0xf0) == 0x50 then
        local vibration_enabled_value = buffer(2, 1)
        tree:add_le(rumblePacketId, packet_id_A_value):append_text(" ("..bit.band(packet_id_A_value:le_uint(), 0xf)..")")
        tree:add_le(rumbleEnabled, vibration_enabled_value)
        vibration_text = bit.band(packet_id_A_value:le_uint(), 0xf) .. cmn.getBytes(buffer(3, 0xc))
    end

    if bit.band(packet_id_B_value:le_uint(), 0xf0) == 0x50 then
        local vibration_enabled_value = buffer(18, 1)
        tree:add_le(rumblePacketId, packet_id_B_value):append_text(" ("..bit.band(packet_id_B_value:le_uint(), 0xf)..")")
        tree:add_le(rumbleEnabled, vibration_enabled_value)
        vibration_text = vibration_text .. ", " .. bit.band(packet_id_B_value:le_uint(), 0xf) .. cmn.getBytes(buffer(19, 0xc))
    end

    pinfo.cols.info = "Send vibration:" .. vibration_text
end

function switch2_protocol.dissector(buffer, pinfo, tree)
    if buffer:len() == 0 then return end

    pinfo.cols.protocol = switch2_protocol.name

    local subtree = tree:add(switch2_protocol, buffer(), "Switch2 Protocol Data")

    local report_mode_value = buffer(1, 1)
    local report_mode_text = " (Unknown)"

    if     report_mode_value:le_uint() == Reply then   report_mode_text = parse_reply(buffer, pinfo, subtree)
    elseif report_mode_value:le_uint() == Request then report_mode_text = parse_request(buffer, pinfo, subtree) end

    subtree:add_le(reportMode, report_mode_value):append_text(report_mode_text)
end

function switch2ble_protocol.dissector(buffer, pinfo, tree)
    if buffer:len() == 0 then return end

    pinfo.cols.protocol = switch2_protocol.name

    local subtree = tree:add(switch2_protocol, buffer(), "Switch2 Protocol BLE Data")
    local payload_buffer = buffer:bytes():tvb("Payload")

    local report_mode_value = payload_buffer(1, 1)
    local report_mode_text = " (Unknown)"

    if report_mode_value:le_uint() == 0 then
        parse_rumble(payload_buffer, pinfo, subtree)
        if payload_buffer(0x6, 1):le_uint() == Request then
            local command_buffer = payload_buffer(0x5, payload_buffer:len() - 0x5):bytes():tvb("Command payload")
            report_mode_value = payload_buffer(0x6, 1)
            report_mode_text = " (GC Rumble + Request)"
            parse_request(command_buffer, pinfo, subtree)
        elseif payload_buffer(0x22, 1):le_uint() == Request then
            local command_buffer = payload_buffer(0x21, payload_buffer:len() - 0x21):bytes():tvb("Command payload")
            report_mode_value = payload_buffer(0x21, 1)
            report_mode_text = " (Rumble + Request)"
            parse_request(command_buffer, pinfo, subtree)
        else report_mode_text = " (Rumble)" end
    elseif bit.band(report_mode_value:le_uint(), 0xf0) == 0x50 then
        parse_rumble(payload_buffer, pinfo, subtree)
        report_mode_text = " (Rumble)"
    elseif report_mode_value:le_uint() == Reply then report_mode_text = parse_reply(payload_buffer, pinfo, subtree)
    elseif report_mode_value:le_uint() == Request then report_mode_text = parse_request(payload_buffer, pinfo, subtree) end

    subtree:add_le(reportMode,report_mode_value):append_text(report_mode_text)
end

DissectorTable.get("usb.bulk"):add(0xff, switch2_protocol)
DissectorTable.get("btatt.handle"):add(0x0012, switch2ble_protocol) -- BLE rumble only
DissectorTable.get("btatt.handle"):add(0x0014, switch2ble_protocol) -- BLE command only
DissectorTable.get("btatt.handle"):add(0x0016, switch2ble_protocol) -- BLE rumble + command
DissectorTable.get("btatt.handle"):add(0x001a, switch2ble_protocol) -- BLE reply
