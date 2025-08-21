cmn = require "common"

switch2_protocol = Proto("switch2", "Nintendo Switch 2 controller Protocol")
switch2ble_protocol = Proto("switch2_ble", "Nintendo Switch 2 controller Protocol BLE")
switch2ble_long_protocol = Proto("switch2_long_ble", "Nintendo Switch 2 controller Protocol BLE long")

-- Input report mode
local ReportMode = {
    Reply =   0x01, -- Reply from controller
    Request = 0x91, -- Request from console
}

local ReportModeNames = {
    [ReportMode.Reply] =   "Reply",
    [ReportMode.Request] = "Request",
}

-- Communication Type
local CommunicationType = {
    Usb = 0x00,
    Ble = 0x01,
}

local CommunicationTypeNames = {
    [CommunicationType.Usb] =   "USB",
    [CommunicationType.Ble] = "BLE",
}

local LongCommandType = {
    Head = 0x01, -- First command
    Body = 0x02, -- Rest of the command data
}

local LongCommandTypeNames = {
    [LongCommandType.Head] = "Head",
    [LongCommandType.Body] = "Body",
}

-- PID/VID
local Vid = {
    Nintendo =         0x057e,
}

local Pid = {
    JoyconLeft =       0x2006,
    JoyconRight =      0x2007,
    ExtGripDfu =       0x2008,
    ProController =    0x2009,
    ExtGrip =          0x200E,
    ProControllerDfu = 0x200F,
    Lucia =            0x2017,
    LuciaDfu =         0x2018,
    Lagon =            0x2019,
    LagonDfu =         0x201A,
    Lager =            0x201E,
    LagerDfu =         0x201F,
    Joycon2Right =     0x2066,
    Joycon2Left =      0x2067,
    ProController2 =   0x2069,
    GCController2 =    0x2073,
}

-- SPI addresss, 2MB
local Spi = {
    InitialFirmware =      0x000000, -- 0x30000+ bytes, SYS
    FirmwareFailSafeAddr = 0x011000, -- 0x4 bytes, Address to last known good firmware 0xFFFFFFFF, 0x015000, 0x075000
    Unknown12000 =         0x012000, -- 0x2 bytes, Unknown 0xBEEF or 0xFFFF
    DeviceInfo =           0x013000, -- 0x40 bytes
    Unknown13000 =         0x013000, -- 0x2 bytes, 0x0100
    SerialNumber =         0x013002, -- 0xe bytes, HBW, HEJ, HEW, HCW, HHW, HHJ
    VendorId =             0x013012, -- 0x2 bytes
    ProductId =            0x013014, -- 0x2 bytes
    Unknown13016 =         0x013016, -- 0x3 bytes, 0x010601
    ColorA =               0x013019, -- 0x3 bytes RGB
    ColorB =               0x01301c, -- 0x3 bytes RGB
    ColorC =               0x01301f, -- 0x3 bytes RGB
    ColorD =               0x013022, -- 0x3 bytes RGB
    Unknown13040 =         0x013040, -- 0x10 bytes, Factory magnetometer calibration?
    Unknown13060 =         0x013060, -- 0x20 bytes, Only present in joycons
    CalibrationA =         0x013080, -- 0x40 bytes
    FactoryCalJoystickA =  0x0130A8, -- 0xb bytes, Left/Right joycon, Left pro/gc controller. center, max, min
    CalibrationB =         0x0130C0, -- 0x40 bytes
    FactoryCalJoystickB =  0x0130E8, -- 0xb bytes, Right pro/gc controller. center, max, min
    Unknown13100 =         0x013100, -- 0x18 bytes, Factory motion calibration?
    Unknown13140 =         0x013140, -- 0x9 bytes, Unknown in joycons. Empty on pro
    FactoryCalTrigger =    0x013140, -- 0x2 bytes, Gc only. Max left, max Right.
    Unknown13e00 =         0x013e00, -- 0x20 bytes, serial like number
    Unknown13e20 =         0x013e20, -- 0x4 bytes, 0x01020202
    Unknown13e30 =         0x013e30, -- 0xa bytes, 0x03020401050206020702
    Unknown13e60 =         0x013e60, -- 0x2 bytes, 0x1100
    Unknown13e80 =         0x013e80, -- 0x9 bytes, 0x0023E9CE0041304B41
    Unknown13efb =         0x013efb, -- 0x4 bytes, 0x0100150C
    FailSafeFirmwareA =    0x015000, -- 0x30000+ bytes, SYS
    FailSafeFirmwareB =    0x075000, -- 0x30000+ bytes, SYS
    FirmwareUnknown =      0x0D5000, -- 0x9000+ bytes
    FirmwareDsph =         0x175000, -- 0x30000+ bytes, DSPH MT3616A0DSP
    PairingInfo =          0x1fa000, -- 0x58 bytes
    PairingEntries =       0x1fa000, -- 0x1 byte. Each entry is 0x28 bytes
    ConsoleMacA =          0x1fa008, -- 0x6 bytes
    LtkA =                 0x1fa01a, -- 0x10 bytes
    ConsoleMacB =          0x1fa030, -- 0x6 bytes
    LtkB =                 0x1fa042, -- 0x10 bytes
    Unknown1fb000 =        0x1fb000, -- 0x8 bytes
    Unknown1fb00e =        0x1fb00e, -- 0x2 bytes
    Unknown1fb042 =        0x1fb042, -- 0x1c bytes
    Unknown1fb070 =        0x1fb070, -- 0x12 bytes
    UserCalMotion =        0x1fc000, -- 0x40 bytes, 0xFF...FF. no calibration
    UserCalJoystickA =     0x1fc040, -- 0xb bytes, 0xFF...FF. no calibration, Left/Right joycon, Left pro/gc controller. magic, center, max, min
    UserCalJoystickB =     0x1fc060, -- 0xb bytes, 0xFF...FF no calibration, Right pro/gc controller. magic, center, max, min
    ShipmentFlagA =        0x1fd000, -- 0x4 bytes, zero if virgin otherwise 0xFFFFFFFF
    ShipmentFlagB =        0x1fd010, -- 0x4 bytes, zero if virgin otherwise 0xFFFFFFFF
    Unknown1fe000 =        0x1fe000, -- 0x100 bytes
    Unknown1ff000 =        0x1ff000, -- 0x58 bytes
    Unknown1ff400 =        0x1ff400, -- 0x490 bytes
}

local SpiNames = {
    [Spi.InitialFirmware] = "Initial Firmware",
    [Spi.FirmwareFailSafeAddr] = "Firmware Fail Safe Addr",
    [Spi.Unknown12000] = "Unknown 0x12000",
    [Spi.DeviceInfo] = "Device Info",
    [Spi.Unknown13000] = "Unknown 0x13000",
    [Spi.SerialNumber] = "SerialNumber",
    [Spi.VendorId] = "Vendor ID",
    [Spi.ProductId] = "Product ID",
    [Spi.Unknown13016] = "Unknown 0x13016",
    [Spi.ColorA] = "Color A",
    [Spi.ColorB] = "Color B",
    [Spi.ColorC] = "Color C",
    [Spi.ColorD] = "Color D",
    [Spi.Unknown13040] = "Unknown 0x13040",
    [Spi.Unknown13060] = "Unknown 0x13060",
    [Spi.CalibrationA] = "Calibration A",
    [Spi.FactoryCalJoystickA] = "Factory Joystick A calibration",
    [Spi.CalibrationB] = "Calibration B",
    [Spi.FactoryCalJoystickB] = "Factory Joystick B calibration",
    [Spi.Unknown13100] = "Unknown 0x13100",
    [Spi.FactoryCalTrigger] = "Factory Trigger calibration0",
    [Spi.Unknown13e00] = "Unknown 0x13e00",
    [Spi.Unknown13e20] = "Unknown 0x13e20",
    [Spi.Unknown13e30] = "Unknown 0x13e30",
    [Spi.Unknown13e60] = "Unknown 0x13e60",
    [Spi.Unknown13e80] = "Unknown 0x13e80",
    [Spi.Unknown13efb] = "Unknown 0x13efb",
    [Spi.FailSafeFirmwareA] = "Fail Safe Firmware A",
    [Spi.FailSafeFirmwareB] = "Fail Safe Firmware B",
    [Spi.FirmwareUnknown] = "Firmware Unknown",
    [Spi.FirmwareDsph] = "Firmware Dsph",
    [Spi.PairingInfo] = "Pairing Info",
    [Spi.PairingEntries] = "Pairing Entries",
    [Spi.ConsoleMacA] = "Console Mac A",
    [Spi.LtkA] = "Ltk A",
    [Spi.ConsoleMacB] = "Console Mac B",
    [Spi.LtkB] = "Ltk B",
    [Spi.Unknown1fb000] = "Unknown 0x1fb000",
    [Spi.Unknown1fb00e] = "Unknown 0x1fb00e",
    [Spi.Unknown1fb042] = "Unknown 0x1fb042",
    [Spi.Unknown1fb070] = "Unknown 0x1fb070",
    [Spi.UserCalMotion] = "User Motion calibration",
    [Spi.UserCalJoystickA] = "User Joystick A calibration",
    [Spi.UserCalJoystickB] = "User Joystick B calibration",
    [Spi.ShipmentFlagA] = "Shipment Flag A",
    [Spi.ShipmentFlagB] = "Shipment Flag B",
    [Spi.Unknown1fe000] = "Unknown 0x1fe000",
    [Spi.Unknown1ff000] = "Unknown 0x1ff000",
    [Spi.Unknown1ff400] = "Unknown 0x1ff400",
}

-- SPI magic values
local SpiCalibrationMagic = 0xb2a1 -- If present user calibration data is set

-- Vibration samples
local VibrationSample = {
    None =        0x00, -- No sound
    Buzz =        0x01, -- 1s buzz
    Find =        0x02, -- Find controller. 1s high pich buzz followed by a beep beep
    Connect =     0x03, -- Connect controller. Button click sound
    Paring =      0x04, -- Pairing sound
    StrongThunk = 0x05, -- Change order?
    Dun =         0x06, -- Screen recording?
    Ding =        0x07, -- Screen recording?
}

local VibrationSampleNames = {
    [VibrationSample.None] =        "None",
    [VibrationSample.Buzz] =        "Buzz",
    [VibrationSample.Find] =        "Find",
    [VibrationSample.Connect] =     "Connect",
    [VibrationSample.Paring] =      "Paring",
    [VibrationSample.StrongThunk] = "StrongThunk",
    [VibrationSample.Dun] =         "Dun",
    [VibrationSample.Ding] =        "Ding",
}

-- Joycon features
local FeatureTypes = {
    Button =    0x01,
    Stick =    0x02,
    Motion =       0x04,
    Unknown08 =    0x08,
    Mouse =        0x10,
    Current =      0x20,
    Unknown40 =    0x40,
    Magnetometer = 0x80,
}

local FeatureTypeNames = {
    [FeatureTypes.Button] =       "Button",
    [FeatureTypes.Stick] =        "Stick",
    [FeatureTypes.Motion] =       "Motion",
    [FeatureTypes.Unknown08] =    "Unknown 0x08",
    [FeatureTypes.Mouse] =        "Mouse",
    [FeatureTypes.Current] =      "Current",
    [FeatureTypes.Unknown40] =    "Unknown 0x40",
    [FeatureTypes.Magnetometer] = "Magnetometer",
}

-- Result codes
local ResultCode = {
    Ack =    0xf8, -- Reply from controller
    AckBle = 0x78, -- Request from console
}

local ResultCodeNames = {
    [ResultCode.Ack] =    "ACK",
    [ResultCode.AckBle] = "ACK",
}

-- Command report types
local ReportType = {
    Mcu =          0x01, -- MCU commands. NFC read/write
    Spi =          0x02, -- SPI commands. Read
    Init =         0x03, -- Unknown, First command with console serial
    Report06 =     0x06, -- Unknown
    Report07 =     0x07, -- Unknown
    Report08 =     0x08, -- Unknown
    PlayerLights = 0x09, -- Controller leds. Write
    Vibration =    0x0a, -- Vibration presets
    Feature =      0x0c, -- Feature commands. Enable motion, mouse, magnetometer and others.
    Firmware =     0x0d, -- Firmware update commands
    Report10 =     0x10, -- Unknown
    Report11 =     0x11, -- Unknown
    Pairing =      0x15, -- Paring data transfers
    Report16 =     0x16, -- Unknown
    Report18 =     0x18, -- Unknown
}

local ReportTypeNames = {
    [ReportType.Mcu] =          "MCU",
    [ReportType.Spi] =          "SPI",
    [ReportType.Init] =         "Init",
    [ReportType.Report07] =     "Report 0x07",
    [ReportType.Report08] =     "Report 0x08",
    [ReportType.PlayerLights] = "Player Lights",
    [ReportType.Vibration] =    "Vibration",
    [ReportType.Feature] =      "Feature",
    [ReportType.Firmware] =     "Firmware",
    [ReportType.Report10] =     "Report 0x10",
    [ReportType.Report11] =     "Report 0x11",
    [ReportType.Pairing] =      "Pairing",
    [ReportType.Report16] =     "Report 0x16",
    [ReportType.Report18] =     "Report 0x18",
}

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

-- SPI commands
local SpiRead = 0x04 -- Read up to 0x40 bytes?
local SpiWrite = 0x05 -- Write up to 0x40 bytes?

-- Init commands
local InitCommand07 = 0x07 -- Unknown. Set final LTK key?
local InitCommand0a = 0x0a -- Unknown
local InitCommand0c = 0x0c -- Unknown
local InitCommand0d = 0x0d -- Unknown. Has console address

-- 06 commands
local Report06Command03 = 0x03 -- Unknown

-- 07 commands
local Report07Command01 = 0x01 -- Unknown

-- 08 commands
local Report08Command01 = 0x01 -- Unknown
local Report08Command02 = 0x02 -- Unknown

-- Player lights commands
local PlayerLightsSetLedPattern = 0x07 -- Set Led pattern

-- Vibration commands
local VibrationPlaySample = 0x02 -- Play different vibration samples
local VibrationCommand08 =  0x08 -- Unknown

-- Feature commands
local FeatureInit =      0x02 -- Init feature
local FeatureFinalize =  0x03 -- Finalize feature
local FeatureEnable =    0x04 -- Enable feature
local FeatureDisable =   0x05 -- Disable feature
local FeatureCommand06 = 0x06 -- Unknown

-- Firmware commands
local FirmwareCommand01 =  0x01 -- Unknown. Init?
local FirmwareCommand02 =  0x02 -- Unknown
local FirmwareProperties = 0x03 -- Contains info like the full FW size
local FirmwareData =       0x04 -- Sends the firmware data in 0x4c chunks
local FirmwareCommand05 =  0x05 -- Unknown
local FirmwareCommand06 =  0x06 -- Unknown
local FirmwareCommand07 =  0x07 -- Unknown. Finalize?

-- 10 commands
local Report10Command01 = 0x01 -- Unknown

-- 11 commands
local Report11Command03 = 0x03 -- Unknown

-- Pairing commands
local PairingSetAddress = 0x01
local PairingCommand02 =  0x02
local PairingCommand03 =  0x03
local PairingCommand04 =  0x04

-- 16 commands
local Report16Command01 = 0x01 -- Unknown

-- 18 commands
local Report18Command01 = 0x01 -- Unknown

local reportType =       ProtoField.uint8("switch2.reportType",       "ReportType",        base.HEX, ReportTypeNames)
local reportMode =       ProtoField.uint8("switch2.reportMode",       "ReportMode",        base.HEX, ReportModeNames)
local comunicationType = ProtoField.uint8("switch2.comunicationType", "comunicationType",  base.HEX, CommunicationTypeNames)
local command =          ProtoField.uint8("switch2.command",          "Command",           base.HEX)
local commandFlags =     ProtoField.uint8("switch2.commandFlags",     "CommandFlags",      base.HEX)
local commandLength =    ProtoField.uint8("switch2.commandLength",    "CommandLength",     base.HEX)
local commandBuffer =    ProtoField.bytes("switch2.commandBuffer",    "CommandBuffer",     base.NONE)
local longCmdType =      ProtoField.uint8("switch2.longCmdType",      "LongCommandType",   base.HEX, LongCommandTypeNames)
local longCmdId =        ProtoField.uint8("switch2.longCmdId",        "LongCommandId",     base.HEX)
local longCmdLength =    ProtoField.uint8("switch2.longCmdLength",    "LongCommandLength", base.HEX)
local result =           ProtoField.uint8("switch2.result",           "Result",            base.HEX, ResultCodeNames)
-- vibration
local vibrationPacketId = ProtoField.uint8("switch2.vibrationPacketId", "VibrationPacketId", base.HEX)
local vibrationEnabled =  ProtoField.bool("switch2.vibrationEnabled",   "VibrationEnabled")
local vibrationSample =  ProtoField.uint8("switch2.vibrationSample",   "vibrationSample", base.HEX, VibrationSampleNames)
-- spi
local spiLength =  ProtoField.uint8("switch2.spiLength",  "SpiLength",  base.DEC)
local spiCommand = ProtoField.uint8("switch2.spiCommand", "SpiCommand", base.HEX)
local spiAddress = ProtoField.uint32("switch2.spiAddress", "SpiAddress", base.HEX, SpiNames)
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
-- feature
local FeatureType = ProtoField.uint8("switch2.FeatureType", "FeatureType", base.HEX)

-- Hack to read mcu buffer
local mcuDataBuffer = {}
local mcuDataBufferSize = 0

switch2_protocol.fields = {reportType, reportMode, command, result, spiLength, spiCommand, spiAddress, spiData, spiMagic,
                           ledPattern, firmwareLength, firmwareData, mcuBlockCount, pairingBuffer,
                           mcuReadBlock, mcuTagType, mcuUID, mcuUIDLength, mcuUnk, mcuDataOffset, mcuDataLength, mcuDataType,
                           mcuBuffer, mcuBlock0Data, mcuBlock1Data, mcuBlock2Data, mcuBlock3Data, mcuWriteBlock, spiMax,
                           spiCenter, spiMin, vibrationPacketId, vibrationEnabled, pairingEntries ,pairingAddress, commandLength,
                           commandBuffer, vibrationSample, longCmdType, longCmdId, longCmdLength, commandFlags, comunicationType,
                           FeatureType}

local function parse_vibration_sample(sample)
    if sample == VibrationSampleBuzz then return " (Buzz)" end
    if sample == VibrationSampleFind then return " (Find)" end
    if sample == VibrationSampleConnect then return " (Connect)" end
    if sample == VibrationSampleParing then return " (Pairing)" end
    if sample == VibrationSampleStrongThunk then return " (Strong Thunk)" end
    if sample == VibrationSampleDun then return " (Dun)" end
    if sample == VibrationSampleDing then return " (Ding)" end
    return " (Unknown)"
end

local function parse_features(feature_value)
    local features_array = {}

    if cmn.isBitSet(feature_value, FeatureTypes.Button) then       table.insert(features_array, FeatureTypeNames[FeatureTypes.Button]) end
    if cmn.isBitSet(feature_value, FeatureTypes.Stick) then        table.insert(features_array, FeatureTypeNames[FeatureTypes.Stick]) end
    if cmn.isBitSet(feature_value, FeatureTypes.Motion) then       table.insert(features_array, FeatureTypeNames[FeatureTypes.Motion]) end
    if cmn.isBitSet(feature_value, FeatureTypes.Unknown08) then    table.insert(features_array, FeatureTypeNames[FeatureTypes.Unknown08]) end
    if cmn.isBitSet(feature_value, FeatureTypes.Mouse) then        table.insert(features_array, FeatureTypeNames[FeatureTypes.Mouse]) end
    if cmn.isBitSet(feature_value, FeatureTypes.Current) then      table.insert(features_array, FeatureTypeNames[FeatureTypes.Current]) end
    if cmn.isBitSet(feature_value, FeatureTypes.Unknown40) then    table.insert(features_array, FeatureTypeNames[FeatureTypes.Unknown40]) end
    if cmn.isBitSet(feature_value, FeatureTypes.Magnetometer) then table.insert(features_array, FeatureTypeNames[FeatureTypes.Magnetometer]) end

    local features_text = " (none)"

    if #features_array ~= 0 then
        features_text = " (" .. table.concat(features_array, ", ") .. ")"
    end

    return features_text
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
    tree:add_le(spiAddress, address_value)
    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_init_0d(buffer, pinfo, tree)
    local entries_value = buffer(0,1)
    local address_text = ""

    tree:add_le(pairingEntries, entries_value)

    for i=0,entries_value:le_uint() -1 do
        local address_value = buffer(2+(i*6),6)
        address_text = address_text .. cmn.getBytes(address_value)
        tree:add_le(pairingAddress, address_value)
    end

    pinfo.cols.info = "Request Init(0x0d): address"..address_text

    return " (Init 0x0d)"
end

local function parse_init_command(buffer, pinfo, tree, command_value)
    local command_text = " (Init unknown)"

    if     command_value:le_uint() == InitCommand0d then command_text = parse_init_0d(buffer, pinfo, tree)
    else pinfo.cols.info = "Request Init(0x"..command_value..") ->".. cmn.getBytes(buffer) end

    tree:add_le(command, command_value):append_text(command_text)
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
end

local function parse_vibration_play_sample(buffer, pinfo, tree)
    local vibration_sample_value = buffer(0, 1)

    pinfo.cols.info = "Request Vibration Play sample: " .. VibrationSampleNames[vibration_sample_value:le_uint()]

    tree:add_le(vibrationSample, vibration_sample_value)

    return " (Vibration Play sample)"
end

local function parse_vibration_command(buffer, pinfo, tree, command_value, command_length_value)
    local command_text = " (Vibration unknown)"

    if command_value:le_uint() == VibrationPlaySample then command_text = parse_vibration_play_sample(buffer, pinfo, tree)
    else pinfo.cols.info = "Request Vibration(0x"..command_value..")=" .. cmn.getBytes(buffer) end

    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_feature_command(buffer, pinfo, tree, command_value, command_length_value)
    local feature_value = buffer(0, 1)
    local command_text = " (Feature unknown)"
    local feature_text = parse_features(feature_value:le_uint())

    tree:add_le(FeatureType, feature_value):append_text(feature_text)

    if command_value:le_uint() == FeatureInit then
        pinfo.cols.info = "Request Feature Init: " .. feature_text
        command_text = " (Feature Init)"
    elseif command_value:le_uint() == FeatureFinalize then
        pinfo.cols.info = "Request Feature Finalize: " .. feature_text
        command_text = " (Feature Finalize)"
    elseif command_value:le_uint() == FeatureEnable then
        pinfo.cols.info = "Request Feature Enable: " .. feature_text
        command_text = " (Feature Enable)"
    elseif command_value:le_uint() == FeatureDisable then
        pinfo.cols.info = "Request Feature Disable: " .. feature_text
        command_text = " (Feature Disable)"
    else
        pinfo.cols.info = "Request Feature(0x"..command_value..") ->".. cmn.getBytes(buffer)
    end

    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_firmware_properties(buffer, pinfo, tree)
    local length_value = buffer(5, 4)

    tree:add_le(firmwareLength, length_value)

    pinfo.cols.info = "Request Firmware properties: file size 0x"..cmn.hex(length_value:le_uint())
    return " (Firmware properties)"
end

local function parse_firmware_data(buffer, pinfo, tree)
    local length_value = buffer(0, 2)
    local data_value = cmn.getSubBuffer(buffer,0x4,length_value:le_uint())

    tree:add_le(firmwareLength, length_value)
    tree:add_le(firmwareData, data_value)

    pinfo.cols.info = "Request Firmware data: size "..length_value:le_uint().." ->".. cmn.getBytes(data_value)
    return " (Firmware data)"
end

local function parse_firmware_command(buffer, pinfo, tree, command_value, command_length_value)
    local command_text = " (Firmware unknown)"

    if     command_value:le_uint() == FirmwareProperties then command_text = parse_firmware_properties(buffer, pinfo, tree)
    elseif command_value:le_uint() == FirmwareData then       command_text = parse_firmware_data(buffer, pinfo, tree)
    else pinfo.cols.info = "Request Firmware(0x"..command_value..") ->".. cmn.getBytes(buffer) end

    tree:add_le(command, command_value):append_text(command_text)
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
end

local function parse_request(buffer, pinfo, tree)
    local report_type_value = buffer(0,1)
    local comunication_type_value = buffer(2,1)
    local command_value = buffer(3,1)
    local command_flags_value = buffer(4,1)
    local command_length_value = buffer(5,1)

    local command_buffer_value = cmn.getSubBuffer(buffer,8,command_length_value:le_uint())
    if command_flags_value:le_uint() == 0x10 then
        command_buffer_value = cmn.getSubBuffer(buffer,8,buffer:len()-8)
    end
    local command_buffer = command_buffer_value:bytes():tvb("Command buffer")

    tree:add_le(reportType, report_type_value)
    tree:add_le(comunicationType, comunication_type_value)
    tree:add_le(commandFlags, command_flags_value)
    tree:add_le(commandLength, command_length_value)
    tree:add_le(commandBuffer, command_buffer_value)

    if     report_type_value:le_uint() == ReportType.Mcu then          parse_mcu_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == ReportType.Spi then          parse_spi_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == ReportType.Init then         parse_init_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == ReportType.PlayerLights then parse_player_lights_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == ReportType.Vibration then    parse_vibration_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == ReportType.Feature then      parse_feature_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == ReportType.Firmware then     parse_firmware_command(command_buffer, pinfo, tree, command_value, command_length_value)
    elseif report_type_value:le_uint() == ReportType.Pairing then      parse_paring_command(command_buffer, pinfo, tree, command_value, command_length_value)
    else
        tree:add_le(command, command_value):append_text(" (0x" .. cmn.hex(report_type_value:le_uint()) .. " unknown)")
        pinfo.cols.info = "Request (0x" .. cmn.hex(report_type_value:le_uint()) .. ", 0x" .. command_value .. ") ->".. cmn.getBytes(command_buffer)
    end

end


local function parse_mcu_state_reply(buffer, pinfo, tree, result_value)
    -- TODO: Include the rest of the state data
    local result_text = ResultCodeNames[result_value:le_uint()]
    local uid_length_value = buffer(0x10, 1)
    local uid_value = buffer(0x11, uid_length_value:le_uint())

    tree:add_le(mcuUIDLength, uid_length_value)
    tree:add_le(mcuUID, uid_value)

    pinfo.cols.info = "Reply   MCU state: " .. result_text.." uid".. cmn.getBytes(uid_value)
    return " (MCU state)"
end

local function parse_mcu_read_device_reply(buffer, pinfo, tree, result_value)
    local result_text = ResultCodeNames[result_value:le_uint()]

    pinfo.cols.info = "Reply   MCU read device: " .. result_text

    return " (MCU read device)"
end

local function parse_mcu_write_device_reply(buffer, pinfo, tree, result_value)
    local result_text = ResultCodeNames[result_value:le_uint()]

    pinfo.cols.info = "Reply   MCU write device: " .. result_text

    return " (MCU write device)"
end

local function parse_mcu_write_buffer_reply(buffer, pinfo, tree, result_value)
    local result_text = ResultCodeNames[result_value:le_uint()]

    pinfo.cols.info = "Reply   MCU write buffer: " .. result_text

    return " (MCU write buffer)"
end

local function parse_mcu_nfc_data(buffer, pinfo, tree, result_value)
    -- TODO: Include the rest of the state data
    local result_text = ResultCodeNames[result_value:le_uint()]
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

    pinfo.cols.info = "Reply   MCU read nfc data: " .. result_text.." uid".. cmn.getBytes(uid_value) .. " blocks " .. cmn.getBytes(blocks_value) .. " ->" ..
                      cmn.getBytes(block_0_value) .. cmn.getBytes(block_1_value) .. cmn.getBytes(block_2_value) .. cmn.getBytes(block_3_value)

    return " (NFC)"
end

local function parse_mcu_read_buffer_reply(buffer, pinfo, tree, result_value)
    local result_text = ResultCodeNames[result_value:le_uint()]
    local data_type_value = buffer(8, 1)
    local data_type_text = " (Unknown)"
    local data_length_value = buffer(9, 2)
    local buffer_value = buffer(0x0b, data_length_value:le_uint())
    local mcu_buffer = buffer_value:bytes():tvb("MCU buffer")

    if data_type_value:le_uint() == 0x01 then data_type_text = parse_mcu_nfc_data(mcu_buffer, pinfo, tree, result_value)
    else pinfo.cols.info = "Reply   MCU read buffer: " .. result_text .. " ->" .. cmn.getBytes(buffer_value) end

    tree:add_le(mcuDataLength, data_length_value)
    tree:add_le(mcuBuffer, buffer_value)
    tree:add_le(mcuDataType, data_type_value):append_text(data_type_text)

    return " (MCU read buffer)"
end

local function parse_mcu_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (MCU unknown)"
    local result_text = ResultCodeNames[result_value:le_uint()]

    if command_value:le_uint() == McuState then command_text = parse_mcu_state_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == McuReadDevice then command_text = parse_mcu_read_device_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == McuWireDevice then command_text = parse_mcu_write_device_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == McuReadBuffer then command_text = parse_mcu_write_buffer_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == McuWriteBuffer then command_text = parse_mcu_read_buffer_reply(buffer, pinfo, tree, result_value)
    else pinfo.cols.info = "Reply   MCU(0x"..command_value.."): " .. result_text.. " ->" .. cmn.getBytes(buffer(8,buffer:len()-8)) end

    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_spi_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (SPI unknown)"
    local length_value = buffer(8, 1)
    local address_value = buffer(0xc, 4)
    local data_value = buffer(0x10, length_value:le_uint())

    tree:add_le(spiLength, length_value)
    tree:add_le(spiAddress, address_value)

    if command_value:le_uint() == SpiRead then
        command_text = " (SPI Read)"
        tree:add_le(spiData, data_value)
        pinfo.cols.info = "Reply   SPI read: address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x" .. length_value .. " ->" .. cmn.getBytes(data_value)
    elseif command_value:le_uint() == SpiWrite then
        command_text = " (SPI Write)"
        pinfo.cols.info = "Reply   SPI write: address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x" .. length_value
    end

    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_init_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (Init unknown)"
    local result_text = ResultCodeNames[result_value:le_uint()]

    pinfo.cols.info = "Reply   Init(0x"..command_value.."): " .. result_text .. " ->" .. cmn.getBytes(buffer(8, buffer:len()-8))

    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_player_lights_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (Player lights unknown)"
    local result_text = ResultCodeNames[result_value:le_uint()]

    pinfo.cols.info = "Reply   Player lights: " .. result_text

    if command_value:le_uint() == PlayerLightsSetLedPattern then command_text = " (Player lights set pattern)" end

    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_vibration_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (Vibration unknown)"
    local result_text = ResultCodeNames[result_value:le_uint()]

    pinfo.cols.info = "Reply   Vibration(0x"..command_value.."): " .. result_text

    if command_value:le_uint() == VibrationPlaySample then
        pinfo.cols.info = "Reply   Vibration Play sample: " .. result_text
        command_text = " (Vibration Play sample)"
    end

    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_feature_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (Feature unknown)"
    local result_text = ResultCodeNames[result_value:le_uint()]

    pinfo.cols.info = "Reply   Feature(0x"..command_value.."): " .. result_text .. " ->" .. cmn.getBytes(buffer(8, buffer:len()-8))

    if     command_value:le_uint() == FeatureInit then command_text = " (Feature Initialize)"
    elseif command_value:le_uint() == FeatureFinalize then command_text = " (Feature Finalize)"
    elseif command_value:le_uint() == FeatureEnable then command_text = " (Feature Enable)"
    elseif command_value:le_uint() == FeatureDisable then  command_text = " (Feature Disable)" end

    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_firmware_properties_reply(buffer, pinfo, tree, result_value)
    local result_text = ResultCodeNames[result_value:le_uint()]

    pinfo.cols.info = "Reply   Firmware data: " .. result_text
    return " (Firmware properties)"
end

local function parse_firmware_data_reply(buffer, pinfo, tree, result_value)
    local result_text = ResultCodeNames[result_value:le_uint()]

    pinfo.cols.info = "Reply   Firmware data: " .. result_text
    return " (Firmware data)"
end

local function parse_firmware_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (Firmware unknown)"
    local result_text = ResultCodeNames[result_value:le_uint()]

    if     command_value:le_uint() == FirmwareProperties then command_text = parse_firmware_properties_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == FirmwareData then       command_text = parse_firmware_data_reply(buffer, pinfo, tree, result_value)
    else pinfo.cols.info = "Reply   Firmware(0x"..command_value.."): " .. result_text end

    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_set_address_reply(buffer, pinfo, tree, result_value)
    local result_text = ResultCodeNames[result_value:le_uint()]
    local address_value = buffer(3,6)

    tree:add_le(pairingAddress, address_value)

    pinfo.cols.info = "Reply   Pairing set address: " .. result_text .. " address" .. cmn.getBytes(address_value)
    return " (Firmware data)"
end

local function parse_pairing_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (Pairing unknown)"
    local result_text = ResultCodeNames[result_value:le_uint()]
    local buffer_value = buffer(8, buffer:len()-8)
    local buffer_length = buffer_value:len()
    local pairing_buffer = buffer_value:bytes():tvb("Pairing buffer")

    tree:add_le(pairingBuffer, buffer_value)

    if     command_value:le_uint() == PairingSetAddress then command_text = parse_set_address_reply(pairing_buffer, pinfo, tree, result_value)
    else pinfo.cols.info = "Reply   Pairing(" .. command_value .. "): ".. result_text .. " size 0x".. cmn.hex(buffer_length) .. " ->" .. cmn.getBytes(pairing_buffer) end

    tree:add_le(command, command_value):append_text(command_text)
end

local function parse_reply(buffer, pinfo, tree)
    local report_type_value =       buffer(0, 1)
    local comunication_type_value = buffer(2,1)
    local command_value =           buffer(3, 1)
    local result_value =            buffer(5, 1)

    tree:add_le(reportType, report_type_value)
    tree:add_le(comunicationType, comunication_type_value)
    tree:add_le(result, result_value)

    if     report_type_value:le_uint() == ReportType.Mcu then          parse_mcu_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == ReportType.Spi then          parse_spi_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == ReportType.Init then         parse_init_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == ReportType.PlayerLights then parse_player_lights_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == ReportType.Vibration then    parse_vibration_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == ReportType.Feature then      parse_feature_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == ReportType.Firmware then     parse_firmware_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value:le_uint() == ReportType.Pairing then      parse_pairing_reply(buffer, pinfo, tree, command_value, result_value)
    else
        local result_text = ResultCodeNames[result_value:le_uint()]
        tree:add_le(command, command_value):append_text(" (0x" .. cmn.hex(report_type_value:le_uint()) .. " unknown)")
        pinfo.cols.info = "Reply   (0x" .. cmn.hex(report_type_value:le_uint()) .. ", 0x" .. command_value .. ") -> " .. result_text .. cmn.getBytes(buffer(8, buffer:len()-8))
    end

end

local function parse_vibration(buffer, pinfo, tree)
    local packet_id_A_value = buffer(1, 1)
    local packet_id_B_value = buffer(17, 1)
    local vibration_text="";

    if bit.band(packet_id_A_value:le_uint(), 0xf0) == 0x50 then
        local vibration_enabled_value = buffer(2, 1)
        tree:add_le(vibrationPacketId, packet_id_A_value):append_text(" ("..bit.band(packet_id_A_value:le_uint(), 0xf)..")")
        tree:add_le(vibrationEnabled, vibration_enabled_value)
        vibration_text = bit.band(packet_id_A_value:le_uint(), 0xf) .. cmn.getBytes(buffer(3, 0xc))
    end

    if bit.band(packet_id_B_value:le_uint(), 0xf0) == 0x50 then
        local vibration_enabled_value = buffer(18, 1)
        tree:add_le(vibrationPacketId, packet_id_B_value):append_text(" ("..bit.band(packet_id_B_value:le_uint(), 0xf)..")")
        tree:add_le(vibrationEnabled, vibration_enabled_value)
        vibration_text = vibration_text .. ", " .. bit.band(packet_id_B_value:le_uint(), 0xf) .. cmn.getBytes(buffer(19, 0xc))
    end

    pinfo.cols.info = "Send vibration:" .. vibration_text
end

function switch2_protocol.dissector(buffer, pinfo, tree)
    if buffer:len() == 0 then return end

    pinfo.cols.protocol = switch2_protocol.name

    local subtree = tree:add(switch2_protocol, buffer(), "Switch2 Protocol Data")

    local report_mode_value = buffer(1, 1)

    if     report_mode_value:le_uint() == ReportMode.Reply then   parse_reply(buffer, pinfo, subtree)
    elseif report_mode_value:le_uint() == ReportMode.Request then parse_request(buffer, pinfo, subtree) end

    subtree:add_le(reportMode, report_mode_value)
end

function switch2ble_protocol.dissector(buffer, pinfo, tree)
    if buffer:len() < 8 then return end

    pinfo.cols.protocol = switch2_protocol.name

    local subtree = tree:add(switch2_protocol, buffer(), "Switch2 Protocol BLE Data")
    local payload_buffer = buffer:bytes():tvb("Payload")

    local report_mode_value = payload_buffer(1, 1)
    local report_mode_text = " (Unknown)"

    if report_mode_value:le_uint() == 0 then
        parse_vibration(payload_buffer, pinfo, subtree)
        if payload_buffer(0x0f, 1):le_uint() == ReportMode.Reply then
            local command_buffer = payload_buffer(0x0e, payload_buffer:len() - 0x0e):bytes():tvb("Command payload")
            report_mode_value = payload_buffer(0x0f, 1)
            report_mode_text = " (Pro Vibration + Reply)"
            parse_reply(command_buffer, pinfo, subtree)
        elseif payload_buffer(0x12, 1):le_uint() == ReportMode.Request then
            local command_buffer = payload_buffer(0x11, payload_buffer:len() - 0x11):bytes():tvb("Command payload")
            report_mode_value = payload_buffer(0x12, 1)
            report_mode_text = " (GC Vibration + Request)"
            parse_request(command_buffer, pinfo, subtree)
        elseif payload_buffer(0x22, 1):le_uint() == ReportMode.Request then
            local command_buffer = payload_buffer(0x21, payload_buffer:len() - 0x21):bytes():tvb("Command payload")
            report_mode_value = payload_buffer(0x21, 1)
            report_mode_text = " (Vibration + Request)"
            parse_request(command_buffer, pinfo, subtree)
        else report_mode_text = " (Vibration)" end
    elseif bit.band(report_mode_value:le_uint(), 0xf0) == 0x50 then
        parse_vibration(payload_buffer, pinfo, subtree)
        report_mode_text = " (Vibration)"
    elseif report_mode_value:le_uint() == ReportMode.Reply then
        parse_reply(payload_buffer, pinfo, subtree)
        report_mode_text = ""
    elseif report_mode_value:le_uint() == ReportMode.Request then
        parse_request(payload_buffer, pinfo, subtree)
        report_mode_text = ""
    end

    subtree:add_le(reportMode,report_mode_value):append_text(report_mode_text)
end

function switch2ble_long_protocol.dissector(buffer, pinfo, tree)
    if buffer:len() < 8 then return end

    pinfo.cols.protocol = switch2_protocol.name

    local subtree = tree:add(switch2_protocol, buffer(), "Switch2 Protocol BLE Data")

    local long_command_buffer = buffer:bytes():tvb("LongCommand")
    local long_cmd_type_value = long_command_buffer(0,1)
    local long_cmd_id_value = long_command_buffer(1,1)
    local long_cmd_length_value = long_command_buffer(2,1)

    local payload_buffer = long_command_buffer(4,long_cmd_length_value:le_uint()):bytes():tvb("Payload")
    local report_mode_value = payload_buffer(1, 1)

    subtree:add_le(longCmdType,long_cmd_type_value)
    subtree:add_le(longCmdId,long_cmd_id_value)
    subtree:add_le(longCmdLength,long_cmd_length_value)

    if long_cmd_type_value:le_uint() == LongCommandType.Head then
        if report_mode_value:le_uint() == ReportMode.Reply then parse_reply(payload_buffer, pinfo, subtree)
        elseif report_mode_value:le_uint() == ReportMode.Request then parse_request(payload_buffer, pinfo, subtree) end
    else pinfo.cols.info = "Long command data(0x"..long_cmd_id_value..") ->" .. cmn.getBytes(payload_buffer) end

    subtree:add_le(reportMode,report_mode_value)
end

DissectorTable.get("usb.bulk"):add(0xff, switch2_protocol)
DissectorTable.get("btatt.handle"):add(0x0012, switch2ble_protocol) -- BLE vibration only
DissectorTable.get("btatt.handle"):add(0x0014, switch2ble_protocol) -- BLE command only
DissectorTable.get("btatt.handle"):add(0x0016, switch2ble_protocol) -- BLE vibration + command
DissectorTable.get("btatt.handle"):add(0x0018, switch2ble_long_protocol) -- BLE long command
DissectorTable.get("btatt.handle"):add(0x001a, switch2ble_protocol) -- BLE reply
DissectorTable.get("btatt.handle"):add(0x001e, switch2ble_protocol) -- BLE reply
