cmn = require "common"

switch2_protocol = Proto("switch2",  "Nintendo Switch 2 controller Protocol")

local reportType = ProtoField.uint8("switch2.reportType", "ReportType", base.HEX)
local reportMode = ProtoField.uint8("switch2.reportMode", "ReportMode", base.HEX)
local command =    ProtoField.uint8("switch2.command",    "Command",    base.HEX)
local result =     ProtoField.uint8("switch2.result",     "Result",     base.HEX)
-- spi
local spiLength =  ProtoField.uint8("switch2.spiLength",  "SpiLength",  base.DEC)
local spiCommand = ProtoField.uint8("switch2.spiCommand", "SpiCommand", base.HEX)
local spiAddress = ProtoField.uint8("switch2.spiAddress", "SpiAddress", base.HEX)
local spiData =    ProtoField.bytes("switch2.spiData",    "SpiData",    base.NONE)
-- led
local ledPattern = ProtoField.uint8("switch2.ledPattern", "LedPattern", base.HEX)
-- data
local dataLength = ProtoField.uint8("switch2.dataLength", "DataLength", base.DEC)
local dataData =   ProtoField.bytes("switch2.dataData",   "DataData",   base.NONE)
-- firmware
local firmwareLength = ProtoField.uint8("switch2.firmwareLength", "FirmwareLength", base.DEC)
local firmwareData =   ProtoField.bytes("switch2.firmwareData",   "FirmwareData",   base.NONE)
-- mcu
local mcuReadType =   ProtoField.uint8("switch2.mcuReadType",    "McuReadType",   base.DEC)
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
local mcuDataOffset = ProtoField.uint8("switch2.mcuDataOffset",  "McuDataOffset", base.HEX)
local mcuDataLength = ProtoField.uint8("switch2.mcuDataLength",  "McuDataLength", base.HEX)
local mcuDataType =   ProtoField.uint8("switch2.mcuDataType",    "McuDataType",   base.HEX)
local mcuData =       ProtoField.bytes("switch2.mcuData",        "McuData",       base.NONE)

-- Hack to read mcu buffer
local mcuDataBuffer = {}
local mcuDataBufferSize = 0

switch2_protocol.fields = {reportType, reportMode, command, result, spiLength, spiCommand, spiAddress, spiData, ledPattern,
                           dataLength, dataData, firmwareLength, firmwareData, mcuReadType, mcuBlockCount, mcuReadBlock,
                           mcuTagType, mcuUID, mcuUIDLength, mcuUnk, mcuDataOffset, mcuDataLength, mcuDataType, mcuData,
                           mcuBlock0Data, mcuBlock1Data, mcuBlock2Data, mcuBlock3Data, mcuWriteBlock}

-- Input report mode
local Reply =   0x01 -- Reply from controller
local Request = 0x91 -- Request from console

-- Command report types
local McuReport =          0x01 -- MCU commands. NFC read/write
local SpiReport =          0x02 -- SPI commands. Read
local InitReport =         0x03 -- Unknown, First command with console serial
local Report07 =           0x07 -- Unknown
local PlayerLightsReport = 0x09 -- Controller leds. Write
local Report0a =           0x0a -- Unknown
local Report10 =           0x10 -- Unknown
local ImuReport =          0x0c -- IMU commands. Enable/Disable
local FirmwareReport =     0x0d -- Firmware update commands
local DataReport =         0x15 -- Unknown data transfers
local Report16 =           0x16 -- Unknown
local Report18 =           0x18 -- Unknown

-- PID/VID
local VidNintendo =         0x057e
local PidJoyconLeft =       0x2006
local PidJoyconRight =      0x2007
local PidExtGripDfu =       0x2008
local PidProController =    0x2009
local PidExtGrip =          0x200E
local PidProControllerDfu = 0x200F
local PidJoyconRight =      0x2017
local PidProController =    0x2018
local PidProController =    0x2019
local PidProController =    0x201A
local PidProController =    0x201E
local PidProController =    0x201F
local PidGCController2 =    0x2073
local PidJoycon2Right =     0x2066
local PidJoycon2Left =      0x2067
local PidProController2 =   0x2069
local PidGCController2 =    0x2073

-- SPI addresss, 2MB
local SpiFirmwareA =            0x000000 -- 0x30000+ bytes
local SpiSerialNumber =         0x013002 -- 0xe bytes
local SpiVendorId =             0x013012 -- 2 bytes
local SpiProductId =            0x013014 -- 2 bytes
local SpiColorA =               0x013019 -- 3 bytes RGB
local SpiColorB =               0x01301c -- 3 bytes RGB
local SpiColorC =               0x01301f -- 3 bytes RGB
local SpiColorD =               0x013022 -- 3 bytes RGB
local SpiUnknown13040 =         0x013040 -- 0x10 bytes
local SpiUnknown13060 =         0x013060 -- 0x20 bytes
local SpiCalibrationA =         0x013080 -- 0x40 bytes
local SpiCalibrationB =         0x0130C0 -- 0x40 bytes
local SpiUnknown13100 =         0x013100 -- 0x18 bytes
local SpiFirmwareB =            0x015000 -- 0x30000+ bytes
local SpiConsoleMac =           0x1fa008 -- 6 bytes
local SpiCalibrationJoystickL = 0x1fc040 -- 0xb bytes, 0xFF.. no calibration
local SpiCalibrationJoystickR = 0x1fc060 -- 0xb bytes, 0xFF.. no calibration

local function parse_result(result_value)
    if result_value == 0xf8 then return " (ACK)" end
    return " (Unknown)"
end

local function parse_spi_address(address)
    if address == SpiFirmwareA then return " (Firmware A)" end
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
    if address == SpiConsoleMac then return " (Console MAC)" end
    if address == SpiCalibrationJoystickL then return " (User Joystick L calibration)" end
    if address == SpiCalibrationJoystickR then return " (User Joystick R calibration)" end
    return " (Unknown)"
end

local function parse_mcu_read_type(read_type)
    if read_type == 19 then return " (Read NTAG)" end
    return " (Unknown)"
end

local function parse_mcu_tag_type(tag_type)
    if tag_type == 0x01 then return " (NTAG 215)" end
    return " (Unknown)"
end

local function parse_mcu_state(buffer, pinfo, tree)
    local data_value = buffer(5,3)

    tree:add_le(mcuData, data_value)

    pinfo.cols.info = "Request MCU state: ->" .. cmn.getBytes(data_value)
    return " (MCU state)"
end

local function parse_mcu_read_device(buffer, pinfo, tree)
    local mcu_read_type_value = buffer(5,1)
    local mcu_read_type_text = parse_mcu_read_type(mcu_read_type_value:le_uint())
    local mcu_unknown_value = buffer(8,1)
    local uid_length_value = buffer(9,1)
    local uid_value = buffer(10,uid_length_value:le_uint())
    local tag_type_value = buffer(17, 1)
    local tag_type_text = parse_mcu_tag_type(tag_type_value:le_uint())
    local block_count_value = buffer(18, 1)
    local blocks_value = buffer(19, block_count_value:le_uint()*2)

    tree:add_le(mcuUnk, mcu_unknown_value)
    tree:add_le(mcuUIDLength, uid_length_value)
    tree:add_le(mcuUID, uid_value)
    tree:add_le(mcuTagType, tag_type_value):append_text(tag_type_text)
    tree:add_le(mcuBlockCount, block_count_value)
    tree:add_le(mcuReadBlock, blocks_value)
    tree:add_le(mcuReadType, mcu_read_type_value):append_text(mcu_read_type_text)

    pinfo.cols.info = "Request MCU read device:"..tag_type_text.." uid" .. cmn.getBytes(uid_value) .. " blocks" .. cmn.getBytes(blocks_value)

    return " (MCU read device)"
end

local function parse_mcu_write_device(buffer, pinfo, tree)
    local buffer_text= cmn.getBytes2(mcuDataBuffer, 0x1c6) .. "00";
    local mcu_buffer = ByteArray.new(buffer_text):tvb("MCU buffer")

    local uid_length_value = mcu_buffer(1,1)
    local uid_value = mcu_buffer(2,uid_length_value:le_uint())
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
    local mcu_unknown_value = buffer(5, 1)
    local data_offset_value = buffer(8, 2)
    local data_length_value = buffer(10, 1)
    local data_value = buffer(12, data_length_value:le_uint())

    tree:add_le(mcuUnk, mcu_unknown_value)
    tree:add_le(mcuDataOffset, data_offset_value)
    tree:add_le(mcuDataLength, data_length_value)
    tree:add_le(mcuData, data_value)

    local offset=data_offset_value:le_uint() + 0
    for i=0,data_length_value:le_uint() - 1 do
        mcuDataBuffer[i+offset] = data_value:bytes():get_index(i)
    end

    if mcuDataBufferSize < data_offset_value:le_uint() + data_length_value:le_uint() then
        mcuDataBufferSize = data_offset_value:le_uint() + data_length_value:le_uint()
    end

    pinfo.cols.info = "Request MCU write buffer: offset 0x".. cmn.hex(data_offset_value:le_uint()).. " ->" .. cmn.getBytes(data_value)
    return " (MCU write buffer)"
end

local function parse_mcu_read_buffer(buffer, pinfo, tree)
    pinfo.cols.info = "Request MCU read buffer: ->".. cmn.getBytes(buffer(5,buffer:len()-5))
    return " (MCU read buffer)"
end


local function parse_mcu_command(buffer, pinfo, tree, command_value)
    local command_text = " (MCU unknown)"

    if command_value:le_uint() == 0x05 then command_text = parse_mcu_state(buffer, pinfo, tree)
    elseif command_value:le_uint() == 0x06 then command_text = parse_mcu_read_device(buffer, pinfo, tree)
    elseif command_value:le_uint() == 0x08 then command_text = parse_mcu_write_device(buffer, pinfo, tree)
    elseif command_value:le_uint() == 0x14 then command_text = parse_mcu_write_buffer(buffer, pinfo, tree)
    elseif command_value:le_uint() == 0x15 then command_text = parse_mcu_read_buffer(buffer, pinfo, tree)
    else pinfo.cols.info = "Request MCU(0x"..command_value..") ->".. cmn.getBytes(buffer(5,buffer:len()-5)) end

    tree:add_le(command, command_value):append_text(command_text)

    return " (MCU)"
end

local function parse_spi_command(buffer, pinfo, tree, command_value)
    local length_value = buffer(8, 1)
    local sub_command_value = buffer(9, 1)
    local address_value = buffer(0xc, 4)
    local address_text = parse_spi_address(address_value:le_uint())
    local command_text = " (Unknown)"
    
    if command_value:le_uint() == 0x4 then
        command_text = " (SPI read)"
        pinfo.cols.info = "Request SPI read: address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x"..length_value
    elseif command_value:le_uint() == 0x5 then
        command_text = " (SPI write)"
        pinfo.cols.info = "Request SPI write: address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x"..length_value .. cmn.getBytes(buffer(0x10,length_value:le_uint()))
        tree:add_le(spiData, buffer(0x10,length_value:le_uint()))
    else pinfo.cols.info = "Request SPI 0x" .. sub_command_value .. ": address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x"..length_value end
    
    tree:add_le(spiLength, length_value)
    tree:add_le(spiCommand, sub_command_value)
    tree:add_le(spiAddress,  address_value):append_text(address_text)
    tree:add_le(command, command_value):append_text(command_text)
    
    return " (SPI)"
end

local function parse_player_lights_command(buffer, pinfo, tree, command_value)
    local command_text = " (Player lights unknown)"
    local led_pattern_value = buffer(8, 1)

    if command_value:le_uint() == 0x07 then
        pinfo.cols.info = "Request Player lights: set led pattern 0x" .. cmn.hex(led_pattern_value:le_uint())
        command_text = " (Player lights set pattern)"
    else
        pinfo.cols.info = "Request Player lights: unknown"
    end

    tree:add_le(ledPattern, led_pattern_value)
    tree:add_le(command, command_value):append_text(command_text)

    return " (Player lights)"
end

local function parse_imu_command(buffer, pinfo, tree, command_value)
    local command_text = " (IMU unknown)"

    if command_value:le_uint() == 0x02 then
        pinfo.cols.info = "Request IMU: Disable motion"
        command_text = " (IMU disable motion)"
    elseif command_value:le_uint() == 0x04 then
        pinfo.cols.info = "Request IMU: Enable motion"
        command_text = " (IMU enable motion)"
    else
        pinfo.cols.info = "Request IMU: unknown"
    end

    tree:add_le(command, command_value):append_text(command_text)

    return " (IMU)"
end

local function parse_firmware_properties(buffer, pinfo, tree)
    local length_value = buffer(0xd, 4)

    tree:add_le(firmwareLength, length_value)

    pinfo.cols.info = "Request Firmware properties: file size 0x"..cmn.hex(length_value:le_uint())
    return " (Firmware properties)"
end

local function parse_firmware_data(buffer, pinfo, tree)
    local length_value = buffer(8, 1)
    local data_value = buffer(0xc, length_value:le_uint())

    tree:add_le(firmwareLength, length_value)
    tree:add_le(firmwareData, data_value)

    pinfo.cols.info = "Request Firmware data: size 0x"..length_value.."->".. cmn.getBytes(data_value)
    return " (Firmware data)"
end

local function parse_firmware_command(buffer, pinfo, tree, command_value)
    local command_text = " (Firmware unknown)"

    if command_value:le_uint() == 0x03 then command_text = parse_firmware_properties(buffer, pinfo, tree)
    elseif command_value:le_uint() == 0x04 then command_text = parse_firmware_data(buffer, pinfo, tree)
    else pinfo.cols.info = "Request Firmware(0x"..command_value..") ->".. cmn.getBytes(buffer(5,buffer:len()-5)) end

    tree:add_le(command, command_value):append_text(command_text)

    return " (Firmware)"
end

local function parse_data_command(buffer, pinfo, tree, command_value)
    local length_value = buffer(5,1)
    local data_value = buffer(8,length_value:le_uint())

    tree:add_le(dataLength, length_value)
    tree:add_le(dataData, data_value)
    tree:add_le(command, command_value):append_text(" (Data unknown)")

    pinfo.cols.info = "Request data(" .. command_value .. "): size 0x" .. length_value .. " ->" .. cmn.getBytes(data_value)
    return " (Data)"
end

local function parse_request(buffer, pinfo, tree)
    local report_type_value = buffer(0,1):le_uint()
    local report_type_text = " (Unknown)"
    local command_value = buffer(3,1)

    if report_type_value == McuReport then report_type_text = parse_mcu_command(buffer, pinfo, tree, command_value)
    elseif report_type_value == SpiReport then report_type_text = parse_spi_command(buffer, pinfo, tree, command_value)
    elseif report_type_value == PlayerLightsReport then report_type_text = parse_player_lights_command(buffer, pinfo, tree, command_value)
    elseif report_type_value == ImuReport then report_type_text = parse_imu_command(buffer, pinfo, tree, command_value)
    elseif report_type_value == FirmwareReport then report_type_text = parse_firmware_command(buffer, pinfo, tree, command_value)
    elseif report_type_value == DataReport then report_type_text = parse_data_command(buffer, pinfo, tree, command_value)
    else
        tree:add_le(command, command_value):append_text(" (0x" .. cmn.hex(report_type_value) .. " unknown)")
        pinfo.cols.info = "Request (0x" .. cmn.hex(report_type_value) .. ", 0x" .. command_value .. ") ->".. cmn.getBytes(buffer(5,buffer:len()-5))
    end

    tree:add_le(reportType,   buffer(0,1)):append_text(report_type_text)

    return " (Request)"
end


local function parse_mcu_state_reply(buffer, pinfo, tree, result_value)
    -- TODO: Include the rest of the state data
    local result_text = parse_result(result_value:le_uint())
    local uid_length_value = buffer(0x10,1)
    local uid_value = buffer(0x11,uid_length_value:le_uint())

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
    local uid_length_value = buffer(7,1)
    local uid_value = buffer(8,uid_length_value:le_uint())
    local block_count_value = buffer(0x33, 1)
    local blocks_value = buffer(0x34, block_count_value:le_uint()*2)
    local block_0_value = buffer(0x3C,0);
    local block_1_value = buffer(0x3C,0);
    local block_2_value = buffer(0x3C,0);
    local block_3_value = buffer(0x3C,0);
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
    local data_value = buffer(0x0b,data_length_value:le_uint())
    local mcu_buffer = data_value:bytes():tvb("MCU buffer")

    if data_type_value:le_uint() == 0x01 then data_type_text = parse_mcu_nfc_data(mcu_buffer, pinfo, tree, result_value)
    else pinfo.cols.info = "Reply   MCU read buffer:" .. result_text .. " ->" .. cmn.getBytes(data_value) end

    tree:add_le(mcuDataLength, data_length_value)
    tree:add_le(mcuData, data_value)
    tree:add_le(mcuDataType, data_type_value):append_text(data_type_text)

    return " (MCU read buffer)"
end

local function parse_mcu_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (MCU unknown)"
    local result_text = parse_result(result_value:le_uint())

    if command_value:le_uint() == 0x05 then command_text = parse_mcu_state_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == 0x06 then command_text = parse_mcu_read_device_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == 0x08 then command_text = parse_mcu_write_device_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == 0x14 then command_text = parse_mcu_write_buffer_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == 0x15 then command_text = parse_mcu_read_buffer_reply(buffer, pinfo, tree, result_value)
    else pinfo.cols.info = "Reply   MCU(0x"..command_value.."):" .. result_text.. " ->" .. cmn.getBytes(buffer(8,buffer:len()-8)) end

    tree:add_le(command, command_value):append_text(command_text)

    return " (MCU)"
end

local function parse_spi_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (SPI unknown)"
    local length_value = buffer(8,1)
    local address_value = buffer(0xc,4)
    local address_text = parse_spi_address(address_value:le_uint())
    local data_value = buffer(0x10,length_value:le_uint())

    tree:add_le(spiLength, length_value)
    tree:add_le(spiAddress,  address_value):append_text(address_text)
    tree:add_le(spiData,  data_value)
    tree:add_le(command, command_value):append_text(command_text)

    pinfo.cols.info = "Reply   SPI: address 0x" .. cmn.hex(address_value:le_uint()) .. " size 0x" .. length_value .. " ->" .. cmn.getBytes(data_value)
    return " (SPI)"
end

local function parse_player_lights_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (Player lights unknown)"
    local result_text = parse_result(result_value:le_uint())

    pinfo.cols.info = "Reply   Player lights:" .. result_text

    if command_value:le_uint() == 0x07 then command_text = " (Player lights set pattern)" end

    tree:add_le(command, command_value):append_text(command_text)

    return " (Player lights)"
end

local function parse_imu_reply(buffer, pinfo, tree, command_value, result_value)
    local command_text = " (IMU unknown)"
    local result_text = parse_result(result_value:le_uint())

    pinfo.cols.info = "Reply   IMU:" .. result_text

    if command_value:le_uint() == 0x02 then command_text = " (IMU disable motion)"
    elseif command_value:le_uint() == 0x04 then command_text = " (IMU enable motion)" end

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

    if command_value:le_uint() == 0x03 then command_text = parse_firmware_properties_reply(buffer, pinfo, tree, result_value)
    elseif command_value:le_uint() == 0x04 then command_text = parse_firmware_data_reply(buffer, pinfo, tree, result_value)
    else pinfo.cols.info = "Reply   Firmware(0x"..command_value.."):" .. result_text end

    tree:add_le(command, command_value):append_text(command_text)

    return " (Firmware)"
end

local function parse_data_reply(buffer, pinfo, tree, command_value, result_value)
    local length = buffer:len()
    local result_text = parse_result(result_value:le_uint())
    local data_value = buffer(8,buffer:len()-8)
    local data_length = data_value:len()

    tree:add_le(command, command_value):append_text(" (Data unknown)")

    pinfo.cols.info = "Reply   data(" .. command_value .. "):".. result_text .. " size 0x".. cmn.hex(data_length) .. " ->" .. cmn.getBytes(data_value)
    return " (Data)"
end

local function parse_reply(buffer, pinfo, tree)
    local report_type_value = buffer(0,1):le_uint()
    local report_type_text = " (Unknown)"
    local command_value = buffer(3,1)
    local result_value = buffer(5,1)
    local result_text = parse_result(result_value:le_uint())

    tree:add_le(result, result_value):append_text(result_text)

    if report_type_value == McuReport then report_type_text = parse_mcu_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value == SpiReport then report_type_text = parse_spi_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value == PlayerLightsReport then report_type_text = parse_player_lights_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value == ImuReport then report_type_text = parse_imu_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value == FirmwareReport then report_type_text = parse_firmware_reply(buffer, pinfo, tree, command_value, result_value)
    elseif report_type_value == DataReport then report_type_text = parse_data_reply(buffer, pinfo, tree, command_value, result_value)
    else
        tree:add_le(command, command_value):append_text(" (0x" .. cmn.hex(report_type_value) .. " unknown)")
        pinfo.cols.info = "Reply   (0x" .. cmn.hex(report_type_value) .. ", 0x" .. command_value .. ") ->" .. result_text .. cmn.getBytes(buffer(8,buffer:len()-8))
    end

    tree:add_le(reportType,   buffer(0,1)):append_text(report_type_text)

    return " (Reply)"
end

function switch2_protocol.dissector(buffer, pinfo, tree)
    if buffer:len() == 0 then return end

    pinfo.cols.protocol = switch2_protocol.name

    local subtree = tree:add(switch2_protocol, buffer(), "Switch2 Protocol Data")

    local report_mode_value = buffer(1,1):le_uint()
    local report_mode_text = " (Unknown)"

    if report_mode_value == Reply then report_mode_text = parse_reply(buffer, pinfo, subtree)
    elseif report_mode_value == Request then report_mode_text = parse_request(buffer, pinfo, subtree) end

    subtree:add_le(reportMode,   buffer(1,1)):append_text(report_mode_text)
    
end

DissectorTable.get("usb.bulk"):add(0xff, switch2_protocol)
--DissectorTable.get("usb.bulk"):add(0xffff, switch2_protocol)
