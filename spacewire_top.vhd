--
--  Test of spwstream on Digilent XC3S200 board.
--  60 MHz system clock, 200 MHz receive clock and transmit clock.
--
--  LED 0 = link started
--  LED 1 = link connecting
--  LED 2 = link run
--  LED 3 = link error (sticky until clear button)
--  LED 4 = gotdata
--  LED 5 = off
--  LED 6 = data error (sticky until reset)
--  LED 7 = time code error (sticky until reset)
--
--  Button 0 = reset
--  Button 1 = clear LED 3
--
--  Switch 0 = link autostart
--  Switch 1 = link start
--  Switch 2 = link disable
--  Switch 3 = send data and time codes
--  Switch 4-7 = bits 0-3 of tx bit rate scale factor
--
--  SpaceWire signals on A2 expansion connector:
--    Data In    pos,neg  =  B5,C5  =  pin 19,6
--    Strobe In  pos,neg  =  D6,E6  =  pin 7,4
--    Data Out   pos,neg  =  B6,C6  =  pin 21,8
--    Strobe Out pos,neg  =  D7,E7  =  pin 11,9
--
--  Note: these are not true LVDS signals; they are configured as LVDS25
--  but powered from 3.3V instead of 2.5V, not differentially routed and
--  not properly terminated.
--
--  The SpaceWire port should be looped back to itself with wires from
--  outputs to corresponding inputs.
--

--  The SpaceWire port should be looped back to itself, either directly
--  or via an other SpaceWire device. For a direct loopback, place 4 wires
--  from the output pins to the corresponding input pins. For an indirect
--  loopback, connect the SpaceWire signals to an additional SpaceWire device
--  which is programmed to echo everything it receives (characters, packets,
--  time codes).

LIBRARY ieee;
USE ieee.std_logic_1164.ALL, ieee.numeric_std.ALL;
USE work.spwpkg.ALL;

ENTITY spacewire_top IS
    GENERIC (
        countVal1 : INTEGER := 50e6/4;
        countVal2 : INTEGER := 50e6*2/4;
        countVal3 : INTEGER := 50e6*3/4;
        countVal4 : INTEGER := 50e6
    );
    PORT (
        --Acclerometer ports
        acc_spi_chip_select : OUT STD_LOGIC; -- Accelerometer chip select (negated)
        acc_spi_clk : OUT STD_LOGIC;
        acc_spi_data : INOUT STD_LOGIC; -- no MOSI/MISO, hardware supports only 3-wire SPI
        acc_interrupt : IN STD_LOGIC;

        -- External LCD ports
        LCD_EN : OUT STD_LOGIC;
        LCD_RS : OUT STD_LOGIC;
        LCD_RW : OUT STD_LOGIC;
        LCD_DATA : INOUT STD_LOGIC_VECTOR(7 DOWNTO 0);

        --SpW ports
        clk50 : IN STD_LOGIC;
        btn_reset : IN STD_LOGIC;
        btn_clear : IN STD_LOGIC;
        switch : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        led : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        spw_di : IN STD_LOGIC;
        spw_si : IN STD_LOGIC;
        spw_do : OUT STD_LOGIC;
        spw_so : OUT STD_LOGIC
    );

END ENTITY spacewire_top;

ARCHITECTURE spacewire_top_arch OF spacewire_top IS
    --Accelerometer signals

    --pure output of 12-bit ADC (0 padding in front?)
    --register map p.23: https://www.analog.com/media/en/technical-documentation/data-sheets/adxl345.pdf
    SIGNAL sensor_data   : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL LCD_x : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL LCD_y : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL LCD_z : STD_LOGIC_VECTOR(15 DOWNTO 0);
    -- select accelerometer readout axis (1000 sysclk delay?)
    SIGNAL s_sel_axis : INTEGER RANGE 0 TO 2;
    SIGNAL s_count : STD_LOGIC_VECTOR(26 DOWNTO 0);

    --LCD signals register control
    SIGNAL s_lcd_register_data_in : STD_LOGIC_VECTOR(15 DOWNTO 0);

    -- SpW signals
    -- Clock generation.
    SIGNAL sysclk : STD_LOGIC;

    -- Synchronize buttons
    SIGNAL s_resetbtn : STD_LOGIC := '0';
    SIGNAL s_clearbtn : STD_LOGIC := '0';

    -- Sticky LED
    SIGNAL s_linkerrorled : STD_LOGIC := '0';

    -- Interface signals.
    SIGNAL s_rst : STD_LOGIC := '1';
    SIGNAL s_linkstart : STD_LOGIC := '0';
    SIGNAL s_autostart : STD_LOGIC := '0';
    SIGNAL s_linkdisable : STD_LOGIC := '0';
    SIGNAL s_senddata : STD_LOGIC := '0';
    SIGNAL s_sendtick : STD_LOGIC := '0';
    SIGNAL s_txdivcnt : STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";
    SIGNAL s_linkstarted : STD_LOGIC;
    SIGNAL s_linkconnecting : STD_LOGIC;
    SIGNAL s_linkrun : STD_LOGIC;
    SIGNAL s_linkerror : STD_LOGIC;
    SIGNAL s_gotdata : STD_LOGIC;
    SIGNAL s_dataerror : STD_LOGIC;
    SIGNAL s_tickerror : STD_LOGIC;
    SIGNAL s_spwdi : STD_LOGIC;
    SIGNAL s_spwsi : STD_LOGIC;
    SIGNAL s_spwdo : STD_LOGIC;
    SIGNAL s_spwso : STD_LOGIC;

    SIGNAL s_spwout_x : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL s_spwout_y : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL s_spwout_z : STD_LOGIC_VECTOR(15 DOWNTO 0);

    SIGNAL s_x : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL s_y : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL s_z : STD_LOGIC_VECTOR(15 DOWNTO 0);

    SIGNAL s_to_send_x : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL s_to_send_y : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL s_to_send_z : STD_LOGIC_VECTOR(15 DOWNTO 0);

    -- Accelerometer state machine
    TYPE accel_state_type IS (accel_prepare, accel_read_x, accel_read_y, accel_read_z, accel_latch);

    SIGNAL accel_state : accel_state_type := accel_prepare;

BEGIN
    -- Accelerometer instance
    accelerometer_inst : ENTITY work.accelerometer
        PORT MAP(
            clk => sysclk,
            sel_axis => s_sel_axis,
            rst => btn_reset,
            G_SENSOR_CS_N => acc_spi_chip_select,
            G_SENSOR_INT => acc_interrupt,
            I2C_SCLK => acc_spi_clk,
            G_SENSOR_OUT => sensor_data,
            I2C_SDAT => acc_spi_data
        );

    -- LCD driver instance
    lcd_inst : ENTITY work.spacewire_lcd_driver
        PORT MAP(
            CLOCK_50 => sysclk, -- DE0 CLOCK_50 (50MHz CLK)
            KEY => btn_reset, -- DE0 KEY (button) [reset]
            KEY2 => btn_clear,
            --LED             => led,
            -- External LCD ports
            LCD_EN => LCD_EN,
            LCD_RS => LCD_RS,
            LCD_RW => LCD_RW,
            LCD_DATA => LCD_DATA,
            -- LCD Register control
            data_x => LCD_x,
            data_y => LCD_y,
            data_z => LCD_z
        );

    -- Streamtest instance
    streamtest_inst : ENTITY work.streamtest
        GENERIC MAP(
            sysfreq => 50.0e6,
            txclkfreq => 0.0,
            tickdiv => 20,
            rximpl => impl_generic,
            rxchunk => 1,
            tximpl => impl_generic,
            rxfifosize_bits => 11,
            txfifosize_bits => 11
        )
        PORT MAP(
            clk => sysclk,
            rxclk => '0',
            txclk => '0',
            rst => s_rst,
            linkstart => s_linkstart,
            autostart => s_autostart,
            linkdisable => s_linkdisable,
            senddata => s_senddata,
            sendtick => s_sendtick,
            txdivcnt => s_txdivcnt,
            linkstarted => s_linkstarted,
            linkconnecting => s_linkconnecting,
            linkrun => s_linkrun,
            linkerror => s_linkerror,
            gotdata => s_gotdata,
            dataerror => s_dataerror,
            tickerror => s_tickerror,
            spw_di => s_spwdi,
            spw_si => s_spwsi,
            spw_do => s_spwdo,
            spw_so => s_spwso,

            data_in_x => s_to_send_x,
            data_in_y => s_to_send_y,
            data_in_z => s_to_send_z,

            data_out_x => s_spwout_x,
            data_out_y => s_spwout_y,
            data_out_z => s_spwout_z
        );

    -- Connect inputs/outputs to internal signals
    sysclk <= clk50;
    s_spwdi <= spw_di;
    s_spwsi <= spw_si;
    spw_do <= s_spwdo;
    spw_so <= s_spwso;

    PROCESS (sysclk) IS
    BEGIN
        IF rising_edge(sysclk) THEN
        
            -- Writing data received from SpW to output to LCD display
            -- To chyba mozna wywalic i bezposrednio polaczyc sygnal s_spwout do LCD
            LCD_x <= s_spwout_x;
            LCD_y <= s_spwout_y;
            LCD_z <= s_spwout_z;

            -- Choosing periodically axis of accelerometer

            CASE accel_state IS
                WHEN accel_prepare =>
                    s_sel_axis <= 0;
                    accel_state <= accel_read_x;
                WHEN accel_read_x =>
                    s_count <= STD_LOGIC_VECTOR(unsigned(s_count) + 1);
                    IF (unsigned(s_count) >= CountVal1) THEN
                        s_count <= (OTHERS => '0');
                        s_x <= sensor_data;
                        s_sel_axis <= 1;
                        accel_state <= accel_read_y;
                    END IF;
                WHEN accel_read_y =>
                    s_count <= STD_LOGIC_VECTOR(unsigned(s_count) + 1);
                    IF (unsigned(s_count) >= CountVal1) THEN
                        s_count <= (OTHERS => '0');
                        s_y <= sensor_data;
                        s_sel_axis <= 2;
                        accel_state <= accel_read_z;
                    END IF;
                WHEN accel_read_z =>
                    s_count <= STD_LOGIC_VECTOR(unsigned(s_count) + 1);
                    IF (unsigned(s_count) >= CountVal1) THEN
                        s_count <= (OTHERS => '0');
                        s_z <= sensor_data;
                        s_sel_axis <= 0;
                        accel_state <= accel_latch;
                    END IF;
                WHEN accel_latch =>
                    -- Latching signals for sending via SpaceWire
                    s_to_send_x <= s_x;
                    s_to_send_y <= s_y;
                    s_to_send_z <= s_z;
                    accel_state <= accel_prepare;
            END CASE;

            s_rst <= s_resetbtn;
            s_clearbtn <= NOT btn_clear;

            -- Synchronize switch settings
            s_autostart <= '0';
            s_linkstart <= switch(0);
            s_linkdisable <= switch(1);
            s_senddata <= switch(2);
            s_txdivcnt(7 DOWNTO 0) <= "00000010";

            -- Sticky link error LED
            s_linkerrorled <= (s_linkerrorled OR s_linkerror) AND
                (NOT s_clearbtn) AND
                (NOT s_resetbtn);
            -- Changing information to display on LEDs
            IF switch(3) = '1' THEN
                led <= s_spwout_x(7 DOWNTO 0);
            ELSE
                -- Drive LEDs (inverted logic)
                led(0) <= s_linkstarted;
                led(1) <= s_linkconnecting;
                led(2) <= s_linkrun;
                led(3) <= s_linkerrorled;
                led(4) <= s_gotdata;
                led(5) <= '0';
                led(6) <= s_dataerror;
                led(7) <= s_tickerror;
            END IF;
        END IF;
    END PROCESS;

END ARCHITECTURE spacewire_top_arch;