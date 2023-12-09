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
    PORT (
        --Acclerometer ports
        acc_spi_chip_select     : OUT   STD_LOGIC; -- Accelerometer chip select (negated)
        acc_spi_clk             : OUT   STD_LOGIC;
        acc_spi_data            : INOUT STD_LOGIC; -- no MOSI/MISO, hardware supports only 3-wire SPI
        acc_interrupt           : IN    STD_LOGIC;

		-- External LCD ports
		LCD_EN                  : OUT STD_LOGIC;
		LCD_RS                  : OUT STD_LOGIC;
		LCD_RW                  : OUT STD_LOGIC;
		LCD_DATA                : INOUT STD_LOGIC_VECTOR(7 DOWNTO 0);

        --SpW ports
        clk50:      in  std_logic;
        btn_reset:  in  std_logic;
        btn_clear:  in  std_logic; 
        switch:     in  std_logic_vector(3 downto 0);
        led:        out std_logic_vector(7 downto 0);
        spw_di:     in  std_logic;
        spw_si:     in  std_logic;
        spw_do:     out std_logic;
        spw_so:     out std_logic 
        );

END ENTITY spacewire_top;

ARCHITECTURE spacewire_top_arch OF spacewire_top IS

    --Accelerometer signals

    --pure output of 12-bit ADC (0 padding in front?)
    --register map p.23: https://www.analog.com/media/en/technical-documentation/data-sheets/adxl345.pdf
    signal sensor_data : STD_LOGIC_VECTOR(15 downto 0);
    signal s_sel_axis : INTEGER range 0 to 2; -- select accelerometer readout axis (1000 sysclk delay?)

    --LCD signals register control
    signal s_lcd_register_data_in : STD_LOGIC_VECTOR(15 DOWNTO 0);

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

BEGIN
    -- Accelerometer instance
    accelerometer_inst : entity work.accelerometer 
    port map (
        clk             => sysclk,
        sel_axis        => s_sel_axis,
        rst             => btn_reset,
        G_SENSOR_CS_N   => acc_spi_chip_select,
        G_SENSOR_INT    => acc_interrupt,
        I2C_SCLK        => acc_spi_clk,
        G_SENSOR_OUT    => sensor_data,
        I2C_SDAT        => acc_spi_data
    );

    -- LCD driver instance
    lcd_inst : entity work.spacewire_lcd_driver
    port map (
      CLOCK_50        => sysclk, -- DE0 CLOCK_50 (50MHz CLK)
		KEY             => btn_reset, -- DE0 KEY (button) [reset]
		LED             => led,
		-- External LCD ports
		LCD_EN          => LCD_EN,
		LCD_RS          => LCD_RS,
		LCD_RW          => LCD_RW,
		LCD_DATA        => LCD_DATA,
		-- LCD Register control
		lcd_register_data_in => sensor_data 
    );

    -- Streamtest instance
	streamtest_inst : entity work.streamtest 
    generic map (
         sysfreq     => 50.0e6,
         txclkfreq   => 0.0,
         tickdiv     => 20,
         rximpl      => impl_generic,
         rxchunk     => 1,
         tximpl      => impl_generic,
         rxfifosize_bits => 11,
         txfifosize_bits => 11
    )
    port map (
        clk         => sysclk,
        rxclk       => '0',
        txclk       => '0',
        rst         => s_rst,
        linkstart   => s_linkstart,
        autostart   => s_autostart,
        linkdisable => s_linkdisable,
        senddata    => s_senddata,
        sendtick    => s_sendtick,
        txdivcnt    => s_txdivcnt,
        linkstarted => s_linkstarted,
        linkconnecting => s_linkconnecting,
        linkrun     => s_linkrun,
        linkerror   => s_linkerror,
        gotdata     => s_gotdata,
        dataerror   => s_dataerror,
        tickerror   => s_tickerror,
        spw_di      => s_spwdi,
        spw_si      => s_spwsi,
        spw_do      => s_spwdo,
        spw_so      => s_spwso
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
            --select axis to send to lcd
            s_sel_axis <= 1; 

            s_rst <= s_resetbtn;
            s_clearbtn <= NOT btn_clear;

            -- Synchronize switch settings
            s_autostart <= '0';
            s_linkstart <= switch(0);
            s_linkdisable <= switch(1);
            s_senddata <= switch(2);
            s_sendtick <= switch(3);
            s_txdivcnt(7 DOWNTO 0) <= "00000000";

            -- Sticky link error LED
            s_linkerrorled <= (s_linkerrorled OR s_linkerror) AND
                (NOT s_clearbtn) AND
                (NOT s_resetbtn);

            -- Drive LEDs (inverted logic)
            -- led <= sensor_data(7 DOWNTO 0);
            -- led(0) <= s_linkstarted;
            -- led(1) <= s_linkconnecting;
            -- led(2) <= s_linkrun;
            -- led(3) <= s_linkerrorled;
            -- led(4) <= s_gotdata;
            -- led(5) <= '0';
            -- led(6) <= s_dataerror;
            -- led(7) <= s_tickerror;

        END IF;
    END PROCESS;

END ARCHITECTURE spacewire_top_arch;