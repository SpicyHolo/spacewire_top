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

ENTITY streamtest_top IS

    PORT (
        clk50 : IN STD_LOGIC;
        btn_reset : IN STD_LOGIC;
        btn_clear : IN STD_LOGIC;
        switch : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        led : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        spw_di : IN STD_LOGIC;
        spw_si : IN STD_LOGIC;
        spw_do : OUT STD_LOGIC;
        spw_so : OUT STD_LOGIC);

END ENTITY streamtest_top;

ARCHITECTURE streamtest_top_arch OF streamtest_top IS

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
    COMPONENT streamtest IS
        GENERIC (
            sysfreq : real;
            txclkfreq : real;
            tickdiv : INTEGER RANGE 12 TO 24 := 20;
            rximpl : spw_implementation_type := impl_generic;
            rxchunk : INTEGER RANGE 1 TO 4 := 1;
            tximpl : spw_implementation_type := impl_generic;
            rxfifosize_bits : INTEGER RANGE 6 TO 14 := 11;
            txfifosize_bits : INTEGER RANGE 2 TO 14 := 11);
        PORT (
            clk : IN STD_LOGIC;
            rxclk : IN STD_LOGIC;
            txclk : IN STD_LOGIC;
            rst : IN STD_LOGIC;
            linkstart : IN STD_LOGIC;
            autostart : IN STD_LOGIC;
            linkdisable : IN STD_LOGIC;
            senddata : IN STD_LOGIC;
            sendtick : IN STD_LOGIC;
            txdivcnt : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            linkstarted : OUT STD_LOGIC;
            linkconnecting : OUT STD_LOGIC;
            linkrun : OUT STD_LOGIC;
            linkerror : OUT STD_LOGIC;
            gotdata : OUT STD_LOGIC;
            dataerror : OUT STD_LOGIC;
            tickerror : OUT STD_LOGIC;
            spw_di : IN STD_LOGIC;
            spw_si : IN STD_LOGIC;
            spw_do : OUT STD_LOGIC;
            spw_so : OUT STD_LOGIC);
    END COMPONENT;

BEGIN
    -- Streamtest instance
    streamtest_inst : streamtest
    GENERIC MAP(
        sysfreq => 50.0e6,
        txclkfreq => 0.0,
        tickdiv => 20,
        rximpl => impl_generic,
        rxchunk => 1,
        tximpl => impl_generic,
        rxfifosize_bits => 11,
        txfifosize_bits => 11)
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
        spw_so => s_spwso);

    sysclk <= clk50;
    s_spwdi <= spw_di;
    s_spwsi <= spw_si;
    spw_do <= s_spwdo;
    spw_so <= s_spwso;

    PROCESS (sysclk) IS
    BEGIN
        IF rising_edge(sysclk) THEN

            -- Synchronize buttons
            s_resetbtn <= NOT btn_reset;
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
            led(0) <= s_linkstarted;
            led(1) <= s_linkconnecting;
            led(2) <= s_linkrun;
            led(3) <= s_linkerrorled;
            led(4) <= s_gotdata;
            led(5) <= '0';
            led(6) <= s_dataerror;
            led(7) <= s_tickerror;

        END IF;
    END PROCESS;

END ARCHITECTURE streamtest_top_arch;