--
--  Test of spwstream on Pender GR-XC3S-1500 board.
--  60 MHz system clock; 200 MHz receive clock and transmit clock.
--
--  LED 0 = link run
--  LED 1 = link error (sticky until clear button)
--  LED 2 = gotdata
--  LED 3 = data/timecode error (sticky until reset)
--
--  Button S2 = reset
--  Button S3 = clear LED 1
--
--  Switch 0 = link start
--  Switch 1 = link disable
--  Switch 2 = send data
--  Switch 3 = send time codes
--  Switch 4-7 = bits 0-3 of tx bit rate scale factor
--
--  SpaceWire signals on expansion connector J12:
--    Data In    pos,neg  =  m1,m2  =  pin 3,2
--    Strobe In  pos,neg  =  m3,m4  =  pin 6,5
--    Data Out   pos,neg  =  n1,n2  =  pin 9,8
--    Strobe Out pos,neg  =  n3,n4  =  pin 12,11
--
--  To get proper LVDS signals from connector J12, the voltage on I/O bank 6
--  must be set to 2.5V. This is the default on GR-XC3S-1500-rev2, but on
--  GR-XC3S-1500-rev1 a change is required on the board (described in
--  the board manual).
--
--  To terminate the incoming LVDS signals, 100 Ohm termination resistors
--  must be installed on the board in positions R120 and R121.
--
--  The SpaceWire port should be looped back to itself, either directly
--  or via an other SpaceWire device. For a direct loopback, place 4 wires
--  from the output pins to the corresponding input pins. For an indirect
--  loopback, connect the SpaceWire signals to an additional SpaceWire device
--  which is programmed to echo everything it receives (characters, packets,
--  time codes). See the datasheet for a wiring diagram from J12 to MDM9.
--

library ieee;
use ieee.std_logic_1164.all, ieee.numeric_std.all;
use work.spwpkg.all;

entity streamtest_top is

    port (
        clk:        in  std_logic;
        btn_reset:  in  std_logic;
        btn_clear:  in  std_logic;
        switch:     in  std_logic_vector(3 downto 0);
        led:        out std_logic_vector(3 downto 0) := "0000";
        spw_di:   in  std_logic;
        spw_si:   in  std_logic;
        spw_do:   out std_logic;
        spw_so:   out std_logic);

end entity streamtest_top;

architecture streamtest_top_arch of streamtest_top is

    -- Clock generation.
    signal sysclk:          std_logic;
	 
    -- Synchronize buttons
    signal s_resetbtn:      std_logic := '0';
    signal s_clearbtn:      std_logic := '0';

    -- Sticky LED
    signal s_linkerrorled:  std_logic := '0';

    -- Interface signals.
    signal s_rst:           std_logic := '1';
    signal s_linkstart:     std_logic := '0';
    signal s_autostart:     std_logic := '0';
    signal s_linkdisable:   std_logic := '0';
    signal s_senddata:      std_logic := '0';
    signal s_sendtick:      std_logic := '0';
    signal s_txdivcnt:      std_logic_vector(7 downto 0) := "00000000";
    signal s_linkstarted:   std_logic;
    signal s_linkconnecting: std_logic;
    signal s_linkrun:       std_logic;
    signal s_linkerror:     std_logic;
    signal s_gotdata:       std_logic;
    signal s_dataerror:     std_logic;
    signal s_tickerror:     std_logic;
    signal s_spwdi:         std_logic;
    signal s_spwsi:         std_logic;
    signal s_spwdo:         std_logic;
    signal s_spwso:         std_logic;

    component streamtest is
        generic (
            sysfreq:    real;
            txclkfreq:  real;
            tickdiv:    integer range 12 to 24 := 20;
            rximpl:     spw_implementation_type := impl_generic;
            rxchunk:    integer range 1 to 4 := 1;
            tximpl:     spw_implementation_type := impl_generic;
            rxfifosize_bits: integer range 6 to 14 := 11;
            txfifosize_bits: integer range 2 to 14 := 11 );
        port (
            clk:        in  std_logic;
            rxclk:      in  std_logic;
            txclk:      in  std_logic;
            rst:        in  std_logic;
            linkstart:  in  std_logic;
            autostart:  in  std_logic;
            linkdisable: in std_logic;
            senddata:   in  std_logic;
            sendtick:   in  std_logic;
            txdivcnt:   in  std_logic_vector(7 downto 0);
            linkstarted: out std_logic;
            linkconnecting: out std_logic;
            linkrun:    out std_logic;
            linkerror:  out std_logic;
            gotdata:    out std_logic;
            dataerror:  out std_logic;
            tickerror:  out std_logic;
            spw_di:     in  std_logic;
            spw_si:     in  std_logic;
            spw_do:     out std_logic;
            spw_so:     out std_logic );
    end component;

begin
    -- Streamtest instance
    streamtest_inst: streamtest
        generic map (
            sysfreq     => 50.0e6,
            txclkfreq   => 0.0,
            tickdiv     => 22,
            rximpl      => impl_generic,
            rxchunk     => 1,
            tximpl      => impl_generic,
            rxfifosize_bits => 11,
            txfifosize_bits => 10 )
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
            spw_so      => s_spwso );

    process (sysclk) is
    begin
        if rising_edge(sysclk) then

            -- Synchronize buttons
            s_resetbtn  <= btn_reset;
            s_rst       <= s_resetbtn;
            s_clearbtn  <= btn_clear;

            -- Synchronize switch settings
            s_autostart <= '0';
            s_linkstart <= switch(0);
            s_linkdisable <= switch(1);
            s_senddata  <= switch(2);
            s_sendtick  <= switch(3);
            s_txdivcnt(7 downto 0) <= "0000000";

            -- Sticky link error LED
            s_linkerrorled <= (s_linkerrorled or s_linkerror) and
                              (not s_clearbtn) and
                              (not s_resetbtn);

            -- Drive LEDs (inverted logic)
            led(0)  <= s_linkrun;
            led(1)  <= s_linkerrorled;
            led(2)  <= s_gotdata;
            led(3)  <= (s_dataerror or s_tickerror);

        end if;
    end process;

end architecture streamtest_top_arch;