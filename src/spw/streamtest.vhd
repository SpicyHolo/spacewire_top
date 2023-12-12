--
--  Test application for spwstream.
--
--  This entity implements one spwstream instance with SpaceWire signals
--  routed to external ports. The SpaceWire port is assumed to be looped back
--  to itself externally, either directly (tx pins wired to rx pins) or
--  through a remote SpaceWire device which is programmed to echo anything
--  it receives.
--
--  This entity submits a series of test patterns to the transmit side of
--  spwstream. At the same time it monitors the receive side of spwstream
--  and verifies that received data matches the transmitted data pattern.
--
--  Link mode and tx bit rate may be programmed through digital inputs
--  (presumably connected to switches or buttons). Link state and progress of
--  the test are reported through digital outputs (presumably connected to
--  LEDs).
--
--  Note: there is no check on the integrity of the first packet received
--  after the link goes up.
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.spwpkg.ALL;

ENTITY streamtest IS

    GENERIC (
        -- System clock frequency in Hz.
        sysfreq : real;

        -- txclk frequency in Hz (if tximpl = impl_fast).
        txclkfreq : real;

        -- 2-log of division factor from system clock freq to timecode freq.
        tickdiv : INTEGER RANGE 12 TO 24 := 20;

        -- Receiver front-end implementation.
        rximpl : spw_implementation_type := impl_generic;

        -- Maximum number of bits received per system clock (impl_fast only).
        rxchunk : INTEGER RANGE 1 TO 4 := 1;

        -- Transmitter implementation.
        tximpl : spw_implementation_type := impl_generic;

        -- Size of receive FIFO.
        rxfifosize_bits : INTEGER RANGE 6 TO 14 := 11;

        -- Size of transmit FIFO.
        txfifosize_bits : INTEGER RANGE 2 TO 14 := 11);

    PORT (
        -- System clock.
        clk : IN STD_LOGIC;

        -- Receiver sample clock (only for impl_fast).
        rxclk : IN STD_LOGIC;

        -- Transmit clock (only for impl_fast).
        txclk : IN STD_LOGIC;

        -- Synchronous reset (active-high).
        rst : IN STD_LOGIC;

        -- Enables spontaneous link start.
        linkstart : IN STD_LOGIC;

        -- Enables automatic link start on receipt of a NULL token.
        autostart : IN STD_LOGIC;

        -- Do not start link and/or disconnect current link.
        linkdisable : IN STD_LOGIC;

        -- Scaling factor minus 1 for TX bitrate.
        txdivcnt : IN STD_LOGIC_VECTOR(7 DOWNTO 0);

        -- Link in state Started.
        linkstarted : OUT STD_LOGIC;

        -- Link in state Connecting.
        linkconnecting : OUT STD_LOGIC;

        -- Link in state Run.
        linkrun : OUT STD_LOGIC;

        -- Link error (one cycle pulse, not directly suitable for LED)
        linkerror : OUT STD_LOGIC;

        -- SpaceWire signals.
        spw_di : IN STD_LOGIC;
        spw_si : IN STD_LOGIC;
        spw_do : OUT STD_LOGIC;
        spw_so : OUT STD_LOGIC);

END ENTITY streamtest;

ARCHITECTURE streamtest_arch OF streamtest IS
    -- Registers.
    TYPE regs_type IS RECORD
        running : STD_ULOGIC;
        tick_in : STD_ULOGIC;
        time_in : STD_LOGIC_VECTOR(5 DOWNTO 0);
        txwrite : STD_ULOGIC;
        txflag : STD_ULOGIC;
        txdata : STD_LOGIC_VECTOR(7 DOWNTO 0);
        rxread : STD_ULOGIC;
    END RECORD;

    -- Reset state.
    CONSTANT regs_reset : regs_type := (
        running => '0',
        tick_in => '0',
        time_in => (OTHERS => '0'),
        txwrite => '0',
        txflag => '0',
        txdata => (OTHERS => '0'),
        rxread => '0');

    SIGNAL r : regs_type := regs_reset;
    SIGNAL rin : regs_type;

    -- Interface signals.
    SIGNAL s_txrdy : STD_LOGIC;
    SIGNAL s_tickout : STD_LOGIC;
    SIGNAL s_timeout : STD_LOGIC_VECTOR(5 DOWNTO 0);
    SIGNAL s_rxvalid : STD_LOGIC;
    SIGNAL s_rxflag : STD_LOGIC;
    SIGNAL s_rxdata : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL s_running : STD_LOGIC;
    SIGNAL s_errdisc : STD_LOGIC;
    SIGNAL s_errpar : STD_LOGIC;
    SIGNAL s_erresc : STD_LOGIC;
    SIGNAL s_errcred : STD_LOGIC;

BEGIN

    -- spwstream instance
    spwstream_inst : spwstream
    GENERIC MAP(
        sysfreq => sysfreq,
        txclkfreq => txclkfreq,
        rximpl => rximpl,
        rxchunk => rxchunk,
        tximpl => tximpl,
        rxfifosize_bits => rxfifosize_bits,
        txfifosize_bits => txfifosize_bits)
    PORT MAP(
        clk => clk,
        rxclk => rxclk,
        txclk => txclk,
        rst => rst,
        autostart => autostart,
        linkstart => linkstart,
        linkdis => linkdisable,
        txdivcnt => txdivcnt,
        tick_in => r.tick_in,
        ctrl_in => (OTHERS => '0'),
        time_in => r.time_in,
        txwrite => r.txwrite,
        txflag => r.txflag,
        txdata => r.txdata,
        txrdy => s_txrdy,
        txhalff => OPEN,
        tick_out => s_tickout,
        ctrl_out => OPEN,
        time_out => s_timeout,
        rxvalid => s_rxvalid,
        rxhalff => OPEN,
        rxflag => s_rxflag,
        rxdata => s_rxdata,
        rxread => r.rxread,
        started => linkstarted,
        connecting => linkconnecting,
        running => s_running,
        errdisc => s_errdisc,
        errpar => s_errpar,
        erresc => s_erresc,
        errcred => s_errcred,
        spw_di => spw_di,
        spw_si => spw_si,
        spw_do => spw_do,
        spw_so => spw_so);

    -- Drive status indications.
    linkrun <= s_running;
    linkerror <= s_errdisc OR s_errpar OR s_erresc OR s_errcred;

    PROCESS (r, rst) IS
        VARIABLE v : regs_type;
    BEGIN
        v := r;
    
        -- Synchronous reset.
        IF rst = '1' THEN
            v := regs_reset;
        END IF;

        -- Update registers.
        rin <= v;
    END PROCESS;

    -- Update registers.
    PROCESS (clk) IS
    BEGIN
        IF rising_edge(clk) THEN
            r <= rin;
        END IF;
    END PROCESS;

END ARCHITECTURE streamtest_arch;