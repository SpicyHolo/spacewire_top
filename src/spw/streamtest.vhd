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

        -- Enable sending test patterns to spwstream.
        senddata : IN STD_LOGIC;

        -- Enable sending time codes to spwstream.
        sendtick : IN STD_LOGIC;

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

        -- High when taking a byte from the receive FIFO.
        gotdata : OUT STD_LOGIC;

        -- Incorrect or unexpected data received (sticky).
        dataerror : OUT STD_LOGIC;

        -- Incorrect or unexpected time code received (sticky).
        tickerror : OUT STD_LOGIC;

        -- SpaceWire signals.
        spw_di : IN STD_LOGIC;
        spw_si : IN STD_LOGIC;
        spw_do : OUT STD_LOGIC;
        spw_so : OUT STD_LOGIC;
        
        -- Data ouput signal.
        data_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));


        
        
END ENTITY streamtest;

ARCHITECTURE streamtest_arch OF streamtest IS

    -- -- Update 16-bit maximum length LFSR by 8 steps
    -- FUNCTION lfsr16(x : IN STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
    --     VARIABLE y : STD_LOGIC_VECTOR(15 DOWNTO 0);
    -- BEGIN
    --     -- poly = x^16 + x^14 + x^13 + x^11 + 1
    --     -- tap positions = x(0), x(2), x(3), x(5)
    --     y(7 DOWNTO 0) := x(15 DOWNTO 8);
    --     y(15 DOWNTO 8) := x(7 DOWNTO 0) XOR x(9 DOWNTO 2) XOR x(10 DOWNTO 3) XOR x(12 DOWNTO 5);
    --     RETURN y;
    -- END FUNCTION;

    -- Sending side state.
    TYPE tx_state_type IS (txst_idle, txst_prepare, txst_data);

    -- Receiving side state.
    TYPE rx_state_type IS (rxst_idle, rxst_data);

    -- Registers.
    TYPE regs_type IS RECORD
        tx_state : tx_state_type;
        tx_clk1hz : STD_LOGIC_VECTOR(32 DOWNTO 0);
        tx_pktlen : STD_LOGIC_VECTOR(15 DOWNTO 0);
        tx_enabledata : STD_ULOGIC;
        rx_state : rx_state_type;
        rx_enabledata : STD_ULOGIC;
        rx_badpacket : STD_ULOGIC;
        rx_pktlen : STD_LOGIC_VECTOR(15 DOWNTO 0);
        running : STD_ULOGIC;
        tick_in : STD_ULOGIC;
        time_in : STD_LOGIC_VECTOR(5 DOWNTO 0);
        txwrite : STD_ULOGIC;
        txflag : STD_ULOGIC;
        txdata : STD_LOGIC_VECTOR(7 DOWNTO 0);
        rxread : STD_ULOGIC;
        gotdata : STD_ULOGIC;
        dataerror : STD_ULOGIC;
        tickerror : STD_ULOGIC;
    END RECORD;

    -- Reset state.
    CONSTANT regs_reset : regs_type := (
        tx_state => txst_idle,
        tx_clk1hz => (OTHERS => '0'),
        tx_pktlen => (OTHERS => '0'),
        tx_enabledata => '0',
        rx_state => rxst_idle,
        rx_enabledata => '1',
        rx_badpacket => '0',
        rx_pktlen => (OTHERS => '0'),
        running => '0',
        tick_in => '0',
        time_in => (OTHERS => '0'),
        txwrite => '0',
        txflag => '0',
        txdata => (OTHERS => '0'),
        rxread => '0',
        gotdata => '0',
        dataerror => '0',
        tickerror => '0');

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
    
    SIGNAL s_output_reg : STD_LOGIC_VECTOR(7 DOWNTO 0);

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
    gotdata <= r.gotdata;
    dataerror <= r.dataerror;
    tickerror <= r.tickerror;

    data_out <= s_output_reg; -- Output from spwout latch

    PROCESS (r, rst, senddata, sendtick, s_txrdy, s_tickout, s_timeout, s_rxvalid, s_rxflag, s_rxdata, s_running) IS
        VARIABLE v : regs_type;
    BEGIN
        v := r;

        -- Transmit Data once per second
        v.tx_clk1hz := STD_LOGIC_VECTOR(unsigned(r.tx_clk1hz) + 1);
        IF unsigned(r.tx_clk1hz) = 50000000 THEN -- 1Hz clock
            v.tx_clk1hz := (OTHERS => '0');
            v.tx_enabledata := senddata;
            v.dataerror := NOT r.dataerror;
        ELSE
            v.tx_enabledata := '0';
        END IF;
        
        -- Generate data packets.
        CASE r.tx_state IS
            WHEN txst_idle =>
                -- generate packet length
                v.tx_state := txst_prepare;
                v.tx_pktlen := std_logic_vector(to_unsigned(2, 16));
                v.txwrite := '0';
            WHEN txst_prepare =>
                -- generate first byte of packet
                v.tx_state := txst_data;
                v.txwrite := r.tx_enabledata;
                v.txflag := '0';
                v.txdata := "10101010";
            WHEN txst_data =>
                -- generate data bytes and EOP
                v.txwrite := r.tx_enabledata;
                IF r.txwrite = '1' AND s_txrdy = '1' THEN
                    -- just sent one byte
                    v.tx_pktlen := STD_LOGIC_VECTOR(unsigned(r.tx_pktlen) - 1);
                    IF unsigned(r.tx_pktlen) = 0 THEN
                        -- done with packet
                        v.tx_state := txst_idle;
                        v.txwrite := '0';
                    ELSIF unsigned(r.tx_pktlen) = 1 THEN
                        -- generate EOP
                        v.txwrite := r.tx_enabledata;
                        v.txflag := '1';
                        v.txdata := (OTHERS => '0');
                    ELSE
                        -- generate next data byte
                        v.txwrite := r.tx_enabledata;
                        v.txflag := '0';
                        v.txdata := "10101010";
                    END IF;
                END IF;
        END CASE;

        -- Blink light when receivg data.
        v.gotdata := s_rxvalid AND r.rxread;

        CASE r.rx_state IS
            WHEN rxst_idle =>
                -- get expected packet length
                v.rx_state := rxst_data;
                v.rx_pktlen := std_logic_vector(to_unsigned(2, 16));
            WHEN rxst_data =>
                v.rxread := r.rx_enabledata;
                IF r.rxread = '1' AND s_rxvalid = '1' THEN
                    -- got next byte
                    v.rx_pktlen := STD_LOGIC_VECTOR(unsigned(r.rx_pktlen) - 1);
                    IF s_rxflag = '1' THEN
                        -- got EOP or EEP
                        v.rxread := '0';
                        v.rx_state := rxst_idle;
                        IF s_rxdata = "00000000" THEN
                            -- got EOP
                            IF unsigned(r.rx_pktlen) /= 0 THEN
                                -- unexpected EOP
                                v.rx_badpacket := '1';
                            END IF;
                        ELSE
                            -- got EEP
                            v.rx_badpacket := '1';
                        END IF;
                        v.rx_badpacket := '0';
                    ELSE
                        -- got next byte
                        IF unsigned(r.rx_pktlen) = 0 THEN
                            -- missing EOP
                            v.rx_badpacket := '1';
                        ELSE -- got corfrect byte
                            s_output_reg <= s_rxdata; -- set output latch
                        END IF;
                    END IF;
                END IF;
        END CASE;

        -- Synchronous reset.
        IF rst = '1' THEN
            v := regs_reset;
            s_output_reg <= (others => '0'); -- reset output latch
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