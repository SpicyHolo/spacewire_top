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

ENTITY echotest_recv IS

    GENERIC (
        sysfreq : real;
        txclkfreq : real; -- (if tximpl = impl_fast).
        -- log2 of division factor from system clock freq to timecode freq.
        tickdiv : INTEGER RANGE 12 TO 24 := 20;
        rximpl : spw_implementation_type := impl_generic;
        rxchunk : INTEGER RANGE 1 TO 4 := 1; -- Max bits per sysclock (impl_fast only).
        tximpl : spw_implementation_type := impl_generic;
        rxfifosize_bits : INTEGER RANGE 6 TO 14 := 11;
        txfifosize_bits : INTEGER RANGE 2 TO 14 := 11
    );
    PORT (
        clk : IN STD_LOGIC;
        rxclk : IN STD_LOGIC; -- Receiver sample clock (only for impl_fast).
        txclk : IN STD_LOGIC; -- Transmit clock (only for impl_fast).
        rst : IN STD_LOGIC;
        linkstart : IN STD_LOGIC; -- Enables spontaneous link start.
        autostart : IN STD_LOGIC; -- link start on NULL
        linkdisable : IN STD_LOGIC;
        senddata : IN STD_LOGIC;
        sendtick : IN STD_LOGIC;
        -- Scaling factor minus 1 for TX bitrate.
        txdivcnt : IN STD_LOGIC_VECTOR(7 DOWNTO 0);

        -- State
        linkstarted : OUT STD_LOGIC;
        linkconnecting : OUT STD_LOGIC;
        linkrun : OUT STD_LOGIC;
        linkerror : OUT STD_LOGIC; -- one pulse, not for LED
        -- High when taking a byte from the receive FIFO.
        gotdata : OUT STD_LOGIC;
        dataerror : OUT STD_LOGIC; -- (sticky)
        tickerror : OUT STD_LOGIC; -- (sticky)

        -- SpaceWire signals.
        spw_di : IN STD_LOGIC;
        spw_si : IN STD_LOGIC;
        spw_do : OUT STD_LOGIC;
        spw_so : OUT STD_LOGIC;

        linkerror_disc : OUT STD_LOGIC;
        linkerror_par : OUT STD_LOGIC;
        linkerror_esc : OUT STD_LOGIC;
        linkerror_cred : OUT STD_LOGIC
    );

END ENTITY echotest_recv;

ARCHITECTURE echotest_recv_arch OF echotest_recv IS

    -- Receiving side state.
    TYPE rx_state_type IS (rxst_idle, rxst_data);
    TYPE tx_state_type IS (txst_idle, txst_prepare, txst_data);
    -- Registers.
    TYPE regs_type IS RECORD
        tx_state : tx_state_type;
        tx_timecnt : STD_LOGIC_VECTOR((tickdiv - 1) DOWNTO 0);
        tx_quietcnt : STD_LOGIC_VECTOR(15 DOWNTO 0);
        tx_enabledata : STD_ULOGIC;
        rx_state : rx_state_type;
        rx_quietcnt : STD_LOGIC_VECTOR(15 DOWNTO 0);
        rx_enabledata : STD_ULOGIC;
        rx_badpacket : STD_ULOGIC;
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
        tx_timecnt => (OTHERS => '0'),
        tx_quietcnt => (OTHERS => '0'),
        tx_enabledata => '0',
        rx_state => rxst_idle,
        rx_quietcnt => (OTHERS => '0'),
        rx_enabledata => '0',
        rx_badpacket => '0',
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
        spw_so => spw_so
    );

    -- Drive status indications.
    linkrun <= s_running;
    linkerror <= s_errdisc OR s_errpar OR s_erresc OR s_errcred;
    linkerror_disc <= s_errdisc;
    linkerror_par <= s_errpar;
    linkerror_esc <= s_erresc;
    linkerror_cred <= s_errcred;
    gotdata <= r.gotdata;
    dataerror <= r.dataerror;
    tickerror <= r.tickerror;

    PROCESS (r, rst, senddata, sendtick, s_txrdy, s_tickout, s_timeout, s_rxvalid, s_rxflag, s_rxdata, s_running) IS
        VARIABLE v : regs_type;
    BEGIN
        v := r;
        v.tick_in := s_tickout;
        v.time_in := s_timeout;
        -- Receive and check incoming timecodes.
        IF s_tickout = '1' THEN
            IF unsigned(s_timeout) + 1 /= unsigned(r.time_in) THEN
                -- Received time code does not match last transmitted code.
                v.tickerror := '1';
            ELSE
                v.tickerror := '0';
            END IF;
        END IF;
        -- TODO:
        -- Create receive FIFO
        -- Handle EEP
        -- Implement isEOP & isEEP

        -- -- Turn data generator on/off at regular intervals.
        -- v.tx_quietcnt := std_logic_vector(unsigned(r.tx_quietcnt) + 1);
        -- if unsigned(r.tx_quietcnt) = 61000 then
        --     v.tx_quietcnt := (others => '0');
        -- end if;
        -- v.tx_enabledata := senddata and (not r.tx_quietcnt(15));

        -- -- Generate data packets.
        -- case r.tx_state is
        --     when txst_idle =>
        --         v.tx_state  := txst_prepare; -- if reg not empty
        --         v.txwrite   := '0';
        --     when txst_prepare =>
        --         -- generate first byte of packet
        --         v.tx_state  := txst_data;
        --         v.txwrite   := r.tx_enabledata;
        --         v.txflag    := '0';
        --         v.txdata    := r.tx_lfsr(15 downto 8); -- get first byte in reg
        --     when txst_data =>
        --         -- generate data bytes and EOP
        --         v.txwrite   := r.tx_enabledata;
        --         if r.txwrite = '1' and s_txrdy = '1' then
        --             -- just sent one byte
        --             if unsigned(r.tx_pktlen) = 0 then -- if r.txdata = EOP or EEP
        --                 -- done with packet
        --                 v.tx_state  := txst_idle;
        --                 v.txwrite   := '0';
        --             else
        --                 -- generate next data byte
        --                 v.txwrite   := r.tx_enabledata;
        --                 v.txdata    := r.tx_lfsr(15 downto 8); -- get next from reg
        --                 v.txflag    := '0'; -- := isEOP(v.txdata)
        --             end if;
        --         end if;
        -- end case;
        -- -- Turn data receiving on/off at regular intervals
        v.rx_quietcnt := STD_LOGIC_VECTOR(unsigned(r.rx_quietcnt) + 1);
        IF unsigned(r.rx_quietcnt) = 55000 THEN
            v.rx_quietcnt := (OTHERS => '0');
        END IF;
        v.rx_enabledata := NOT r.rx_quietcnt(15);
        v.rxread := r.rx_enabledata;
        IF r.rxread = '1' AND s_rxvalid = '1' THEN
            -- got next byte
            v.txwrite := '1';
            v.txflag := s_rxflag;
            v.txdata := s_rxdata;
        ELSE
            v.txwrite := '0';
        END IF;
        -- -- Blink light when receiving data.
        v.gotdata := s_rxvalid AND r.rxread;
        -- If the link goes away, we should expect inconsistency on the receiving side.
        v.running := s_running;

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

END ARCHITECTURE echotest_recv_arch;