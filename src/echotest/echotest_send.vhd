LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.spwpkg.ALL;

ENTITY echotest_send IS

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
        spw_so : OUT STD_LOGIC
    );

END ENTITY echotest_send;

ARCHITECTURE echotest_send_arch OF echotest_send IS

    -- Update 16-bit maximum length LFSR by 8 steps
    FUNCTION lfsr16(x : IN STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
        VARIABLE y : STD_LOGIC_VECTOR(15 DOWNTO 0);
    BEGIN
        -- poly = x^16 + x^14 + x^13 + x^11 + 1
        -- tap positions = x(0), x(2), x(3), x(5)
        y(7 DOWNTO 0) := x(15 DOWNTO 8);
        y(15 DOWNTO 8) := x(7 DOWNTO 0) XOR x(9 DOWNTO 2) XOR x(10 DOWNTO 3) XOR x(12 DOWNTO 5);
        RETURN y;
    END FUNCTION;

    -- Sending side state.
    TYPE tx_state_type IS (txst_idle, txst_prepare, txst_data);

    -- Receiving side state.
    TYPE rx_state_type IS (rxst_idle, rxst_data);

    -- Registers.
    TYPE regs_type IS RECORD
        tx_state : tx_state_type;
        tx_timecnt : STD_LOGIC_VECTOR((tickdiv - 1) DOWNTO 0);
        tx_quietcnt : STD_LOGIC_VECTOR(15 DOWNTO 0);
        tx_pktlen : STD_LOGIC_VECTOR(15 DOWNTO 0);
        tx_lfsr : STD_LOGIC_VECTOR(15 DOWNTO 0);
        tx_enabledata : STD_ULOGIC;
        rx_state : rx_state_type;
        rx_quietcnt : STD_LOGIC_VECTOR(15 DOWNTO 0);
        rx_enabledata : STD_ULOGIC;
        rx_gottick : STD_ULOGIC;
        rx_expecttick : STD_ULOGIC;
        rx_expectglitch : unsigned(5 DOWNTO 0);
        rx_badpacket : STD_ULOGIC;
        rx_pktlen : STD_LOGIC_VECTOR(15 DOWNTO 0);
        rx_prev : STD_LOGIC_VECTOR(15 DOWNTO 0);
        rx_lfsr : STD_LOGIC_VECTOR(15 DOWNTO 0);
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
        tx_pktlen => (OTHERS => '0'),
        tx_lfsr => (1 => '1', OTHERS => '0'),
        tx_enabledata => '0',
        rx_state => rxst_idle,
        rx_quietcnt => (OTHERS => '0'),
        rx_enabledata => '0',
        rx_gottick => '0',
        rx_expecttick => '0',
        rx_expectglitch => "000001",
        rx_badpacket => '0',
        rx_pktlen => (OTHERS => '0'),
        rx_prev => (OTHERS => '0'),
        rx_lfsr => (OTHERS => '0'),
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
    gotdata <= r.gotdata;
    dataerror <= r.dataerror;
    tickerror <= r.tickerror;

    PROCESS (r, rst, senddata, sendtick, s_txrdy, s_tickout, s_timeout, s_rxvalid, s_rxflag, s_rxdata, s_running) IS
        VARIABLE v : regs_type;
    BEGIN
        v := r;

        -- Initiate timecode transmissions.
        v.tx_timecnt := STD_LOGIC_VECTOR(unsigned(r.tx_timecnt) + 1);
        IF unsigned(v.tx_timecnt) = 0 THEN
            v.tick_in := sendtick;
        ELSE
            v.tick_in := '0';
        END IF;
        IF r.tick_in = '1' THEN
            v.time_in := STD_LOGIC_VECTOR(unsigned(r.time_in) + 1);
            v.rx_expecttick := '1';
            v.rx_gottick := '0';
        END IF;

        -- Turn data generator on/off at regular intervals.
        v.tx_quietcnt := STD_LOGIC_VECTOR(unsigned(r.tx_quietcnt) + 1);
        IF unsigned(r.tx_quietcnt) = 61000 THEN
            v.tx_quietcnt := (OTHERS => '0');
        END IF;
        v.tx_enabledata := senddata AND (NOT r.tx_quietcnt(15)); -- ?????

        -- Generate data packets.
        CASE r.tx_state IS
            WHEN txst_idle =>
                -- generate packet length
                v.tx_state := txst_prepare;
                v.tx_pktlen := r.tx_lfsr;
                v.txwrite := '0';
                v.tx_lfsr := lfsr16(r.tx_lfsr);
            WHEN txst_prepare =>
                -- generate first byte of packet
                v.tx_state := txst_data;
                v.txwrite := r.tx_enabledata;
                v.txflag := '0';
                v.txdata := r.tx_lfsr(15 DOWNTO 8);
                v.tx_lfsr := lfsr16(r.tx_lfsr);
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
                        v.tx_lfsr := lfsr16(r.tx_lfsr);
                    ELSE
                        -- generate next data byte
                        v.txwrite := r.tx_enabledata;
                        v.txflag := '0';
                        v.txdata := r.tx_lfsr(15 DOWNTO 8);
                        v.tx_lfsr := lfsr16(r.tx_lfsr);
                    END IF;
                END IF;
        END CASE;

        -- Blink light when receiving data.
        v.gotdata := s_rxvalid AND r.rxread;

        -- Detect missing timecodes.
        IF r.tick_in = '1' AND r.rx_expecttick = '1' THEN
            -- This is bad; a new timecode is being generated while
            -- we have not even received the previous one yet.
            v.tickerror := '1';
        END IF;

        -- Receive and check incoming timecodes.
        IF s_tickout = '1' THEN
            IF unsigned(s_timeout) + 1 /= unsigned(r.time_in) THEN
                -- Received time code does not match last transmitted code.
                v.tickerror := '1';
            END IF;
            IF r.rx_gottick = '1' THEN
                -- Already received the last transmitted time code.
                v.tickerror := '1';
            END IF;
            v.rx_expecttick := '0';
            v.rx_gottick := '1';
        END IF;

        -- Turn data receiving on/off at regular intervals
        v.rx_quietcnt := STD_LOGIC_VECTOR(unsigned(r.rx_quietcnt) + 1);
        IF unsigned(r.rx_quietcnt) = 55000 THEN
            v.rx_quietcnt := (OTHERS => '0');
        END IF;
        v.rx_enabledata := NOT r.rx_quietcnt(15);

        CASE r.rx_state IS
            WHEN rxst_idle =>
                -- get expected packet length
                v.rx_state := rxst_data;
                v.rx_pktlen := r.rx_lfsr;
                v.rx_lfsr := lfsr16(r.rx_lfsr);
                v.rx_prev := (OTHERS => '0');
            WHEN rxst_data =>
                v.rxread := r.rx_enabledata;
                IF r.rxread = '1' AND s_rxvalid = '1' THEN
                    -- got next byte
                    v.rx_pktlen := STD_LOGIC_VECTOR(unsigned(r.rx_pktlen) - 1);
                    v.rx_prev := s_rxdata & r.rx_prev(15 DOWNTO 8);
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
                            -- count errors against expected glitches
                            IF v.rx_badpacket = '1' THEN
                                -- got glitch
                                IF r.rx_expectglitch = 0 THEN
                                    v.dataerror := '1';
                                ELSE
                                    v.rx_expectglitch := r.rx_expectglitch - 1;
                                END IF;
                            END IF;
                            -- resynchronize LFSR
                            v.rx_lfsr := lfsr16(lfsr16(r.rx_prev));
                        ELSE
                            -- got EEP
                            v.rx_badpacket := '1';
                        END IF;
                        v.rx_badpacket := '0';
                    ELSE
                        -- got next byte
                        v.rx_lfsr := lfsr16(r.rx_lfsr);
                        IF unsigned(r.rx_pktlen) = 0 THEN
                            -- missing EOP
                            v.rx_badpacket := '1';
                        END IF;
                        IF s_rxdata /= r.rx_lfsr(15 DOWNTO 8) THEN
                            -- bad data
                            v.rx_badpacket := '1';
                        END IF;
                    END IF;
                END IF;
        END CASE;

        -- If the link goes away, we should expect inconsistency on the receiving side.
        v.running := s_running;
        IF r.running = '1' AND s_running = '0' THEN
            IF r.rx_expectglitch /= "111111" THEN
                v.rx_expectglitch := r.rx_expectglitch + 1;
            END IF;
        END IF;

        -- If there is no link, we should not expect to receive time codes.
        IF s_running = '0' THEN
            v.rx_expecttick := '0';
        END IF;

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

END ARCHITECTURE echotest_send_arch;