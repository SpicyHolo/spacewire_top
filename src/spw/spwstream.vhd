--
--  SpaceWire core with character-stream interface.
--
--  This entity provides a SpaceWire core with a character-stream interface.
--  The interface provides means for connection initiation, sending and
--  receiving of N-Chars and TimeCodes, and error reporting.
--
--  This entity instantiates spwlink, spwrecv, spwxmit and one of the
--  spwrecvfront implementations. It also implements a receive FIFO and
--  a transmit FIFO.
--
--  The SpaceWire standard requires that each transceiver use an initial
--  signalling rate of 10 Mbit/s. This implies that the system clock frequency
--  must be a multiple of 10 MHz. See the manual for further details on
--  bitrates and clocking.
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.spwpkg.ALL;

ENTITY spwstream IS

    GENERIC (
        -- System clock frequency in Hz.
        -- This must be set to the frequency of "clk". It is used to setup
        -- counters for reset timing, disconnect timeout and to transmit
        -- at 10 Mbit/s during the link handshake.
        sysfreq : real;

        -- Transmit clock frequency in Hz (only if tximpl = impl_fast).
        -- This must be set to the frequency of "txclk". It is used to
        -- transmit at 10 Mbit/s during the link handshake.
        txclkfreq : real := 0.0;

        -- Selection of a receiver front-end implementation.
        rximpl : spw_implementation_type := impl_generic;

        -- Maximum number of bits received per system clock
        -- (must be 1 in case of impl_generic).
        rxchunk : INTEGER RANGE 1 TO 4 := 1;

        -- Selection of a transmitter implementation.
        tximpl : spw_implementation_type := impl_generic;

        -- Size of the receive FIFO as the 2-logarithm of the number of bytes.
        -- Must be at least 6 (64 bytes).
        rxfifosize_bits : INTEGER RANGE 6 TO 14 := 11;

        -- Size of the transmit FIFO as the 2-logarithm of the number of bytes.
        txfifosize_bits : INTEGER RANGE 2 TO 14 := 11
    );

    PORT (
        -- System clock.
        clk : IN STD_LOGIC;

        -- Receiver sample clock (only for impl_fast)
        rxclk : IN STD_LOGIC;

        -- Transmit clock (only for impl_fast)
        txclk : IN STD_LOGIC;

        -- Synchronous reset (active-high).
        rst : IN STD_LOGIC;

        -- Enables automatic link start on receipt of a NULL character.
        autostart : IN STD_LOGIC;

        -- Enables link start once the Ready state is reached.
        -- Without autostart or linkstart, the link remains in state Ready.
        linkstart : IN STD_LOGIC;

        -- Do not start link (overrides linkstart and autostart) and/or
        -- disconnect a running link.
        linkdis : IN STD_LOGIC;

        -- Scaling factor minus 1, used to scale the transmit base clock into
        -- the transmission bit rate. The system clock (for impl_generic) or
        -- the txclk (for impl_fast) is divided by (unsigned(txdivcnt) + 1).
        -- Changing this signal will immediately change the transmission rate.
        -- During link setup, the transmission rate is always 10 Mbit/s.
        txdivcnt : IN STD_LOGIC_VECTOR(7 DOWNTO 0);

        -- High for one clock cycle to request transmission of a TimeCode.
        -- The request is registered inside the entity until it can be processed.
        tick_in : IN STD_LOGIC;

        -- Control bits of the TimeCode to be sent. Must be valid while tick_in is high.
        ctrl_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);

        -- Counter value of the TimeCode to be sent. Must be valid while tick_in is high.
        time_in : IN STD_LOGIC_VECTOR(5 DOWNTO 0);

        -- Pulled high by the application to write an N-Char to the transmit
        -- queue. If "txwrite" and "txrdy" are both high on the rising edge
        -- of "clk", a character is added to the transmit queue.
        -- This signal has no effect if "txrdy" is low.
        txwrite : IN STD_LOGIC;

        -- Control flag to be sent with the next N_Char.
        -- Must be valid while txwrite is high.
        txflag : IN STD_LOGIC;

        -- Byte to be sent, or "00000000" for EOP or "00000001" for EEP.
        -- Must be valid while txwrite is high.
        txdata : IN STD_LOGIC_VECTOR(7 DOWNTO 0);

        -- High if the entity is ready to accept an N-Char for transmission.
        txrdy : OUT STD_LOGIC;

        -- High if the transmission queue is at least half full.
        txhalff : OUT STD_LOGIC;

        -- High for one clock cycle if a TimeCode was just received.
        tick_out : OUT STD_LOGIC;

        -- Control bits of the last received TimeCode.
        ctrl_out : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);

        -- Counter value of the last received TimeCode.
        time_out : OUT STD_LOGIC_VECTOR(5 DOWNTO 0);

        -- High if "rxflag" and "rxdata" contain valid data.
        -- This signal is high unless the receive FIFO is empty.
        rxvalid : OUT STD_LOGIC;

        -- High if the receive FIFO is at least half full.
        rxhalff : OUT STD_LOGIC;

        -- High if the received character is EOP or EEP; low if the received
        -- character is a data byte. Valid if "rxvalid" is high.
        rxflag : OUT STD_LOGIC;

        -- Received byte, or "00000000" for EOP or "00000001" for EEP.
        -- Valid if "rxvalid" is high.
        rxdata : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);

        -- Pulled high by the application to accept a received character.
        -- If "rxvalid" and "rxread" are both high on the rising edge of "clk",
        -- a character is removed from the receive FIFO and "rxvalid", "rxflag"
        -- and "rxdata" are updated.
        -- This signal has no effect if "rxvalid" is low.
        rxread : IN STD_LOGIC;

        -- High if the link state machine is currently in the Started state.
        started : OUT STD_LOGIC;

        -- High if the link state machine is currently in the Connecting state.
        connecting : OUT STD_LOGIC;

        -- High if the link state machine is currently in the Run state, indicating
        -- that the link is fully operational. If none of started, connecting or running
        -- is high, the link is in an initial state and the transmitter is not yet enabled.
        running : OUT STD_LOGIC;

        -- Disconnect detected in state Run. Triggers a reset and reconnect of the link.
        -- This indication is auto-clearing.
        errdisc : OUT STD_LOGIC;

        -- Parity error detected in state Run. Triggers a reset and reconnect of the link.
        -- This indication is auto-clearing.
        errpar : OUT STD_LOGIC;

        -- Invalid escape sequence detected in state Run. Triggers a reset and reconnect of
        -- the link. This indication is auto-clearing.
        erresc : OUT STD_LOGIC;

        -- Credit error detected. Triggers a reset and reconnect of the link.
        -- This indication is auto-clearing.
        errcred : OUT STD_LOGIC;

        -- Data In signal from SpaceWire bus.
        spw_di : IN STD_LOGIC;

        -- Strobe In signal from SpaceWire bus.
        spw_si : IN STD_LOGIC;

        -- Data Out signal to SpaceWire bus.
        spw_do : OUT STD_LOGIC;

        -- Strobe Out signal to SpaceWire bus.
        spw_so : OUT STD_LOGIC
    );

END ENTITY spwstream;

ARCHITECTURE spwstream_arch OF spwstream IS

    -- Convert boolean to std_logic.
    TYPE bool_to_logic_type IS ARRAY(BOOLEAN) OF STD_ULOGIC;
    CONSTANT bool_to_logic : bool_to_logic_type := (false => '0', true => '1');

    -- Reset time (6.4 us) in system clocks
    CONSTANT reset_time : INTEGER := INTEGER(sysfreq * 6.4e-6);

    -- Disconnect time (850 ns) in system clocks
    CONSTANT disconnect_time : INTEGER := INTEGER(sysfreq * 850.0e-9);

    -- Initial tx clock scaler (10 Mbit).
    TYPE impl_to_real_type IS ARRAY(spw_implementation_type) OF real;
    CONSTANT tximpl_to_txclk_freq : impl_to_real_type :=
    (impl_generic => sysfreq, impl_fast => txclkfreq);
    CONSTANT effective_txclk_freq : real := tximpl_to_txclk_freq(tximpl);
    CONSTANT default_divcnt : STD_LOGIC_VECTOR(7 DOWNTO 0) :=
    STD_LOGIC_VECTOR(to_unsigned(INTEGER(effective_txclk_freq / 10.0e6 - 1.0), 8));

    -- Registers.
    TYPE regs_type IS RECORD
        -- packet state
        rxpacket : STD_LOGIC; -- '1' when receiving a packet
        rxeep : STD_LOGIC; -- '1' when rx EEP character pending
        txpacket : STD_LOGIC; -- '1' when transmitting a packet
        txdiscard : STD_LOGIC; -- '1' when discarding a tx packet
        -- FIFO pointers
        rxfifo_raddr : STD_LOGIC_VECTOR(rxfifosize_bits - 1 DOWNTO 0);
        rxfifo_waddr : STD_LOGIC_VECTOR(rxfifosize_bits - 1 DOWNTO 0);
        txfifo_raddr : STD_LOGIC_VECTOR(txfifosize_bits - 1 DOWNTO 0);
        txfifo_waddr : STD_LOGIC_VECTOR(txfifosize_bits - 1 DOWNTO 0);
        -- FIFO state
        rxfifo_rvalid : STD_LOGIC; -- '1' if s_rxfifo_rdata is valid
        txfifo_rvalid : STD_LOGIC; -- '1' if s_txfifo_rdata is valid
        rxfull : STD_LOGIC; -- '1' if RX fifo is full
        rxhalff : STD_LOGIC; -- '1' if RX fifo is at least half full
        txfull : STD_LOGIC; -- '1' if TX fifo is full
        txhalff : STD_LOGIC; -- '1' if TX fifo is at least half full
        rxroom : STD_LOGIC_VECTOR(5 DOWNTO 0);
    END RECORD;

    CONSTANT regs_reset : regs_type := (
        rxpacket => '0',
        rxeep => '0',
        txpacket => '0',
        txdiscard => '0',
        rxfifo_raddr => (OTHERS => '0'),
        rxfifo_waddr => (OTHERS => '0'),
        txfifo_raddr => (OTHERS => '0'),
        txfifo_waddr => (OTHERS => '0'),
        rxfifo_rvalid => '0',
        txfifo_rvalid => '0',
        rxfull => '0',
        rxhalff => '0',
        txfull => '0',
        txhalff => '0',
        rxroom => (OTHERS => '0'));

    SIGNAL r : regs_type := regs_reset;
    SIGNAL rin : regs_type;

    -- Interface signals to components.
    SIGNAL recv_rxen : STD_LOGIC;
    SIGNAL recvo : spw_recv_out_type;
    SIGNAL recv_inact : STD_LOGIC;
    SIGNAL recv_inbvalid : STD_LOGIC;
    SIGNAL recv_inbits : STD_LOGIC_VECTOR(rxchunk - 1 DOWNTO 0);
    SIGNAL xmiti : spw_xmit_in_type;
    SIGNAL xmito : spw_xmit_out_type;
    SIGNAL xmit_divcnt : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL linki : spw_link_in_type;
    SIGNAL linko : spw_link_out_type;

    -- Memory interface signals.
    SIGNAL s_rxfifo_raddr : STD_LOGIC_VECTOR(rxfifosize_bits - 1 DOWNTO 0);
    SIGNAL s_rxfifo_rdata : STD_LOGIC_VECTOR(8 DOWNTO 0);
    SIGNAL s_rxfifo_wen : STD_LOGIC;
    SIGNAL s_rxfifo_waddr : STD_LOGIC_VECTOR(rxfifosize_bits - 1 DOWNTO 0);
    SIGNAL s_rxfifo_wdata : STD_LOGIC_VECTOR(8 DOWNTO 0);
    SIGNAL s_txfifo_raddr : STD_LOGIC_VECTOR(txfifosize_bits - 1 DOWNTO 0);
    SIGNAL s_txfifo_rdata : STD_LOGIC_VECTOR(8 DOWNTO 0);
    SIGNAL s_txfifo_wen : STD_LOGIC;
    SIGNAL s_txfifo_waddr : STD_LOGIC_VECTOR(txfifosize_bits - 1 DOWNTO 0);
    SIGNAL s_txfifo_wdata : STD_LOGIC_VECTOR(8 DOWNTO 0);

BEGIN

    -- Instantiate link controller.
    link_inst : spwlink
    GENERIC MAP(
        reset_time => reset_time)
    PORT MAP(
        clk => clk,
        rst => rst,
        linki => linki,
        linko => linko,
        rxen => recv_rxen,
        recvo => recvo,
        xmiti => xmiti,
        xmito => xmito);

    -- Instantiate receiver.
    recv_inst : spwrecv
    GENERIC MAP(
        disconnect_time => disconnect_time,
        rxchunk => rxchunk)
    PORT MAP(
        clk => clk,
        rxen => recv_rxen,
        recvo => recvo,
        inact => recv_inact,
        inbvalid => recv_inbvalid,
        inbits => recv_inbits);

    -- Instantiate transmitter.
    xmit_sel0 : IF tximpl = impl_generic GENERATE
        xmit_inst : spwxmit
        PORT MAP(
            clk => clk,
            rst => rst,
            divcnt => xmit_divcnt,
            xmiti => xmiti,
            xmito => xmito,
            spw_do => spw_do,
            spw_so => spw_so);
    END GENERATE;
    xmit_sel1 : IF tximpl = impl_fast GENERATE
        xmit_fast_inst : spwxmit_fast
        PORT MAP(
            clk => clk,
            txclk => txclk,
            rst => rst,
            divcnt => xmit_divcnt,
            xmiti => xmiti,
            xmito => xmito,
            spw_do => spw_do,
            spw_so => spw_so);
    END GENERATE;

    -- Instantiate receiver front-end.
    recvfront_sel0 : IF rximpl = impl_generic GENERATE
        recvfront_generic_inst : spwrecvfront_generic
        PORT MAP(
            clk => clk,
            rxen => recv_rxen,
            inact => recv_inact,
            inbvalid => recv_inbvalid,
            inbits => recv_inbits,
            spw_di => spw_di,
            spw_si => spw_si);
    END GENERATE;
    recvfront_sel1 : IF rximpl = impl_fast GENERATE
        recvfront_fast_inst : spwrecvfront_fast
        GENERIC MAP(
            rxchunk => rxchunk)
        PORT MAP(
            clk => clk,
            rxclk => rxclk,
            rxen => recv_rxen,
            inact => recv_inact,
            inbvalid => recv_inbvalid,
            inbits => recv_inbits,
            spw_di => spw_di,
            spw_si => spw_si);
    END GENERATE;

    -- Instantiate RX memory.
    rxmem : spwram
    GENERIC MAP(
        abits => rxfifosize_bits,
        dbits => 9)
    PORT MAP(
        rclk => clk,
        wclk => clk,
        ren => '1',
        raddr => s_rxfifo_raddr,
        rdata => s_rxfifo_rdata,
        wen => s_rxfifo_wen,
        waddr => s_rxfifo_waddr,
        wdata => s_rxfifo_wdata);

    -- Instantiate TX memory.
    txmem : spwram
    GENERIC MAP(
        abits => txfifosize_bits,
        dbits => 9)
    PORT MAP(
        rclk => clk,
        wclk => clk,
        ren => '1',
        raddr => s_txfifo_raddr,
        rdata => s_txfifo_rdata,
        wen => s_txfifo_wen,
        waddr => s_txfifo_waddr,
        wdata => s_txfifo_wdata);

    -- Combinatorial process
    PROCESS (r, linko, s_rxfifo_rdata, s_txfifo_rdata, rst, autostart, linkstart, linkdis, txdivcnt, tick_in, ctrl_in, time_in, txwrite, txflag, txdata, rxread) IS
        VARIABLE v : regs_type;
        VARIABLE v_tmprxroom : unsigned(rxfifosize_bits - 1 DOWNTO 0);
        VARIABLE v_tmptxroom : unsigned(txfifosize_bits - 1 DOWNTO 0);
    BEGIN
        v := r;
        v_tmprxroom := to_unsigned(0, v_tmprxroom'length);
        v_tmptxroom := to_unsigned(0, v_tmptxroom'length);

        -- Keep track of whether we are sending and/or receiving a packet.
        IF linko.rxchar = '1' THEN
            -- got character
            v.rxpacket := NOT linko.rxflag;
        END IF;
        IF linko.txack = '1' THEN
            -- send character
            v.txpacket := NOT s_txfifo_rdata(8);
        END IF;

        -- Update RX fifo pointers.
        IF (rxread = '1') AND (r.rxfifo_rvalid = '1') THEN
            -- read from fifo
            v.rxfifo_raddr := STD_LOGIC_VECTOR(unsigned(r.rxfifo_raddr) + 1);
        END IF;
        IF r.rxfull = '0' THEN
            IF (linko.rxchar = '1') OR (r.rxeep = '1') THEN
                -- write to fifo (received char or pending EEP)
                v.rxfifo_waddr := STD_LOGIC_VECTOR(unsigned(r.rxfifo_waddr) + 1);
            END IF;
            v.rxeep := '0';
        END IF;

        -- Keep track of whether the RX fifo contains valid data.
        -- (use new value of rxfifo_raddr)
        v.rxfifo_rvalid := bool_to_logic(v.rxfifo_raddr /= r.rxfifo_waddr);

        -- Update room in RX fifo (use new value of rxfifo_waddr).
        v_tmprxroom := unsigned(r.rxfifo_raddr) - unsigned(v.rxfifo_waddr) - 1;
        v.rxfull := bool_to_logic(v_tmprxroom = 0);
        v.rxhalff := NOT v_tmprxroom(v_tmprxroom'high);
        IF v_tmprxroom > 63 THEN
            v.rxroom := (OTHERS => '1');
        ELSE
            v.rxroom := STD_LOGIC_VECTOR(v_tmprxroom(5 DOWNTO 0));
        END IF;

        -- Update TX fifo pointers.
        IF (r.txfifo_rvalid = '1') AND ((linko.txack = '1') OR (r.txdiscard = '1')) THEN
            -- read from fifo
            v.txfifo_raddr := STD_LOGIC_VECTOR(unsigned(r.txfifo_raddr) + 1);
            IF s_txfifo_rdata(8) = '1' THEN
                v.txdiscard := '0'; -- got EOP/EEP, stop discarding data
            END IF;
        END IF;
        IF (r.txfull = '0') AND (txwrite = '1') THEN
            -- write to fifo
            v.txfifo_waddr := STD_LOGIC_VECTOR(unsigned(r.txfifo_waddr) + 1);
        END IF;

        -- Keep track of whether the TX fifo contains valid data.
        -- (use new value of txfifo_raddr)
        v.txfifo_rvalid := bool_to_logic(v.txfifo_raddr /= r.txfifo_waddr);

        -- Update room in TX fifo (use new value of txfifo_waddr).
        v_tmptxroom := unsigned(r.txfifo_raddr) - unsigned(v.txfifo_waddr) - 1;
        v.txfull := bool_to_logic(v_tmptxroom = 0);
        v.txhalff := NOT v_tmptxroom(v_tmptxroom'high);

        -- If the link is lost, set a flag to discard the current packet.
        IF linko.running = '0' THEN
            v.rxeep := v.rxeep OR v.rxpacket; -- use new value of rxpacket
            v.txdiscard := v.txdiscard OR v.txpacket; -- use new value of txpacket
            v.rxpacket := '0';
            v.txpacket := '0';
        END IF;

        -- Clear the discard flag when the link is explicitly disabled.
        IF linkdis = '1' THEN
            v.txdiscard := '0';
        END IF;

        -- Drive control signals to RX fifo.
        s_rxfifo_raddr <= v.rxfifo_raddr; -- using new value of rxfifo_raddr
        s_rxfifo_wen <= (NOT r.rxfull) AND (linko.rxchar OR r.rxeep);
        s_rxfifo_waddr <= r.rxfifo_waddr;
        IF r.rxeep = '1' THEN
            s_rxfifo_wdata <= "100000001";
        ELSE
            s_rxfifo_wdata <= linko.rxflag & linko.rxdata;
        END IF;

        -- Drive control signals to TX fifo.
        s_txfifo_raddr <= v.txfifo_raddr; -- using new value of txfifo_raddr
        s_txfifo_wen <= (NOT r.txfull) AND txwrite;
        s_txfifo_waddr <= r.txfifo_waddr;
        s_txfifo_wdata <= txflag & txdata;

        -- Drive inputs to spwlink.
        linki.autostart <= autostart;
        linki.linkstart <= linkstart;
        linki.linkdis <= linkdis;
        linki.rxroom <= r.rxroom;
        linki.tick_in <= tick_in;
        linki.ctrl_in <= ctrl_in;
        linki.time_in <= time_in;
        linki.txwrite <= r.txfifo_rvalid AND NOT r.txdiscard;
        linki.txflag <= s_txfifo_rdata(8);
        linki.txdata <= s_txfifo_rdata(7 DOWNTO 0);

        -- Drive divcnt input to spwxmit.
        IF linko.running = '1' THEN
            xmit_divcnt <= txdivcnt;
        ELSE
            xmit_divcnt <= default_divcnt;
        END IF;

        -- Drive outputs.
        txrdy <= NOT r.txfull;
        txhalff <= r.txhalff;
        tick_out <= linko.tick_out;
        ctrl_out <= linko.ctrl_out;
        time_out <= linko.time_out;
        rxvalid <= r.rxfifo_rvalid;
        rxhalff <= r.rxhalff;
        rxflag <= s_rxfifo_rdata(8);
        rxdata <= s_rxfifo_rdata(7 DOWNTO 0);
        started <= linko.started;
        connecting <= linko.connecting;
        running <= linko.running;
        errdisc <= linko.errdisc;
        errpar <= linko.errpar;
        erresc <= linko.erresc;
        errcred <= linko.errcred;

        -- Reset.
        IF rst = '1' THEN
            v.rxpacket := '0';
            v.rxeep := '0';
            v.txpacket := '0';
            v.txdiscard := '0';
            v.rxfifo_raddr := (OTHERS => '0');
            v.rxfifo_waddr := (OTHERS => '0');
            v.txfifo_raddr := (OTHERS => '0');
            v.txfifo_waddr := (OTHERS => '0');
            v.rxfifo_rvalid := '0';
            v.txfifo_rvalid := '0';
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

END ARCHITECTURE spwstream_arch;