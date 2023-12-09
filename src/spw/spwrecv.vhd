--
--  SpaceWire Receiver
--
--  This entity decodes the sequence of incoming data bits into tokens.
--  Data bits are passed to this entity from the Receiver Front-end
--  in groups of rxchunk bits at a time.
--
--  The bitrate of the incoming SpaceWire signal must be strictly less
--  than rxchunk times the system clock frequency. 
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL, ieee.numeric_std.ALL;
USE work.spwpkg.ALL;

ENTITY spwrecv IS

    GENERIC (
        -- Disconnect timeout, expressed in system clock cycles.
        -- Should be 850 ns (727 ns .. 1000 ns) according to the standard.
        disconnect_time : INTEGER RANGE 1 TO 255;

        -- Nr of bits sampled per system clock.
        rxchunk : INTEGER RANGE 1 TO 4
    );

    PORT (
        -- System clock.
        clk : IN STD_LOGIC;

        -- High to enable receiver; low to disable and reset receiver.
        rxen : IN STD_LOGIC;

        -- Output signals to spwlink.
        recvo : OUT spw_recv_out_type;

        -- High if there has been recent activity on the input lines.
        inact : IN STD_LOGIC;

        -- High if inbits contains a valid group of received bits.
        inbvalid : IN STD_LOGIC;

        -- Received bits from receiver front-end.
        inbits : IN STD_LOGIC_VECTOR(rxchunk - 1 DOWNTO 0)
    );

END ENTITY spwrecv;

ARCHITECTURE spwrecv_arch OF spwrecv IS

    -- registers
    TYPE regs_type IS RECORD
        -- receiver state
        bit_seen : STD_ULOGIC; -- got a bit transition
        null_seen : STD_ULOGIC; -- got a NULL token
        -- input shift register
        bitshift : STD_LOGIC_VECTOR(8 DOWNTO 0);
        bitcnt : STD_LOGIC_VECTOR(9 DOWNTO 0); -- one-hot counter
        -- parity flag
        parity : STD_ULOGIC;
        -- decoding
        control : STD_ULOGIC; -- next code is control code
        escaped : STD_ULOGIC; -- last code was ESC
        -- output registers
        gotfct : STD_ULOGIC;
        tick_out : STD_ULOGIC;
        rxchar : STD_ULOGIC;
        rxflag : STD_ULOGIC;
        timereg : STD_LOGIC_VECTOR(7 DOWNTO 0);
        datareg : STD_LOGIC_VECTOR(7 DOWNTO 0);
        -- disconnect timer
        disccnt : unsigned(7 DOWNTO 0);
        -- error flags
        errpar : STD_ULOGIC;
        erresc : STD_ULOGIC;
    END RECORD;

    -- Initial state
    CONSTANT regs_reset : regs_type := (
        bit_seen => '0',
        null_seen => '0',
        bitshift => (OTHERS => '1'),
        bitcnt => (OTHERS => '0'),
        parity => '0',
        control => '0',
        escaped => '0',
        gotfct => '0',
        tick_out => '0',
        rxchar => '0',
        rxflag => '0',
        timereg => (OTHERS => '0'),
        datareg => (OTHERS => '0'),
        disccnt => "00000000",
        errpar => '0',
        erresc => '0');

    -- registers
    SIGNAL r : regs_type := regs_reset;
    SIGNAL rin : regs_type;

BEGIN

    -- combinatorial process
    PROCESS (r, rxen, inact, inbvalid, inbits)
        VARIABLE v : regs_type;
        VARIABLE v_inbit : STD_ULOGIC;
    BEGIN
        v := r;
        v_inbit := '0';

        -- disconnect timer
        IF inact = '1' THEN
            -- activity on input; reset timer
            v.disccnt := to_unsigned(disconnect_time, v.disccnt'length);
        ELSIF r.disccnt /= 0 THEN
            -- count down
            v.disccnt := r.disccnt - 1;
        END IF;

        -- assume no new token
        v.gotfct := '0';
        v.tick_out := '0';
        v.rxchar := '0';

        IF inbvalid = '1' THEN

            -- process incoming bits
            FOR i IN 0 TO rxchunk - 1 LOOP
                v_inbit := inbits(i);

                -- got a bit transition
                v.bit_seen := '1';

                IF v.bitcnt(0) = '1' THEN
                    -- received new token
                    -- note that this will not happen before null_seen='1'
                    IF (v.parity XOR v_inbit) = '0' THEN
                        -- Parity check failed.
                        v.errpar := '1';
                    ELSE
                        IF v.control = '1' THEN
                            -- received control code
                            CASE v.bitshift(7 DOWNTO 6) IS
                                WHEN "00" => -- FCT or NULL
                                    v.gotfct := NOT r.escaped;
                                    v.escaped := '0';
                                WHEN "10" => -- EOP
                                    IF r.escaped = '1' THEN
                                        v.erresc := '1';
                                    END IF;
                                    v.escaped := '0';
                                    v.rxchar := NOT r.escaped;
                                    v.rxflag := '1';
                                    v.datareg := "00000000";
                                WHEN "01" => -- EEP
                                    IF r.escaped = '1' THEN
                                        v.erresc := '1';
                                    END IF;
                                    v.escaped := '0';
                                    v.rxchar := NOT r.escaped;
                                    v.rxflag := '1';
                                    v.datareg := "00000001";
                                WHEN OTHERS => -- ESC
                                    IF r.escaped = '1' THEN
                                        v.erresc := '1';
                                    END IF;
                                    v.escaped := '1';
                            END CASE;
                        ELSE
                            -- received 8-bit character
                            IF r.escaped = '1' THEN
                                -- received Time-Code
                                v.tick_out := '1';
                                v.timereg := v.bitshift(7 DOWNTO 0);
                            ELSE
                                -- received data character
                                v.rxflag := '0';
                                v.rxchar := '1';
                                v.datareg := v.bitshift(7 DOWNTO 0);
                            END IF;
                            v.escaped := '0';
                        END IF;
                    END IF;
                    -- prepare for next code
                    v.parity := '0';
                    v.control := v_inbit;
                    IF v_inbit = '1' THEN
                        -- next word will be control code.
                        v.bitcnt := (3 => '1', OTHERS => '0');
                    ELSE
                        -- next word will be a data byte.
                        v.bitcnt := (9 => '1', OTHERS => '0');
                    END IF;
                ELSE
                    -- wait until next code is completely received;
                    -- accumulate parity
                    v.bitcnt := '0' & v.bitcnt(9 DOWNTO 1);
                    v.parity := v.parity XOR v_inbit;
                END IF;

                -- detect first NULL
                IF v.null_seen = '0' THEN
                    IF v.bitshift = "000101110" THEN
                        -- got first NULL pattern
                        v.null_seen := '1';
                        v.control := v_inbit; -- should always be '1'
                        v.parity := '0';
                        v.bitcnt := (3 => '1', OTHERS => '0');
                    END IF;
                END IF;

                -- shift new bit into register.
                v.bitshift := v_inbit & v.bitshift(v.bitshift'high DOWNTO 1);

            END LOOP;
        END IF;

        -- synchronous reset
        IF rxen = '0' THEN
            v.bit_seen := '0';
            v.null_seen := '0';
            v.bitshift := "111111111";
            v.bitcnt := (OTHERS => '0');
            v.gotfct := '0';
            v.tick_out := '0';
            v.rxchar := '0';
            v.rxflag := '0';
            v.escaped := '0';
            v.timereg := "00000000";
            v.datareg := "00000000";
            v.disccnt := to_unsigned(0, v.disccnt'length);
            v.errpar := '0';
            v.erresc := '0';
        END IF;

        -- drive outputs
        recvo.gotbit <= r.bit_seen;
        recvo.gotnull <= r.null_seen;
        recvo.gotfct <= r.gotfct;
        recvo.tick_out <= r.tick_out;
        recvo.ctrl_out <= r.timereg(7 DOWNTO 6);
        recvo.time_out <= r.timereg(5 DOWNTO 0);
        recvo.rxchar <= r.rxchar;
        recvo.rxflag <= r.rxflag;
        recvo.rxdata <= r.datareg;
        IF r.bit_seen = '1' AND r.disccnt = 0 THEN
            recvo.errdisc <= '1';
        ELSE
            recvo.errdisc <= '0';
        END IF;
        recvo.errpar <= r.errpar;
        recvo.erresc <= r.erresc;

        -- update registers
        rin <= v;

    END PROCESS;

    -- update registers on rising edge of system clock
    PROCESS (clk) IS
    BEGIN
        IF rising_edge(clk) THEN
            r <= rin;
        END IF;
    END PROCESS;

END ARCHITECTURE spwrecv_arch;