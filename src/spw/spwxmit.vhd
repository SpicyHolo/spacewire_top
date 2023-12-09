--
--  SpaceWire Transmitter
--
--  This entity translates outgoing characters and tokens into
--  data-strobe signalling.
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.spwpkg.ALL;

ENTITY spwxmit IS

    PORT (
        -- System clock.
        clk : IN STD_LOGIC;

        -- Synchronous reset (active-high).
        rst : IN STD_LOGIC;

        -- Scaling factor minus 1, used to scale the system clock into the
        -- transmission bit rate. The system clock is divided by
        -- (unsigned(divcnt) + 1). Changing this signal will immediately
        -- change the transmission rate.
        divcnt : IN STD_LOGIC_VECTOR(7 DOWNTO 0);

        -- Input signals from spwlink.
        xmiti : IN spw_xmit_in_type;

        -- Output signals to spwlink.
        xmito : OUT spw_xmit_out_type;

        -- Data Out signal to SpaceWire bus.
        spw_do : OUT STD_LOGIC;

        -- Strobe Out signal to SpaceWire bus.
        spw_so : OUT STD_LOGIC
    );

END ENTITY spwxmit;

ARCHITECTURE spwxmit_arch OF spwxmit IS

    -- Registers
    TYPE regs_type IS RECORD
        -- tx clock
        txclken : STD_ULOGIC; -- high if a bit must be transmitted
        txclkcnt : unsigned(7 DOWNTO 0);
        -- output shift register
        bitshift : STD_LOGIC_VECTOR(12 DOWNTO 0);
        bitcnt : unsigned(3 DOWNTO 0);
        -- output signals
        out_data : STD_ULOGIC;
        out_strobe : STD_ULOGIC;
        -- parity flag
        parity : STD_ULOGIC;
        -- pending time tick
        pend_tick : STD_ULOGIC;
        pend_time : STD_LOGIC_VECTOR(7 DOWNTO 0);
        -- transmitter mode
        allow_fct : STD_ULOGIC; -- allowed to send FCTs
        allow_char : STD_ULOGIC; -- allowed to send data and time
        sent_null : STD_ULOGIC; -- sent at least one NULL token
        sent_fct : STD_ULOGIC; -- sent at least one FCT token
    END RECORD;

    -- Initial state
    CONSTANT regs_reset : regs_type := (
        txclken => '0',
        txclkcnt => "00000000",
        bitshift => (OTHERS => '0'),
        bitcnt => "0000",
        out_data => '0',
        out_strobe => '0',
        parity => '0',
        pend_tick => '0',
        pend_time => (OTHERS => '0'),
        allow_fct => '0',
        allow_char => '0',
        sent_null => '0',
        sent_fct => '0');

    -- Registers
    SIGNAL r : regs_type := regs_reset;
    SIGNAL rin : regs_type;

BEGIN

    -- Combinatorial process
    PROCESS (r, rst, divcnt, xmiti) IS
        VARIABLE v : regs_type;
    BEGIN
        v := r;

        -- Generate TX clock.
        IF r.txclkcnt = 0 THEN
            v.txclkcnt := unsigned(divcnt);
            v.txclken := '1';
        ELSE
            v.txclkcnt := r.txclkcnt - 1;
            v.txclken := '0';
        END IF;

        IF xmiti.txen = '0' THEN

            -- Transmitter disabled; reset state.
            v.bitcnt := "0000";
            v.parity := '0';
            v.pend_tick := '0';
            v.allow_fct := '0';
            v.allow_char := '0';
            v.sent_null := '0';
            v.sent_fct := '0';

            -- Gentle reset of spacewire bus signals
            IF r.txclken = '1' THEN
                v.out_data := r.out_data AND r.out_strobe;
                v.out_strobe := '0';
            END IF;

        ELSE
            -- Transmitter enabled.

            v.allow_fct := (NOT xmiti.stnull) AND r.sent_null;
            v.allow_char := (NOT xmiti.stnull) AND r.sent_null AND
            (NOT xmiti.stfct) AND r.sent_fct;

            -- On tick of transmission clock, put next bit on the output.
            IF r.txclken = '1' THEN

                IF r.bitcnt = 0 THEN

                    -- Need to start a new character.
                    IF (r.allow_char = '1') AND (r.pend_tick = '1') THEN
                        -- Send Time-Code.
                        v.out_data := r.parity;
                        v.bitshift(12 DOWNTO 5) := r.pend_time;
                        v.bitshift(4 DOWNTO 0) := "01111";
                        v.bitcnt := to_unsigned(13, v.bitcnt'length);
                        v.parity := '0';
                        v.pend_tick := '0';
                    ELSIF (r.allow_fct = '1') AND (xmiti.fct_in = '1') THEN
                        -- Send FCT.
                        v.out_data := r.parity;
                        v.bitshift(2 DOWNTO 0) := "001";
                        v.bitcnt := to_unsigned(3, v.bitcnt'length);
                        v.parity := '1';
                        v.sent_fct := '1';
                    ELSIF (r.allow_char = '1') AND (xmiti.txwrite = '1') THEN
                        -- Send N-Char.
                        v.bitshift(0) := xmiti.txflag;
                        v.parity := xmiti.txflag;
                        IF xmiti.txflag = '0' THEN
                            -- Data byte
                            v.out_data := NOT r.parity;
                            v.bitshift(8 DOWNTO 1) := xmiti.txdata;
                            v.bitcnt := to_unsigned(9, v.bitcnt'length);
                        ELSE
                            -- EOP or EEP
                            v.out_data := r.parity;
                            v.bitshift(1) := xmiti.txdata(0);
                            v.bitshift(2) := NOT xmiti.txdata(0);
                            v.bitcnt := to_unsigned(3, v.bitcnt'length);
                        END IF;
                    ELSE
                        -- Send NULL.
                        v.out_data := r.parity;
                        v.bitshift(6 DOWNTO 0) := "0010111";
                        v.bitcnt := to_unsigned(7, v.bitcnt'length);
                        v.parity := '0';
                        v.sent_null := '1';
                    END IF;

                ELSE

                    -- Shift next bit to the output.
                    v.out_data := r.bitshift(0);
                    v.parity := r.parity XOR r.bitshift(0);
                    v.bitshift(r.bitshift'high - 1 DOWNTO 0) := r.bitshift(r.bitshift'high DOWNTO 1);
                    v.bitcnt := r.bitcnt - 1;

                END IF;

                -- Data-Strobe encoding.
                v.out_strobe := NOT (r.out_strobe XOR r.out_data XOR v.out_data);

            END IF;

            -- Store requests for time tick transmission.
            IF xmiti.tick_in = '1' THEN
                v.pend_tick := '1';
                v.pend_time := xmiti.ctrl_in & xmiti.time_in;
            END IF;

        END IF;

        -- Synchronous reset
        IF rst = '1' THEN
            v := regs_reset;
        END IF;

        -- Drive outputs.
        -- Note: the outputs are combinatorially dependent on certain inputs.

        -- Set fctack high if (transmitter enabled) AND
        -- (ready for token) AND (FCTs allowed) AND
        -- ((characters not allowed) OR (no timecode pending)) AND
        -- (FCT requested)
        IF (xmiti.txen = '1') AND
            (r.txclken = '1') AND (r.bitcnt = 0) AND (r.allow_fct = '1') AND
            ((r.allow_char = '0') OR (r.pend_tick = '0')) THEN
            xmito.fctack <= xmiti.fct_in;
        ELSE
            xmito.fctack <= '0';
        END IF;

        -- Set txrdy high if (transmitter enabled) AND
        -- (ready for token) AND (characters enabled) AND
        -- (no timecode pending) AND (no FCT requested) AND
        -- (character requested)
        IF (xmiti.txen = '1') AND
            (r.txclken = '1') AND (r.bitcnt = 0) AND (r.allow_char = '1') AND
            (r.pend_tick = '0') AND (xmiti.fct_in = '0') THEN
            xmito.txack <= xmiti.txwrite;
        ELSE
            xmito.txack <= '0';
        END IF;

        -- Update registers
        rin <= v;
    END PROCESS;

    -- Synchronous process
    PROCESS (clk) IS
    BEGIN
        IF rising_edge(clk) THEN

            -- Update registers
            r <= rin;

            -- Drive spacewire output signals
            spw_do <= r.out_data;
            spw_so <= r.out_strobe;

        END IF;
    END PROCESS;

END ARCHITECTURE spwxmit_arch;