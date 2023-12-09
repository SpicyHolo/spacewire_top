--
--  SpaceWire Exchange Level Controller.
--
--  This entity implements exchange level aspects of the SpaceWire protocol.
--  It handles connection setup, error detection and flow control.
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.spwpkg.ALL;

ENTITY spwlink IS

    GENERIC (
        -- Reset time expressed in system clock cycles.
        -- Should be 6.4 us (5.82 us .. 7.2 us) according to the standard.
        reset_time : INTEGER
    );

    PORT (
        -- System clock.
        clk : IN STD_LOGIC;

        -- Synchronous reset (active-high).
        -- Disconnects, resets error conditions, puts the link state machine
        -- in state ErrorReset.
        rst : IN STD_LOGIC;

        -- Link level inputs.
        linki : IN spw_link_in_type;

        -- Link level outputs.
        linko : OUT spw_link_out_type;

        -- Receiver enable signal to spwrecv.
        rxen : OUT STD_LOGIC;

        -- Output signals from spwrecv.
        recvo : IN spw_recv_out_type;

        -- Input signals for spwxmit.
        xmiti : OUT spw_xmit_in_type;

        -- Output signals from spwxmit.
        xmito : IN spw_xmit_out_type
    );

END ENTITY spwlink;

ARCHITECTURE spwlink_arch OF spwlink IS

    -- Convert boolean to std_logic.
    TYPE bool_to_logic_type IS ARRAY(BOOLEAN) OF STD_ULOGIC;
    CONSTANT bool_to_logic : bool_to_logic_type := (false => '0', true => '1');

    -- State machine.
    TYPE state_type IS (
        S_ErrorReset, S_ErrorWait, S_Ready, S_Started, S_Connecting, S_Run);

    -- Registers
    TYPE regs_type IS RECORD
        -- state machine
        state : state_type;
        -- credit accounting
        tx_credit : unsigned(5 DOWNTO 0);
        rx_credit : unsigned(5 DOWNTO 0);
        errcred : STD_ULOGIC;
        -- reset timer
        timercnt : unsigned(10 DOWNTO 0);
        timerdone : STD_ULOGIC;
        -- signal to transmitter
        xmit_fct_in : STD_ULOGIC;
    END RECORD;

    -- Initial state
    CONSTANT regs_reset : regs_type := (
        state => S_ErrorReset,
        tx_credit => "000000",
        rx_credit => "000000",
        errcred => '0',
        timercnt => to_unsigned(reset_time, 11),
        timerdone => '0',
        xmit_fct_in => '0');

    SIGNAL r : regs_type := regs_reset;
    SIGNAL rin : regs_type;

BEGIN

    -- Combinatorial process
    PROCESS (r, rst, linki, recvo, xmito) IS
        VARIABLE v : regs_type;
        VARIABLE v_timerrst : STD_LOGIC;
    BEGIN
        v := r;
        v_timerrst := '0';

        -- State machine.
        CASE r.state IS

            WHEN S_ErrorReset =>
                -- Wait for timer.
                IF r.timercnt = 0 THEN
                    v.state := S_ErrorWait;
                    v_timerrst := '1';
                END IF;
                v.errcred := '0';
                v.xmit_fct_in := '0';

            WHEN S_ErrorWait =>
                -- Wait for 2 timer periods.
                IF ((recvo.errdisc OR recvo.errpar OR recvo.erresc) = '1') OR
                    ((recvo.gotfct OR recvo.tick_out OR recvo.rxchar) = '1') THEN
                    -- Note: spwrecv will never issue errpar, erresc, gotfct,
                    -- tick_out or rxchar before the first NULL has been seen.
                    -- Therefore it's ok here to bail on those conditions
                    -- without explicitly testing got_null.
                    v.state := S_ErrorReset; -- error, go back to reset
                    v_timerrst := '1';
                ELSIF r.timercnt = 0 THEN
                    IF r.timerdone = '1' THEN
                        v.state := S_Ready;
                        v_timerrst := '1';
                    END IF;
                END IF;

            WHEN S_Ready =>
                -- Wait for link start.
                IF ((recvo.errdisc OR recvo.errpar OR recvo.erresc) = '1') OR
                    ((recvo.gotfct OR recvo.tick_out OR recvo.rxchar) = '1') THEN
                    v.state := S_ErrorReset; -- error, go back to reset
                    v_timerrst := '1';
                ELSIF (linki.linkdis = '0') AND (r.xmit_fct_in = '1') AND
                    ((linki.linkstart OR (linki.autostart AND recvo.gotnull)) = '1') THEN
                    v.state := S_Started; -- link enabled; start sending NULL
                    v_timerrst := '1';
                END IF;

            WHEN S_Started =>
                -- Wait for NULL.
                IF ((recvo.errdisc OR recvo.errpar OR recvo.erresc) = '1') OR
                    ((recvo.gotfct OR recvo.tick_out OR recvo.rxchar) = '1') OR
                    ((r.timercnt = 0) AND r.timerdone = '1') THEN
                    v.state := S_ErrorReset; -- error, go back to reset
                    v_timerrst := '1';
                ELSIF recvo.gotnull = '1' THEN
                    v.state := S_Connecting; -- received null, continue
                    v_timerrst := '1';
                END IF;

            WHEN S_Connecting =>
                -- Wait for FCT.
                IF ((recvo.errdisc OR recvo.errpar OR recvo.erresc) = '1') OR
                    ((recvo.tick_out OR recvo.rxchar) = '1') OR
                    ((r.timercnt = 0) AND r.timerdone = '1') THEN
                    v.state := S_ErrorReset; -- error, go back to reset
                    v_timerrst := '1';
                ELSIF recvo.gotfct = '1' THEN
                    v.state := S_Run; -- got FCT, init completed
                END IF;

            WHEN S_Run =>
                -- All is well.
                IF ((recvo.errdisc OR recvo.errpar OR recvo.erresc) = '1') OR
                    (r.errcred = '1') OR
                    (linki.linkdis = '1') THEN
                    v.state := S_ErrorReset; -- error, go back to reset
                    v_timerrst := '1';
                END IF;

            WHEN OTHERS =>
                v.state := S_ErrorReset; -- recover from invalid state
                v_timerrst := '1';

        END CASE;

        -- Update credit counters.
        IF r.state = S_ErrorReset THEN

            -- reset credit
            v.tx_credit := to_unsigned(0, v.tx_credit'length);
            v.rx_credit := to_unsigned(0, v.rx_credit'length);

        ELSE

            -- update TX credit
            IF recvo.gotfct = '1' THEN
                -- just received a FCT token
                v.tx_credit := v.tx_credit + to_unsigned(8, v.tx_credit'length);
                IF r.tx_credit > 48 THEN
                    -- received too many FCT tokens
                    v.errcred := '1';
                END IF;
            END IF;
            IF xmito.txack = '1' THEN
                -- just sent one byte
                v.tx_credit := v.tx_credit - to_unsigned(1, v.tx_credit'length);
            END IF;

            -- update RX credit after sending FCT
            IF xmito.fctack = '1' THEN
                -- just sent a FCT token
                v.rx_credit := v.rx_credit + to_unsigned(8, v.rx_credit'length);
            END IF;

            -- decide about sending FCT tokens
            v.xmit_fct_in := bool_to_logic((v.rx_credit <= 48) AND
            (v.rx_credit + to_unsigned(8, v.rx_credit'length) <= unsigned(linki.rxroom)));

            -- update RX credit after receiving character
            IF recvo.rxchar = '1' THEN
                -- just received a character
                v.rx_credit := v.rx_credit - to_unsigned(1, v.rx_credit'length);
                IF r.rx_credit = 0 THEN
                    -- remote transmitter violated its credit
                    v.errcred := '1';
                END IF;
            END IF;

        END IF;

        -- Update the initializaton reset timer.
        IF v_timerrst = '1' THEN
            v.timercnt := to_unsigned(reset_time, v.timercnt'length);
            v.timerdone := '0';
        ELSE
            IF r.timercnt = 0 THEN
                v.timercnt := to_unsigned(reset_time, v.timercnt'length);
                v.timerdone := '1';
            ELSE
                v.timercnt := r.timercnt - 1;
            END IF;
        END IF;

        -- Reset
        IF rst = '1' THEN
            v := regs_reset;
        END IF;

        -- Drive link level outputs.
        linko.started <= bool_to_logic(r.state = S_Started);
        linko.connecting <= bool_to_logic(r.state = S_Connecting);
        linko.running <= bool_to_logic(r.state = S_Run);
        linko.errdisc <= recvo.errdisc AND bool_to_logic(r.state = S_Run);
        linko.errpar <= recvo.errpar AND bool_to_logic(r.state = S_Run);
        linko.erresc <= recvo.erresc AND bool_to_logic(r.state = S_Run);
        linko.errcred <= r.errcred;
        linko.txack <= xmito.txack;
        linko.tick_out <= recvo.tick_out AND bool_to_logic(r.state = S_Run);
        linko.ctrl_out <= recvo.ctrl_out;
        linko.time_out <= recvo.time_out;
        linko.rxchar <= recvo.rxchar AND bool_to_logic(r.state = S_Run);
        linko.rxflag <= recvo.rxflag;
        linko.rxdata <= recvo.rxdata;

        -- Drive receiver inputs.
        rxen <= bool_to_logic(r.state /= S_ErrorReset);

        -- Drive transmitter input signals.
        xmiti.txen <= bool_to_logic(r.state = S_Started OR
        r.state = S_Connecting OR
        r.state = S_Run);
        xmiti.stnull <= bool_to_logic(r.state = S_Started);
        xmiti.stfct <= bool_to_logic(r.state = S_Connecting);
        xmiti.fct_in <= r.xmit_fct_in;
        xmiti.tick_in <= linki.tick_in AND bool_to_logic(r.state = S_Run);
        xmiti.ctrl_in <= linki.ctrl_in;
        xmiti.time_in <= linki.time_in;
        xmiti.txwrite <= linki.txwrite AND bool_to_logic(r.tx_credit /= 0);
        xmiti.txflag <= linki.txflag;
        xmiti.txdata <= linki.txdata;

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

END ARCHITECTURE spwlink_arch;