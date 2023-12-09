--
--  SpaceWire VHDL package
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

PACKAGE spwpkg IS
    -- Indicates a platform-specific implementation.
    TYPE spw_implementation_type IS (impl_generic, impl_fast);
    -- Input signals to spwlink.
    TYPE spw_link_in_type IS RECORD

        -- Enables automatic link start on receipt of a NULL character.
        autostart : STD_LOGIC;

        -- Enables link start once the Ready state is reached.
        -- Without either "autostart" or "linkstart", the link remains in
        -- state Ready.
        linkstart : STD_LOGIC;

        -- Do not start link (overrides "linkstart" and "autostart") and/or
        -- disconnect the currently running link.
        linkdis : STD_LOGIC;

        -- Number of bytes available in the receive buffer. Used to for
        -- flow-control operation. At least 8 bytes must be available
        -- initially, otherwise the link can not start. Values larger than 63
        -- are irrelevant and may be presented as 63. The available room may
        -- decrease by one byte due to the reception of an N-Char; in that case
        -- the "rxroom" signal must be updated on the clock following the clock
        -- on which "rxchar" is high. Under no other circumstances may "rxroom"
        -- be decreased.
        rxroom : STD_LOGIC_VECTOR(5 DOWNTO 0);

        -- High for one clock cycle to request transmission of a TimeCode.
        -- The request is registered inside spwxmit until it can be processed.
        tick_in : STD_LOGIC;

        -- Control bits of the TimeCode to be sent.
        -- Must be valid while tick_in is high.
        ctrl_in : STD_LOGIC_VECTOR(1 DOWNTO 0);

        -- Counter value of the TimeCode to be sent.
        -- Must be valid while tick_in is high.
        time_in : STD_LOGIC_VECTOR(5 DOWNTO 0);

        -- Requests transmission of an N-Char.
        -- Keep this signal high until confirmed by "txack".
        txwrite : STD_LOGIC;

        -- Control flag to be sent with the next N-Char.
        -- Must be valid while "txwrite" is high.
        txflag : STD_LOGIC;

        -- Byte to be sent, or "00000000" for EOP or "00000001" for EEP.
        -- Must be valid while "txwrite" is high.
        txdata : STD_LOGIC_VECTOR(7 DOWNTO 0);
    END RECORD;
    -- Output signals from spwlink.
    TYPE spw_link_out_type IS RECORD

        -- High if the link state machine is currently in state Started.
        started : STD_LOGIC;

        -- High if the link state machine is currently in state Connecting.
        connecting : STD_LOGIC;

        -- High if the link state machine is currently in state Run.
        running : STD_LOGIC;

        -- Disconnect detected in state Run. Triggers a reset and reconnect.
        -- This indication is auto-clearing.
        errdisc : STD_LOGIC;

        -- Parity error detected in state Run. Triggers a reset and reconnect.
        -- This indication is auto-clearing.
        errpar : STD_LOGIC;

        -- Invalid escape sequence detected in state Run.
        -- Triggers a reset and reconnect; auto-clearing.
        erresc : STD_LOGIC;

        -- Credit error detected. Triggers a reset and reconnect.
        -- This indication is auto-clearing.
        errcred : STD_LOGIC;

        -- High to confirm the transmission of an N-Char.
        -- This is a Wishbone-style handshake signal. It has a combinatorial
        -- dependency on "txwrite".
        txack : STD_LOGIC;

        -- High for one clock cycle if a TimeCode was just received.
        -- Verification of the TimeCode as described in 8.12.2 of ECSS-E-50
        -- is not implemented; all received timecodes are reported.
        tick_out : STD_LOGIC;

        -- Control bits of last received TimeCode.
        ctrl_out : STD_LOGIC_VECTOR(1 DOWNTO 0);

        -- Counter value of last received TimeCode.
        time_out : STD_LOGIC_VECTOR(5 DOWNTO 0);

        -- High for one clock cycle if an N-Char (data byte or EOP or EEP) was
        -- just received. The data bits must be accepted immediately from
        -- "rxflag" and "rxdata".
        rxchar : STD_LOGIC;

        -- High if the received character is EOP or EEP, low if it is a data
        -- byte. Valid when "rxchar" is high.
        rxflag : STD_LOGIC;

        -- Received byte, or "00000000" for EOP or "00000001" for EEP.
        -- Valid when "rxchar" is high.
        rxdata : STD_LOGIC_VECTOR(7 DOWNTO 0);
    END RECORD;
    -- Output signals from spwrecv to spwlink.
    TYPE spw_recv_out_type IS RECORD

        -- High if at least one signal change was seen since enable.
        -- Resets to low when rxen is low.
        gotbit : STD_LOGIC;

        -- High if at least one valid NULL pattern was detected since enable.
        -- Resets to low when rxen is low.
        gotnull : STD_LOGIC;

        -- High for one clock cycle if an FCT token was just received.
        gotfct : STD_LOGIC;

        -- High for one clock cycle if a TimeCode was just received.
        tick_out : STD_LOGIC;

        -- Control bits of last received TimeCode.
        ctrl_out : STD_LOGIC_VECTOR(1 DOWNTO 0);

        -- Counter value of last received TimeCode.
        time_out : STD_LOGIC_VECTOR(5 DOWNTO 0);

        -- High for one clock cycle if an N-Char (data byte or EOP/EEP) was just received.
        rxchar : STD_LOGIC;

        -- High if rxchar is high and the received character is EOP or EEP.
        -- Low if rxchar is high and the received character is a data byte.
        rxflag : STD_LOGIC;

        -- Received byte, or "00000000" for EOP or "00000001" for EEP.
        -- Valid when "rxchar" is high.
        rxdata : STD_LOGIC_VECTOR(7 DOWNTO 0);

        -- Disconnect detected (after a signal change was seen).
        -- Resets to low when rxen is low or when a signal change is seen.
        errdisc : STD_LOGIC;

        -- Parity error detected (after a valid NULL pattern was seen).
        -- Sticky; resets to low when rxen is low.
        errpar : STD_LOGIC;

        -- Escape sequence error detected (after a valid NULL pattern was seen).
        -- Sticky; resets to low when rxen is low.
        erresc : STD_LOGIC;
    END RECORD;
    -- Input signals to spwxmit from spwlink.
    TYPE spw_xmit_in_type IS RECORD

        -- High to enable transmitter; low to disable and reset transmitter.
        txen : STD_LOGIC;

        -- Indicates that only NULL characters may be transmitted.
        stnull : STD_LOGIC;

        -- Indicates that only NULL and/or FCT characters may be transmitted.
        stfct : STD_LOGIC;

        -- Requests transmission of an FCT character.
        -- Keep this signal high until confirmed by "fctack".
        fct_in : STD_LOGIC;

        -- High for one clock cycle to request transmission of a TimeCode.
        -- The request is registered inside spwxmit until it can be processed.
        tick_in : STD_LOGIC;

        -- Control bits of the TimeCode to be sent.
        -- Must be valid while "tick_in" is high.
        ctrl_in : STD_LOGIC_VECTOR(1 DOWNTO 0);

        -- Counter value of the TimeCode to be sent.
        -- Must be valid while "tick_in" is high.
        time_in : STD_LOGIC_VECTOR(5 DOWNTO 0);

        -- Request transmission of an N-Char.
        -- Keep this signal high until confirmed by "txack".
        txwrite : STD_LOGIC;

        -- Control flag to be sent with the next N-Char.
        -- Must be valid while "txwrite" is high.
        txflag : STD_LOGIC;

        -- Byte to send, or "00000000" for EOP or "00000001" for EEP.
        -- Must be valid while "txwrite" is high.
        txdata : STD_LOGIC_VECTOR(7 DOWNTO 0);
    END RECORD;
    -- Output signals from spwxmit to spwlink.
    TYPE spw_xmit_out_type IS RECORD

        -- High to confirm transmission on an FCT character.
        -- This is a Wishbone-style handshaking signal; it is combinatorially
        -- dependent on "fct_in".
        fctack : STD_LOGIC;

        -- High to confirm transmission of an N-Char.
        -- This is a Wishbone-style handshaking signal; it is combinatorially
        -- dependent on both "fct_in" and "txwrite".
        txack : STD_LOGIC;
    END RECORD;
    -- Character-stream interface
    COMPONENT spwstream IS
        GENERIC (
            sysfreq : real; -- clk freq in Hz
            txclkfreq : real := 0.0; -- txclk freq in Hz
            rximpl : spw_implementation_type := impl_generic;
            rxchunk : INTEGER RANGE 1 TO 4 := 1; -- max bits per clk
            tximpl : spw_implementation_type := impl_generic;
            rxfifosize_bits : INTEGER RANGE 6 TO 14 := 11; -- rx fifo size
            txfifosize_bits : INTEGER RANGE 2 TO 14 := 11 -- tx fifo size
        );
        PORT (
            clk : IN STD_LOGIC; -- system clock
            rxclk : IN STD_LOGIC; -- receiver sample clock
            txclk : IN STD_LOGIC; -- transmit clock
            rst : IN STD_LOGIC; -- synchronous reset
            autostart : IN STD_LOGIC; -- automatic link start
            linkstart : IN STD_LOGIC; -- forced link start
            linkdis : IN STD_LOGIC; -- stop link
            txdivcnt : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- tx scale factor
            tick_in : IN STD_LOGIC; -- request timecode xmit
            ctrl_in : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
            time_in : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
            txwrite : IN STD_LOGIC; -- request character xmit
            txflag : IN STD_LOGIC; -- control flag of tx char
            txdata : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            txrdy : OUT STD_LOGIC; -- room in tx fifo
            txhalff : OUT STD_LOGIC; -- tx fifo half full
            tick_out : OUT STD_LOGIC; -- timecode received
            ctrl_out : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
            time_out : OUT STD_LOGIC_VECTOR(5 DOWNTO 0);
            rxvalid : OUT STD_LOGIC; -- rx fifo not empty
            rxhalff : OUT STD_LOGIC; -- rx fifo half full
            rxflag : OUT STD_LOGIC; -- control flag of rx char
            rxdata : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
            rxread : IN STD_LOGIC; -- accept rx character
            started : OUT STD_LOGIC; -- link in Started state
            connecting : OUT STD_LOGIC; -- link in Connecting state
            running : OUT STD_LOGIC; -- link in Run state
            errdisc : OUT STD_LOGIC; -- disconnect error
            errpar : OUT STD_LOGIC; -- parity error
            erresc : OUT STD_LOGIC; -- escape error
            errcred : OUT STD_LOGIC; -- credit error
            spw_di : IN STD_LOGIC;
            spw_si : IN STD_LOGIC;
            spw_do : OUT STD_LOGIC;
            spw_so : OUT STD_LOGIC
        );
    END COMPONENT spwstream;
    -- Link Level Interface
    COMPONENT spwlink IS
        GENERIC (
            reset_time : INTEGER -- reset time in clocks (6.4 us)
        );
        PORT (
            clk : IN STD_LOGIC; -- system clock
            rst : IN STD_LOGIC; -- synchronous reset (active-high)
            linki : IN spw_link_in_type;
            linko : OUT spw_link_out_type;
            rxen : OUT STD_LOGIC;
            recvo : IN spw_recv_out_type;
            xmiti : OUT spw_xmit_in_type;
            xmito : IN spw_xmit_out_type
        );
    END COMPONENT spwlink;
    -- Receiver
    COMPONENT spwrecv IS
        GENERIC (
            disconnect_time : INTEGER RANGE 1 TO 255; -- disconnect period in system clock cycles
            rxchunk : INTEGER RANGE 1 TO 4 -- nr of bits per system clock
        );
        PORT (
            clk : IN STD_LOGIC; -- system clock
            rxen : IN STD_LOGIC; -- receiver enabled
            recvo : OUT spw_recv_out_type;
            inact : IN STD_LOGIC;
            inbvalid : IN STD_LOGIC;
            inbits : IN STD_LOGIC_VECTOR(rxchunk - 1 DOWNTO 0)
        );
    END COMPONENT spwrecv;
    -- Transmitter (generic implementation)
    COMPONENT spwxmit IS
        PORT (
            clk : IN STD_LOGIC; -- system clock
            rst : IN STD_LOGIC; -- synchronous reset (active-high)
            divcnt : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            xmiti : IN spw_xmit_in_type;
            xmito : OUT spw_xmit_out_type;
            spw_do : OUT STD_LOGIC; -- tx data to SPW bus
            spw_so : OUT STD_LOGIC -- tx strobe to SPW bus
        );
    END COMPONENT spwxmit;
    -- Transmitter (separate tx clock domain)
    COMPONENT spwxmit_fast IS
        PORT (
            clk : IN STD_LOGIC; -- system clock
            txclk : IN STD_LOGIC; -- transmit clock
            rst : IN STD_LOGIC; -- synchronous reset (active-high)
            divcnt : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            xmiti : IN spw_xmit_in_type;
            xmito : OUT spw_xmit_out_type;
            spw_do : OUT STD_LOGIC; -- tx data to SPW bus
            spw_so : OUT STD_LOGIC -- tx strobe to SPW bus
        );
    END COMPONENT spwxmit_fast;
    -- Front-end for SpaceWire Receiver (generic implementation)
    COMPONENT spwrecvfront_generic IS
        PORT (
            clk : IN STD_LOGIC; -- system clock
            rxen : IN STD_LOGIC; -- high to enable receiver
            inact : OUT STD_LOGIC; -- high if activity on input
            inbvalid : OUT STD_LOGIC; -- high if inbits contains a valid received bit
            inbits : OUT STD_LOGIC_VECTOR(0 DOWNTO 0); -- received bit
            spw_di : IN STD_LOGIC; -- Data In signal from SpaceWire bus
            spw_si : IN STD_LOGIC -- Strobe In signal from SpaceWire bus
        );
    END COMPONENT spwrecvfront_generic;
    -- Front-end for SpaceWire Receiver (separate rx clock domain)
    COMPONENT spwrecvfront_fast IS
        GENERIC (
            rxchunk : INTEGER RANGE 1 TO 4 -- max number of bits per system clock
        );
        PORT (
            clk : IN STD_LOGIC; -- system clock
            rxclk : IN STD_LOGIC; -- sample clock (DDR)
            rxen : IN STD_LOGIC; -- high to enable receiver
            inact : OUT STD_LOGIC; -- high if activity on input
            inbvalid : OUT STD_LOGIC; -- high if inbits contains a valid group of received bits
            inbits : OUT STD_LOGIC_VECTOR(rxchunk - 1 DOWNTO 0); -- received bits
            spw_di : IN STD_LOGIC; -- Data In signal from SpaceWire bus
            spw_si : IN STD_LOGIC -- Strobe In signal from SpaceWire bus
        );
    END COMPONENT spwrecvfront_fast;
    -- Synchronous two-port memory.
    COMPONENT spwram IS
        GENERIC (
            abits : INTEGER;
            dbits : INTEGER);
        PORT (
            rclk : IN STD_LOGIC;
            wclk : IN STD_LOGIC;
            ren : IN STD_LOGIC;
            raddr : IN STD_LOGIC_VECTOR(abits - 1 DOWNTO 0);
            rdata : OUT STD_LOGIC_VECTOR(dbits - 1 DOWNTO 0);
            wen : IN STD_LOGIC;
            waddr : IN STD_LOGIC_VECTOR(abits - 1 DOWNTO 0);
            wdata : IN STD_LOGIC_VECTOR(dbits - 1 DOWNTO 0));
    END COMPONENT spwram;
    --  Double flip-flop synchronizer.
    COMPONENT syncdff IS
        PORT (
            clk : IN STD_LOGIC; -- clock (destination domain)
            rst : IN STD_LOGIC; -- asynchronous reset, active-high
            di : IN STD_LOGIC; -- input data
            do : OUT STD_LOGIC); -- output data
    END COMPONENT syncdff;

END PACKAGE;