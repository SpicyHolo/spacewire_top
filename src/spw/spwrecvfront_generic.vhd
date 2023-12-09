--
--  Front-end for SpaceWire Receiver
--
--  This entity samples the input signals DataIn and StrobeIn to detect
--  valid bit transitions. Received bits are handed to the application.
--
--  Inputs are sampled on the rising edge of the system clock, therefore
--  the maximum bitrate of the incoming signal must be significantly lower
--  than system clock frequency.
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY spwrecvfront_generic IS

    PORT (
        -- System clock.
        clk : IN STD_LOGIC;

        -- High to enable receiver; low to disable and reset receiver.
        rxen : IN STD_LOGIC;

        -- High if there has been recent activity on the input lines.
        inact : OUT STD_LOGIC;

        -- High if inbits contains a valid received bit.
        -- If inbvalid='1', the application must sample inbits on
        -- the rising edge of clk.
        inbvalid : OUT STD_LOGIC;

        -- Received bit
        inbits : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);

        -- Data In signal from SpaceWire bus.
        spw_di : IN STD_LOGIC;

        -- Strobe In signal from SpaceWire bus.
        spw_si : IN STD_LOGIC);

END ENTITY spwrecvfront_generic;

ARCHITECTURE spwrecvfront_arch OF spwrecvfront_generic IS

    -- input flip-flops
    SIGNAL s_spwdi1 : STD_ULOGIC;
    SIGNAL s_spwsi1 : STD_ULOGIC;
    SIGNAL s_spwdi2 : STD_ULOGIC;
    SIGNAL s_spwsi2 : STD_ULOGIC;

    -- data/strobe decoding
    SIGNAL s_spwsi3 : STD_ULOGIC;

    -- output registers
    SIGNAL s_inbvalid : STD_ULOGIC;
    SIGNAL s_inbit : STD_ULOGIC;

BEGIN

    -- drive outputs
    inact <= s_inbvalid;
    inbvalid <= s_inbvalid;
    inbits(0) <= s_inbit;

    -- synchronous process
    PROCESS (clk) IS
    BEGIN
        IF rising_edge(clk) THEN

            -- sample input signal
            s_spwdi1 <= spw_di;
            s_spwsi1 <= spw_si;

            -- more flip-flops for safe synchronization
            s_spwdi2 <= s_spwdi1;
            s_spwsi2 <= s_spwsi1;

            -- keep strobe signal for data/strobe decoding
            s_spwsi3 <= s_spwsi2;

            -- keep data bit for data/strobe decoding
            s_inbit <= s_spwdi2;

            IF rxen = '1' THEN
                -- data/strobe decoding
                s_inbvalid <= s_spwdi2 XOR s_spwsi2 XOR s_inbit XOR s_spwsi3;
            ELSE
                -- reset receiver
                s_inbvalid <= '0';
            END IF;

        END IF;
    END PROCESS;

END ARCHITECTURE spwrecvfront_arch;