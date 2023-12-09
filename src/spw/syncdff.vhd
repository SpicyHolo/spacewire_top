--
--  Double flip-flop synchronizer.
--
--  This entity is used to safely capture asynchronous signals.
--
--  An implementation may assign additional constraints to this entity
--  in order to reduce the probability of meta-stability issues.
--  For example, an extra tight timing constraint could be placed on
--  the data path from syncdff_ff1 to syncdff_ff2 to ensure that
--  meta-stability of ff1 is resolved before ff2 captures the signal.
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY syncdff IS

    PORT (
        clk : IN STD_LOGIC; -- clock (destination domain)
        rst : IN STD_LOGIC; -- asynchronous reset, active-high
        di : IN STD_LOGIC; -- input data
        do : OUT STD_LOGIC -- output data
    );

    -- Turn off register replication in XST.
    --attribute REGISTER_DUPLICATION: string;
    --attribute REGISTER_DUPLICATION of syncdff: entity is "NO";

END ENTITY syncdff;

ARCHITECTURE syncdff_arch OF syncdff IS

    -- flip-flops
    SIGNAL syncdff_ff1 : STD_ULOGIC := '0';
    SIGNAL syncdff_ff2 : STD_ULOGIC := '0';

    -- Turn of shift-register extraction in XST.
    --attribute SHIFT_EXTRACT: string;
    --attribute SHIFT_EXTRACT of syncdff_ff1: signal is "NO";
    --attribute SHIFT_EXTRACT of syncdff_ff2: signal is "NO";

    -- Tell XST to place both flip-flops in the same slice.
    --attribute RLOC: string;
    --attribute RLOC of syncdff_ff1: signal is "X0Y0";
    --attribute RLOC of syncdff_ff2: signal is "X0Y0";

    -- Tell XST to keep the flip-flop net names to be used in timing constraints.
    ATTRIBUTE KEEP : STRING;
    ATTRIBUTE KEEP OF syncdff_ff1 : SIGNAL IS "SOFT";
    ATTRIBUTE KEEP OF syncdff_ff2 : SIGNAL IS "SOFT";

BEGIN

    -- second flip-flop drives the output signal
    do <= syncdff_ff2;

    PROCESS (clk, rst) IS
    BEGIN
        IF rst = '1' THEN
            -- asynchronous reset
            syncdff_ff1 <= '0';
            syncdff_ff2 <= '0';
        ELSIF rising_edge(clk) THEN
            -- data synchronization
            syncdff_ff1 <= di;
            syncdff_ff2 <= syncdff_ff1;
        END IF;
    END PROCESS;

END ARCHITECTURE syncdff_arch;