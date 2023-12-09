--
--  Synchronous two-port RAM with separate clocks for read and write ports.
--  The synthesizer for Xilinx Spartan-3 will infer Block RAM for this entity.
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY spwram IS

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

END ENTITY spwram;

ARCHITECTURE spwram_arch OF spwram IS

    TYPE mem_type IS ARRAY(0 TO (2 ** abits - 1)) OF
    STD_LOGIC_VECTOR(dbits - 1 DOWNTO 0);

    SIGNAL s_mem : mem_type;

BEGIN

    -- read process
    PROCESS (rclk) IS
    BEGIN
        IF rising_edge(rclk) THEN
            IF ren = '1' THEN
                rdata <= s_mem(to_integer(unsigned(raddr)));
            END IF;
        END IF;
    END PROCESS;

    -- write process
    PROCESS (wclk) IS
    BEGIN
        IF rising_edge(wclk) THEN
            IF wen = '1' THEN
                s_mem(to_integer(unsigned(waddr))) <= wdata;
            END IF;
        END IF;
    END PROCESS;

END ARCHITECTURE;