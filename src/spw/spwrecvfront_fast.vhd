--
--  Front-end for SpaceWire Receiver
--
--  This entity samples the input signals DataIn and StrobeIn to detect
--  valid bit transitions. Received bits are handed to the application
--  in groups of "rxchunk" bits at a time, synchronous to the system clock.
--
--  This receiver is based on synchronous oversampling of the input signals.
--  Inputs are sampled on the rising and falling edges of an externally
--  supplied sample clock "rxclk". Therefore the maximum bitrate of the
--  incoming signal must be significantly lower than two times the "rxclk"
--  clock frequency. The maximum incoming bitrate must also be strictly
--  lower than rxchunk times the system clock frequency.
--
--  This code is tuned for implementation on Xilinx Spartan-3.
--
--  Details
--  -------
--
--  Stage A: The inputs "spw_di" and "spw_si" are handled as DDR signals,
--  synchronously sampled on both edges of "rxclk".
--
--  Stage B: The input signals are re-registered on the rising edge of "rxclk"
--  for further processing. This implies that every rising edge of "rxclk"
--  produces two new samples of "spw_di" and two new samples of "spw_si".
--
--  Stage C: Transitions in input signals are detected by comparing the XOR
--  of data and strobe to the XOR of the previous data and strobe samples.
--  If there is a difference, we know that either data or strobe has changed
--  and the new value of data is a valid new bit. Every rising edge of "rxclk"
--  thus produces either zero, or one or two new data bits.
--
--  Stage D: Received bits are collected in groups of "rxchunk" bits
--  (unless rxchunk=1, in which case groups of 2 bits are used). Complete
--  groups are pushed into an 8-deep cyclic buffer. A 3-bit counter "headptr"
--  indicates the current position in the cyclic buffer.
--
--  The system clock domain reads bit groups from the cyclic buffer. A tail
--  pointer indicates the next location to read from the buffer. A comparison
--  between the "tailptr" and a re-synchronized copy of the "headptr" determines
--  whether valid bits are available in the buffer.
--
--  Activity detection is based on a 3-bit counter "bitcnt". This counter is
--  incremented whenever the rxclk domain receives 1 or 2 new bits. The system
--  clock domain monitors a re-synchronized copy of the activity counter to
--  determine whether it has been updated since the previous system clock cycle.
--
--  Implementation guidelines 
--  -------------------------
--
--  IOB flip-flops must be used to sample spw_di and spw_si.
--  Clock skew between the IOBs for spw_di and spw_si must be minimized.
--
--  "rxclk" must be at least as fast as the system clock;
--  "rxclk" does not need to be phase-related to the system clock;
--  it is allowed for "rxclk" to be equal to the system clock.
--
--  The following timing constraints are needed:
--   * PERIOD constraint on the system clock;
--   * PERIOD constraint on "rxclk";
--   * FROM-TO constraint from "rxclk" to system clock, equal to one "rxclk" period;
--   * FROM-TO constraint from system clock to "rxclk", equal to one "rxclk" period.
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.spwpkg.ALL;

ENTITY spwrecvfront_fast IS

    GENERIC (
        -- Number of bits to pass to the application per system clock.
        rxchunk : INTEGER RANGE 1 TO 4);

    PORT (
        -- System clock.
        clk : IN STD_LOGIC;

        -- Sample clock.
        rxclk : IN STD_LOGIC;

        -- High to enable receiver; low to disable and reset receiver.
        rxen : IN STD_LOGIC;

        -- High if there has been recent activity on the input lines.
        inact : OUT STD_LOGIC;

        -- High if inbits contains a valid group of received bits.
        -- If inbvalid='1', the application must sample inbits on
        -- the rising edge of clk.
        inbvalid : OUT STD_LOGIC;

        -- Received bits (bit 0 is the earliest received bit).
        inbits : OUT STD_LOGIC_VECTOR(rxchunk - 1 DOWNTO 0);

        -- Data In signal from SpaceWire bus.
        spw_di : IN STD_LOGIC;

        -- Strobe In signal from SpaceWire bus.
        spw_si : IN STD_LOGIC);

    -- Turn off FSM extraction.
    -- Without this, XST will happily apply one-hot encoding to rrx.headptr.
    --attribute FSM_EXTRACT: string;
    --attribute FSM_EXTRACT of spwrecvfront_fast: entity is "NO";

END ENTITY spwrecvfront_fast;

ARCHITECTURE spwrecvfront_arch OF spwrecvfront_fast IS

    -- width of bit groups in cyclic buffer;
    -- typically equal to rxchunk, except when rxchunk = 1
    TYPE memwidth_array_type IS ARRAY(1 TO 4) OF INTEGER;
    CONSTANT chunk_to_memwidth : memwidth_array_type := (2, 2, 3, 4);
    CONSTANT memwidth : INTEGER := chunk_to_memwidth(rxchunk);

    -- registers in rxclk domain
    TYPE rxregs_type IS RECORD
        -- stage B: re-register input samples
        b_di0 : STD_ULOGIC;
        b_si0 : STD_ULOGIC;
        b_di1 : STD_ULOGIC;
        b_si1 : STD_ULOGIC;
        -- stage C: data/strobe decoding
        c_bit : STD_LOGIC_VECTOR(1 DOWNTO 0);
        c_val : STD_LOGIC_VECTOR(1 DOWNTO 0);
        c_xor1 : STD_ULOGIC;
        -- stage D: collect groups of memwidth bits
        d_shift : STD_LOGIC_VECTOR(memwidth - 1 DOWNTO 0);
        d_count : STD_LOGIC_VECTOR(memwidth - 1 DOWNTO 0);
        -- cyclic buffer access
        bufdata : STD_LOGIC_VECTOR(memwidth - 1 DOWNTO 0);
        bufwrite : STD_ULOGIC;
        headptr : STD_LOGIC_VECTOR(2 DOWNTO 0);
        -- activity detection
        bitcnt : STD_LOGIC_VECTOR(2 DOWNTO 0);
    END RECORD;

    -- registers in system clock domain
    TYPE regs_type IS RECORD
        -- data path from buffer to output
        tailptr : STD_LOGIC_VECTOR(2 DOWNTO 0);
        inbvalid : STD_ULOGIC;
        -- split 2-bit groups if rxchunk=1
        splitbit : STD_ULOGIC;
        splitinx : STD_ULOGIC;
        splitvalid : STD_ULOGIC;
        -- activity detection
        bitcntp : STD_LOGIC_VECTOR(2 DOWNTO 0);
        inact : STD_ULOGIC;
        -- reset signal towards rxclk domain
        rxdis : STD_ULOGIC;
    END RECORD;

    CONSTANT regs_reset : regs_type := (
        tailptr => "000",
        inbvalid => '0',
        splitbit => '0',
        splitinx => '0',
        splitvalid => '0',
        bitcntp => "000",
        inact => '0',
        rxdis => '1');

    -- Signals that are re-synchronized from rxclk to system clock domain.
    TYPE syncsys_type IS RECORD
        headptr : STD_LOGIC_VECTOR(2 DOWNTO 0); -- pointer in cyclic buffer
        bitcnt : STD_LOGIC_VECTOR(2 DOWNTO 0); -- activity detection
    END RECORD;

    -- Registers.
    SIGNAL r : regs_type := regs_reset;
    SIGNAL rin : regs_type;
    SIGNAL rrx, rrxin : rxregs_type;

    -- Synchronized signals after crossing clock domains.
    SIGNAL syncrx_rstn : STD_LOGIC;
    SIGNAL syncsys : syncsys_type;

    -- Output data from cyclic buffer.
    SIGNAL s_bufdout : STD_LOGIC_VECTOR(memwidth - 1 DOWNTO 0);

    -- stage A: input flip-flops for rising/falling rxclk
    SIGNAL s_a_di0 : STD_LOGIC;
    SIGNAL s_a_si0 : STD_LOGIC;
    SIGNAL s_a_di1 : STD_LOGIC;
    SIGNAL s_a_si1 : STD_LOGIC;
    SIGNAL s_a_di2 : STD_LOGIC;
    SIGNAL s_a_si2 : STD_LOGIC;

    -- force use of IOB flip-flops
    --attribute IOB: string;
    --attribute IOB of s_a_di1: signal is "TRUE";
    --attribute IOB of s_a_si1: signal is "TRUE";
    --attribute IOB of s_a_di2: signal is "TRUE";
    --attribute IOB of s_a_si2: signal is "TRUE";

BEGIN

    -- Cyclic data buffer.
    bufmem : spwram
    GENERIC MAP(
        abits => 3,
        dbits => memwidth)
    PORT MAP(
        rclk => clk,
        wclk => rxclk,
        ren => '1',
        raddr => r.tailptr,
        rdata => s_bufdout,
        wen => rrx.bufwrite,
        waddr => rrx.headptr,
        wdata => rrx.bufdata);

    -- Synchronize reset signal for rxclk domain.
    syncrx_reset : syncdff
    PORT MAP(clk => rxclk, rst => r.rxdis, di => '1', do => syncrx_rstn);

    -- Synchronize signals from rxclk domain to system clock domain.
    syncsys_headptr0 : syncdff
    PORT MAP(clk => clk, rst => r.rxdis, di => rrx.headptr(0), do => syncsys.headptr(0));
    syncsys_headptr1 : syncdff
    PORT MAP(clk => clk, rst => r.rxdis, di => rrx.headptr(1), do => syncsys.headptr(1));
    syncsys_headptr2 : syncdff
    PORT MAP(clk => clk, rst => r.rxdis, di => rrx.headptr(2), do => syncsys.headptr(2));
    syncsys_bitcnt0 : syncdff
    PORT MAP(clk => clk, rst => r.rxdis, di => rrx.bitcnt(0), do => syncsys.bitcnt(0));
    syncsys_bitcnt1 : syncdff
    PORT MAP(clk => clk, rst => r.rxdis, di => rrx.bitcnt(1), do => syncsys.bitcnt(1));
    syncsys_bitcnt2 : syncdff
    PORT MAP(clk => clk, rst => r.rxdis, di => rrx.bitcnt(2), do => syncsys.bitcnt(2));

    -- sample inputs on rising edge of rxclk
    PROCESS (rxclk) IS
    BEGIN
        IF rising_edge(rxclk) THEN
            s_a_di1 <= spw_di;
            s_a_si1 <= spw_si;
        END IF;
    END PROCESS;

    -- sample inputs on falling edge of rxclk
    PROCESS (rxclk) IS
    BEGIN
        IF falling_edge(rxclk) THEN
            s_a_di2 <= spw_di;
            s_a_si2 <= spw_si;
            -- reregister inputs in fabric flip-flops
            s_a_di0 <= s_a_di2;
            s_a_si0 <= s_a_si2;
        END IF;
    END PROCESS;

    -- combinatorial process
    PROCESS (r, rrx, rxen, syncrx_rstn, syncsys, s_bufdout, s_a_di0, s_a_si0, s_a_di1, s_a_si1)
        VARIABLE v : regs_type;
        VARIABLE vrx : rxregs_type;
    BEGIN
        v := r;
        vrx := rrx;

        -- ---- SAMPLE CLOCK DOMAIN ----

        -- stage B: re-register input samples
        vrx.b_di0 := s_a_di0;
        vrx.b_si0 := s_a_si0;
        vrx.b_di1 := s_a_di1;
        vrx.b_si1 := s_a_si1;

        -- stage C: decode data/strobe and detect valid bits
        IF (rrx.b_di0 XOR rrx.b_si0 XOR rrx.c_xor1) = '1' THEN
            vrx.c_bit(0) := rrx.b_di0;
        ELSE
            vrx.c_bit(0) := rrx.b_di1;
        END IF;
        vrx.c_bit(1) := rrx.b_di1;
        vrx.c_val(0) := (rrx.b_di0 XOR rrx.b_si0 XOR rrx.c_xor1) OR
        (rrx.b_di0 XOR rrx.b_si0 XOR rrx.b_di1 XOR rrx.b_si1);
        vrx.c_val(1) := (rrx.b_di0 XOR rrx.b_si0 XOR rrx.c_xor1) AND
        (rrx.b_di0 XOR rrx.b_si0 XOR rrx.b_di1 XOR rrx.b_si1);
        vrx.c_xor1 := rrx.b_di1 XOR rrx.b_si1;

        -- Note:
        -- c_val = "00" if no new bits are received
        -- c_val = "01" if one new bit is received; the new bit is in c_bit(0)
        -- c_val = "11" if two new bits are received

        -- stage D: collect groups of memwidth bits
        IF rrx.c_val(0) = '1' THEN

            -- shift incoming bits into register
            IF rrx.c_val(1) = '1' THEN
                vrx.d_shift := rrx.c_bit & rrx.d_shift(memwidth - 1 DOWNTO 2);
            ELSE
                vrx.d_shift := rrx.c_bit(0) & rrx.d_shift(memwidth - 1 DOWNTO 1);
            END IF;

            -- prepare to store a group of memwidth bits
            IF rrx.d_count(0) = '1' THEN
                -- only one more bit needed
                vrx.bufdata := rrx.c_bit(0) & rrx.d_shift(memwidth - 1 DOWNTO 1);
            ELSE
                vrx.bufdata := rrx.c_bit & rrx.d_shift(memwidth - 1 DOWNTO 2);
            END IF;

            -- countdown nr of needed bits (one-hot counter)
            IF rrx.c_val(1) = '1' THEN
                vrx.d_count := rrx.d_count(1 DOWNTO 0) & rrx.d_count(memwidth - 1 DOWNTO 2);
            ELSE
                vrx.d_count := rrx.d_count(0 DOWNTO 0) & rrx.d_count(memwidth - 1 DOWNTO 1);
            END IF;

        END IF;

        -- stage D: store groups of memwidth bits
        vrx.bufwrite := rrx.c_val(0) AND (rrx.d_count(0) OR (rrx.c_val(1) AND rrx.d_count(1)));

        -- Increment head pointer.
        IF rrx.bufwrite = '1' THEN
            vrx.headptr := STD_LOGIC_VECTOR(unsigned(rrx.headptr) + 1);
        END IF;

        -- Activity detection.
        IF rrx.c_val(0) = '1' THEN
            vrx.bitcnt := STD_LOGIC_VECTOR(unsigned(rrx.bitcnt) + 1);
        END IF;

        -- Synchronous reset of rxclk domain.
        IF syncrx_rstn = '0' THEN
            vrx.c_val := "00";
            vrx.c_xor1 := '0';
            vrx.d_count := (OTHERS => '0');
            vrx.d_count(memwidth - 1) := '1';
            vrx.bufwrite := '0';
            vrx.headptr := "000";
            vrx.bitcnt := "000";
        END IF;

        -- ---- SYSTEM CLOCK DOMAIN ----

        -- Compare tailptr to headptr to decide whether there is new data.
        -- If the values are equal, we are about to read a location which has
        -- not yet been written by the rxclk domain.
        IF r.tailptr = syncsys.headptr THEN
            -- No more data in cyclic buffer.
            v.inbvalid := '0';
        ELSE
            -- Reading valid data from cyclic buffer.
            v.inbvalid := '1';
            -- Increment tail pointer.
            IF rxchunk /= 1 THEN
                v.tailptr := STD_LOGIC_VECTOR(unsigned(r.tailptr) + 1);
            END IF;
        END IF;

        -- If rxchunk=1, split 2-bit groups into separate bits.
        IF rxchunk = 1 THEN
            -- Select one of the two bits.
            IF r.splitinx = '0' THEN
                v.splitbit := s_bufdout(0);
            ELSE
                v.splitbit := s_bufdout(1);
            END IF;
            -- Indicate valid bit.
            v.splitvalid := r.inbvalid;
            -- Increment tail pointer.
            IF r.inbvalid = '1' THEN
                v.splitinx := NOT r.splitinx;
                IF r.splitinx = '0' THEN
                    v.tailptr := STD_LOGIC_VECTOR(unsigned(r.tailptr) + 1);
                END IF;
            END IF;
        END IF;

        -- Activity detection.
        v.bitcntp := syncsys.bitcnt;
        IF r.bitcntp = syncsys.bitcnt THEN
            v.inact := '0';
        ELSE
            v.inact := '1';
        END IF;

        -- Synchronous reset of system clock domain.
        IF rxen = '0' THEN
            v := regs_reset;
        END IF;

        -- Register rxen to ensure glitch-free signal to rxclk domain
        v.rxdis := NOT rxen;

        -- drive outputs
        inact <= r.inact;
        IF rxchunk = 1 THEN
            inbvalid <= r.splitvalid;
            inbits(0) <= r.splitbit;
        ELSE
            inbvalid <= r.inbvalid;
            inbits <= s_bufdout;
        END IF;

        -- update registers
        rrxin <= vrx;
        rin <= v;

    END PROCESS;

    -- update registers on rising edge of rxclk
    PROCESS (rxclk) IS
    BEGIN
        IF rising_edge(rxclk) THEN
            rrx <= rrxin;
        END IF;
    END PROCESS;

    -- update registers on rising edge of system clock
    PROCESS (clk) IS
    BEGIN
        IF rising_edge(clk) THEN
            r <= rin;
        END IF;
    END PROCESS;

END ARCHITECTURE spwrecvfront_arch;