--
--  SpaceWire Transmitter
--
--  This entity translates outgoing characters and tokens into
--  data-strobe signalling.
--
--  The output stage is driven by a separate transmission clock "txclk" which
--  will typically be faster than the system clock. The actual transmission
--  rate is determined by dividing the transmission clock by an integer factor.
--
--  The code is tuned for implementation on Xilinx Spartan-3.
--
--  Concept
--  -------
--
--  Logic in the system clock domain generates a stream of tokens to be
--  transmitted. These tokens are encoded as instances of the token_type
--  record. Tokens are queued in a two-slot FIFO buffer (r.token0 and r.token1)
--  with a 1-bit pointer (r.tokmux) pointing to the head of the queue.
--  When a token is pushed into the buffer, a flag register is flipped
--  (r.sysflip0 and r.sysflip1) to indicate to the txclk domain that the
--  buffer slot has been refilled.
--
--  The txclk domain pulls tokens from the FIFO buffer, flipping flag
--  registers (rtx.txflip0 and rtx.txflip1) to indicate to the system clock
--  domain that a token has been pulled. When the system clock domain detects
--  that a token has been consumed, it refills the buffer slot with a new
--  token (assuming that there are tokens waiting to be transmitted).
--  Whenever the FIFO buffer is empty, the txclk domain sends NULLs instead.
--  This can happen either when there are no tokens to send, or when the
--  system clock domain is late to refill the buffer.
--
--  Details
--  -------
--
--  Logic in the system clock domain accepts transmission requests through
--  the external interface of the entity. Pending requests are translated
--  into a stream of tokens. The tokens are pushed to the txclk domain through
--  the FIFO buffer as described above.
--
--  The data path through the txclk domain is divided into stages B through F
--  in a half-hearted attempt to keep things simple.
--
--  Stage B takes a token from the FIFO buffer and updates a buffer status
--  flag to indicate that the buffer slot needs to be refilled. If the FIFO
--  is empty, a NULL is inserted. Stage B is triggered one clock after
--  stage E switches to a new token. If the previous token was ESC, stage B
--  skips a turn because stage C will already know what to do.
--
--  Stage C takes a token from stage B and translates it into a bit pattern.
--  Time codes and NULL tokens are broken into two separate tokens starting
--  with ESC. Stage C is triggered one clock after the shift buffer in
--  stage E drops to 3 tokens.
--
--  Stage D completes the task of translating tokens to bit patterns and
--  distinguishes between 10-bit and 4-bit tokens. It is not explicitly
--  triggered but simply follows stage C.
--
--  Stage E is the bit shift register. It shifts when "txclken" is high.
--  A one-hot counter keeps track of the number of bits remaining in
--  the register. When the register falls empty, it loads a new 10-bit or
--  4-bit pattern as prepared by stage D. Stage E also computes parity.
--
--  Stage F performs data strobe encoding. When the transmitter is disabled,
--  the outputs of stage F fall to zero in a controlled way.
-- 
--  To generate the transmission bit clock, the txclk is divided by an
--  integer factor (divcnt+1) using an 8-bit down counter. The implementation
--  of this counter has become quite complicated in order to meet timing goals.
--  The counter consists of 4 blocks of two bits each (txclkcnt), with a
--  carry-save concept used between blocks (txclkcy). Detection of terminal
--  count (txclkdone) has a pipeline delay of two cycles. Therefore a separate
--  concept is used if the initial count is less than 2 (txdivnorm). This is
--  all glued together in the final assignment to txclken.
--
--  The initial count for txclk division (divcnt) comes from the system clock
--  domain and thus needs to be synchronized for use in the txclk domain.
--  To facilitate this, the system clock domain latches the value of divcnt
--  once every 6 sysclk cycles and sets a flag to indicate when the latched
--  value can safely be used by the txclk domain.
--
--  A tricky aspect of the design is the initial state of the txclk logic.
--  When the transmitter is enabled (txen goes high), the txclk logic starts
--  with the first ESC pattern already set up in stage D, and stage C ready
--  to produce the FCT part of the first NULL.
--
--  The following guidelines are used to get good timing for the txclk domain:
--   * The new value of a register depends on at most 4 inputs (single LUT),
--     or in a few cases on 5 inputs (two LUTs and F5MUX).
--   * Synchronous resets may be used, but only if the reset signal comes
--     directly from a register (no logic in set/reset path);
--   * Clock enables may be used, but only if the enable signal comes directly
--     from a register (no logic in clock enable path).
--
--  Synchronization issues
--  ----------------------
--
--  There is a two-slot FIFO buffer between the system and txclk domains.
--  After the txclk domain pulls a token from the buffer, the system clock
--  domain should ideally refill the buffer before the txclk domain again
--  tries to pull from the same buffer slot. If the refill occurs late,
--  the txclk domain needs to insert a NULL token which is inefficient
--  use of bandwidth.
--
--  Assuming the transmission consists of a stream of data characters,
--  10 bits per character, there are exactly 2*10 bit periods between
--  successive reads from the same buffer slot by the txclk logic.
--
--  The time needed for the system clock logic to refill a buffer slot =
--     1 txclk period   (update of rtx.txflipN)
--   + 1 txclk period   (routing delay between domains)
--   + 2 sysclk periods (synchronizer for txflipN)
--   + 1 sysclk period  (refill buffer slot and update r.sysflipN)
--   + 1 txclk period   (routing delay between domains)
--   + 2 txclk periods  (synchronizer for sysflipN)
--   = 5 txclk periods + 3 sysclk periods
--
--  If for example txclk is 4 times as fast as sysclk, this amounts to
--   5 txclk + 3 sysclk = 5 + 3*4 txclk = 17 txclk
--  is less than 20 bit periods even at maximum transmission rate, so
--  no problem there.
--
--  This is different when the data stream includes 4-bit tokens.
--  See the manual for further comments.
--
--  Implementation guidelines
--  -------------------------
--
--  To minimize clock skew, IOB flip-flops should be used to drive
--  spw_do and spw_so.
--
--  "txclk" must be at least as fast as the system clock;
--  "txclk" does not need to be phase-related to the system clock;
--  it is allowed for "txclk" to be equal to "clk".
--
--  The following timing constraints are needed:
--   * PERIOD constraint on the system clock;
--   * PERIOD constraint on "txclk";
--   * FROM-TO constraint from "txclk" to the system clock, equal to
--     one "txclk" period;
--   * FROM-TO constraint from the system clock to "txclk", equal to
--     one "txclk" period.
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.spwpkg.ALL;

ENTITY spwxmit_fast IS

    PORT (
        -- System clock.
        clk : IN STD_LOGIC;

        -- Transmit clock.
        txclk : IN STD_LOGIC;

        -- Synchronous reset (active-high)
        -- Used asynchronously by fast clock domain (must be glitch-free).
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

    -- Turn off FSM extraction to avoid synchronization problems.
    --attribute FSM_EXTRACT: string;
    --attribute FSM_EXTRACT of spwxmit_fast: entity is "NO";

END ENTITY spwxmit_fast;

ARCHITECTURE spwxmit_fast_arch OF spwxmit_fast IS

    -- Convert boolean to std_logic.
    TYPE bool_to_logic_type IS ARRAY(BOOLEAN) OF STD_ULOGIC;
    CONSTANT bool_to_logic : bool_to_logic_type := (false => '0', true => '1');

    -- Data records passed between clock domains.
    TYPE token_type IS RECORD
        tick : STD_ULOGIC; -- send time code
        fct : STD_ULOGIC; -- send FCT
        fctpiggy : STD_ULOGIC; -- send FCT and N-char
        flag : STD_ULOGIC; -- send EOP or EEP
        char : STD_LOGIC_VECTOR(7 DOWNTO 0); -- character or time code
    END RECORD;

    -- Registers in txclk domain
    TYPE txregs_type IS RECORD
        -- sync to system clock domain
        txflip0 : STD_ULOGIC;
        txflip1 : STD_ULOGIC;
        -- stage B
        b_update : STD_ULOGIC;
        b_mux : STD_ULOGIC;
        b_txflip : STD_ULOGIC;
        b_valid : STD_ULOGIC;
        b_token : token_type;
        -- stage C
        c_update : STD_ULOGIC;
        c_busy : STD_ULOGIC;
        c_esc : STD_ULOGIC;
        c_fct : STD_ULOGIC;
        c_bits : STD_LOGIC_VECTOR(8 DOWNTO 0);
        -- stage D
        d_bits : STD_LOGIC_VECTOR(8 DOWNTO 0);
        d_cnt4 : STD_ULOGIC;
        d_cnt10 : STD_ULOGIC;
        -- stage E
        e_valid : STD_ULOGIC;
        e_shift : STD_LOGIC_VECTOR(9 DOWNTO 0);
        e_count : STD_LOGIC_VECTOR(9 DOWNTO 0);
        e_parity : STD_ULOGIC;
        -- stage F
        f_spwdo : STD_ULOGIC;
        f_spwso : STD_ULOGIC;
        -- tx clock enable logic
        txclken : STD_ULOGIC;
        txclkpre : STD_ULOGIC;
        txclkcnt : STD_LOGIC_VECTOR(7 DOWNTO 0);
        txclkcy : STD_LOGIC_VECTOR(2 DOWNTO 0);
        txclkdone : STD_LOGIC_VECTOR(1 DOWNTO 0);
        txclkdiv : STD_LOGIC_VECTOR(7 DOWNTO 0);
        txdivnorm : STD_ULOGIC;
    END RECORD;

    -- Registers in system clock domain
    TYPE regs_type IS RECORD
        -- sync status to txclk domain
        txenreg : STD_ULOGIC;
        txdivreg : STD_LOGIC_VECTOR(7 DOWNTO 0);
        txdivnorm : STD_ULOGIC;
        txdivtmp : STD_LOGIC_VECTOR(1 DOWNTO 0);
        txdivsafe : STD_ULOGIC;
        -- data stream to txclk domain
        sysflip0 : STD_ULOGIC;
        sysflip1 : STD_ULOGIC;
        token0 : token_type;
        token1 : token_type;
        tokmux : STD_ULOGIC;
        -- transmitter management
        pend_fct : STD_ULOGIC; -- '1' if an outgoing FCT is pending
        pend_char : STD_ULOGIC; -- '1' if an outgoing N-Char is pending
        pend_data : STD_LOGIC_VECTOR(8 DOWNTO 0); -- control flag and data bits of pending char
        pend_tick : STD_ULOGIC; -- '1' if an outgoing time tick is pending
        pend_time : STD_LOGIC_VECTOR(7 DOWNTO 0); -- data bits of pending time tick
        allow_fct : STD_ULOGIC; -- '1' when allowed to send FCTs
        allow_char : STD_ULOGIC; -- '1' when allowed to send data and time
        sent_fct : STD_ULOGIC; -- '1' when at least one FCT token was sent
    END RECORD;

    -- Initial state of system clock domain
    CONSTANT token_reset : token_type := (
        tick => '0',
        fct => '0',
        fctpiggy => '0',
        flag => '0',
        char => (OTHERS => '0'));
    CONSTANT regs_reset : regs_type := (
        txenreg => '0',
        txdivreg => (OTHERS => '0'),
        txdivnorm => '0',
        txdivtmp => "00",
        txdivsafe => '0',
        sysflip0 => '0',
        sysflip1 => '0',
        token0 => token_reset,
        token1 => token_reset,
        tokmux => '0',
        pend_fct => '0',
        pend_char => '0',
        pend_data => (OTHERS => '0'),
        pend_tick => '0',
        pend_time => (OTHERS => '0'),
        allow_fct => '0',
        allow_char => '0',
        sent_fct => '0');

    -- Signals that are re-synchronized from system clock to txclk domain.
    TYPE synctx_type IS RECORD
        rstn : STD_ULOGIC;
        sysflip0 : STD_ULOGIC;
        sysflip1 : STD_ULOGIC;
        txen : STD_ULOGIC;
        txdivsafe : STD_ULOGIC;
    END RECORD;

    -- Signals that are re-synchronized from txclk to system clock domain.
    TYPE syncsys_type IS RECORD
        txflip0 : STD_ULOGIC;
        txflip1 : STD_ULOGIC;
    END RECORD;

    -- Registers
    SIGNAL rtx : txregs_type;
    SIGNAL rtxin : txregs_type;
    SIGNAL r : regs_type := regs_reset;
    SIGNAL rin : regs_type;

    -- Synchronized signals after crossing clock domains.
    SIGNAL synctx : synctx_type;
    SIGNAL syncsys : syncsys_type;

    -- Output flip-flops
    SIGNAL s_spwdo : STD_LOGIC;
    SIGNAL s_spwso : STD_LOGIC;

    -- Force use of IOB flip-flops
    --attribute IOB: string;
    --attribute IOB of s_spwdo: signal is "TRUE";
    --attribute IOB of s_spwso: signal is "TRUE";

BEGIN

    -- Reset synchronizer for txclk domain.
    synctx_rst : syncdff
    PORT MAP(clk => txclk, rst => rst, di => '1', do => synctx.rstn);

    -- Synchronize signals from system clock domain to txclk domain.
    synctx_sysflip0 : syncdff
    PORT MAP(clk => txclk, rst => rst, di => r.sysflip0, do => synctx.sysflip0);
    synctx_sysflip1 : syncdff
    PORT MAP(clk => txclk, rst => rst, di => r.sysflip1, do => synctx.sysflip1);
    synctx_txen : syncdff
    PORT MAP(clk => txclk, rst => rst, di => r.txenreg, do => synctx.txen);
    synctx_txdivsafe : syncdff
    PORT MAP(clk => txclk, rst => rst, di => r.txdivsafe, do => synctx.txdivsafe);

    -- Synchronize signals from txclk domain to system clock domain.
    syncsys_txflip0 : syncdff
    PORT MAP(clk => clk, rst => rst, di => rtx.txflip0, do => syncsys.txflip0);
    syncsys_txflip1 : syncdff
    PORT MAP(clk => clk, rst => rst, di => rtx.txflip1, do => syncsys.txflip1);

    -- Drive SpaceWire output signals
    spw_do <= s_spwdo;
    spw_so <= s_spwso;

    -- Combinatorial process
    PROCESS (r, rtx, rst, divcnt, xmiti, synctx, syncsys) IS
        VARIABLE v : regs_type;
        VARIABLE vtx : txregs_type;
        VARIABLE v_needtoken : STD_ULOGIC;
        VARIABLE v_havetoken : STD_ULOGIC;
        VARIABLE v_token : token_type;
    BEGIN
        v := r;
        vtx := rtx;
        v_needtoken := '0';
        v_havetoken := '0';
        v_token := token_reset;

        -- ---- FAST CLOCK DOMAIN ----

        -- Stage B: Multiplex tokens from system clock domain.
        -- Update stage B three bit periods after updating stage C
        -- (i.e. in time for the next update of stage C).
        -- Do not update stage B if stage C is indicating that it needs to
        -- send a second token to complete its task.
        vtx.b_update := rtx.txclken AND rtx.e_count(0) AND (NOT rtx.c_busy);
        IF rtx.b_mux = '0' THEN
            vtx.b_txflip := rtx.txflip0;
        ELSE
            vtx.b_txflip := rtx.txflip1;
        END IF;
        IF rtx.b_update = '1' THEN
            IF rtx.b_mux = '0' THEN
                -- get token from slot 0
                vtx.b_valid := synctx.sysflip0 XOR rtx.b_txflip;
                vtx.b_token := r.token0;
                -- update mux flag if we got a valid token
                vtx.b_mux := synctx.sysflip0 XOR rtx.b_txflip;
                vtx.txflip0 := synctx.sysflip0;
                vtx.txflip1 := rtx.txflip1;
            ELSE
                -- get token from slot 1
                vtx.b_valid := synctx.sysflip1 XOR rtx.b_txflip;
                vtx.b_token := r.token1;
                -- update mux flag if we got a valid token
                vtx.b_mux := NOT (synctx.sysflip1 XOR rtx.b_txflip);
                vtx.txflip0 := rtx.txflip0;
                vtx.txflip1 := synctx.sysflip1;
            END IF;
        END IF;

        -- Stage C: Prepare to transmit EOP, EEP or a data character.
        vtx.c_update := rtx.txclken AND rtx.e_count(3);
        IF rtx.c_update = '1' THEN

            -- NULL is broken into two tokens: ESC + FCT.
            -- Time-codes are broken into two tokens: ESC + char.

            -- Enable c_esc on the first pass of a NULL or a time-code.
            vtx.c_esc := (rtx.b_token.tick OR (NOT rtx.b_valid)) AND
            (NOT rtx.c_esc);

            -- Enable c_fct on the first pass of an FCT and on
            -- the second pass of a NULL (also the first pass, but c_esc
            -- is stronger than c_fct).
            vtx.c_fct := (rtx.b_token.fct AND (NOT rtx.c_busy)) OR
            (NOT rtx.b_valid);

            -- Enable c_busy on the first pass of a NULL or a time-code
            -- or a piggy-backed FCT. This will tell stage B that we are
            -- not done yet.
            vtx.c_busy := (rtx.b_token.tick OR (NOT rtx.b_valid) OR
            rtx.b_token.fctpiggy) AND (NOT rtx.c_busy);

            IF rtx.b_token.flag = '1' THEN
                IF rtx.b_token.char(0) = '0' THEN
                    -- prepare to send EOP
                    vtx.c_bits := "000000101"; -- EOP = P101
                ELSE
                    -- prepare to send EEP
                    vtx.c_bits := "000000011"; -- EEP = P110
                END IF;
            ELSE
                -- prepare to send data char
                vtx.c_bits := rtx.b_token.char & '0';
            END IF;
        END IF;

        -- Stage D: Prepare to transmit FCT, ESC, or the stuff from stage C.
        IF rtx.c_esc = '1' THEN
            -- prepare to send ESC
            vtx.d_bits := "000000111"; -- ESC = P111
            vtx.d_cnt4 := '1'; -- 3 bits + implicit parity bit
            vtx.d_cnt10 := '0';
        ELSIF rtx.c_fct = '1' THEN
            -- prepare to send FCT
            vtx.d_bits := "000000001"; -- FCT = P100
            vtx.d_cnt4 := '1'; -- 3 bits + implicit parity bit
            vtx.d_cnt10 := '0';
        ELSE
            -- send the stuff from stage C.
            vtx.d_bits := rtx.c_bits;
            vtx.d_cnt4 := rtx.c_bits(0);
            vtx.d_cnt10 := NOT rtx.c_bits(0);
        END IF;

        -- Stage E: Shift register.
        IF rtx.txclken = '1' THEN
            IF rtx.e_count(0) = '1' THEN
                -- reload shift register; output parity bit
                vtx.e_valid := '1';
                vtx.e_shift(vtx.e_shift'high DOWNTO 1) := rtx.d_bits;
                vtx.e_shift(0) := NOT (rtx.e_parity XOR rtx.d_bits(0));
                vtx.e_count := rtx.d_cnt10 & "00000" & rtx.d_cnt4 & "000";
                vtx.e_parity := rtx.d_bits(0);
            ELSE
                -- shift bits to output; update parity bit
                vtx.e_shift := '0' & rtx.e_shift(rtx.e_shift'high DOWNTO 1);
                vtx.e_count := '0' & rtx.e_count(rtx.e_count'high DOWNTO 1);
                vtx.e_parity := rtx.e_parity XOR rtx.e_shift(1);
            END IF;
        END IF;

        -- Stage F: Data/strobe encoding.
        IF rtx.txclken = '1' THEN
            IF rtx.e_valid = '1' THEN
                -- output next data/strobe bits
                vtx.f_spwdo := rtx.e_shift(0);
                vtx.f_spwso := NOT (rtx.e_shift(0) XOR rtx.f_spwdo XOR rtx.f_spwso);
            ELSE
                -- gentle reset of spacewire signals
                vtx.f_spwdo := rtx.f_spwdo AND rtx.f_spwso;
                vtx.f_spwso := '0';
            END IF;
        END IF;

        -- Generate tx clock enable
        -- An 8-bit counter decrements on every clock. A txclken pulse is
        -- produced 2 cycles after the counter reaches value 2. Counter reload
        -- values of 0 and 1 are handled as special cases.
        -- count down in blocks of two bits
        vtx.txclkcnt(1 DOWNTO 0) := STD_LOGIC_VECTOR(unsigned(rtx.txclkcnt(1 DOWNTO 0)) - 1);
        vtx.txclkcnt(3 DOWNTO 2) := STD_LOGIC_VECTOR(unsigned(rtx.txclkcnt(3 DOWNTO 2)) - unsigned(rtx.txclkcy(0 DOWNTO 0)));
        vtx.txclkcnt(5 DOWNTO 4) := STD_LOGIC_VECTOR(unsigned(rtx.txclkcnt(5 DOWNTO 4)) - unsigned(rtx.txclkcy(1 DOWNTO 1)));
        vtx.txclkcnt(7 DOWNTO 6) := STD_LOGIC_VECTOR(unsigned(rtx.txclkcnt(7 DOWNTO 6)) - unsigned(rtx.txclkcy(2 DOWNTO 2)));
        -- propagate carry in blocks of two bits
        vtx.txclkcy(0) := bool_to_logic(rtx.txclkcnt(1 DOWNTO 0) = "00");
        vtx.txclkcy(1) := rtx.txclkcy(0) AND bool_to_logic(rtx.txclkcnt(3 DOWNTO 2) = "00");
        vtx.txclkcy(2) := rtx.txclkcy(1) AND bool_to_logic(rtx.txclkcnt(5 DOWNTO 4) = "00");
        -- detect value 2 in counter
        vtx.txclkdone(0) := bool_to_logic(rtx.txclkcnt(3 DOWNTO 0) = "0010");
        vtx.txclkdone(1) := bool_to_logic(rtx.txclkcnt(7 DOWNTO 4) = "0000");
        -- trigger txclken
        vtx.txclken := (rtx.txclkdone(0) AND rtx.txclkdone(1)) OR rtx.txclkpre;
        vtx.txclkpre := (NOT rtx.txdivnorm) AND ((NOT rtx.txclkpre) OR (NOT rtx.txclkdiv(0)));
        -- reload counter
        IF rtx.txclken = '1' THEN
            vtx.txclkcnt := rtx.txclkdiv;
            vtx.txclkcy := "000";
            vtx.txclkdone := "00";
        END IF;

        -- Synchronize txclkdiv
        IF synctx.txdivsafe = '1' THEN
            vtx.txclkdiv := r.txdivreg;
            vtx.txdivnorm := r.txdivnorm;
        END IF;

        -- Transmitter disabled.
        IF synctx.txen = '0' THEN
            vtx.txflip0 := '0';
            vtx.txflip1 := '0';
            vtx.b_update := '0';
            vtx.b_mux := '0';
            vtx.b_valid := '0';
            vtx.c_update := '0';
            vtx.c_busy := '1';
            vtx.c_esc := '1'; -- need to send 2nd part of NULL
            vtx.c_fct := '1';
            vtx.d_bits := "000000111"; -- ESC = P111
            vtx.d_cnt4 := '1'; -- 3 bits + implicit parity bit
            vtx.d_cnt10 := '0';
            vtx.e_valid := '0';
            vtx.e_parity := '0';
            vtx.e_count := (0 => '1', OTHERS => '0');
        END IF;

        -- Reset.
        IF synctx.rstn = '0' THEN
            vtx.f_spwdo := '0';
            vtx.f_spwso := '0';
            vtx.txclken := '0';
            vtx.txclkpre := '1';
            vtx.txclkcnt := (OTHERS => '0');
            vtx.txclkdiv := (OTHERS => '0');
            vtx.txdivnorm := '0';
        END IF;

        -- ---- SYSTEM CLOCK DOMAIN ----

        -- Hold divcnt and txen for use by txclk domain.
        v.txdivtmp := STD_LOGIC_VECTOR(unsigned(r.txdivtmp) - 1);
        IF r.txdivtmp = "00" THEN
            IF r.txdivsafe = '0' THEN
                -- Latch the current value of divcnt and txen.
                v.txdivsafe := '1';
                v.txdivtmp := "01";
                v.txdivreg := divcnt;
                IF unsigned(divcnt(divcnt'high DOWNTO 1)) = 0 THEN
                    v.txdivnorm := '0';
                ELSE
                    v.txdivnorm := '1';
                END IF;
                v.txenreg := xmiti.txen;
            ELSE
                -- Drop the txdivsafe flag but keep latched values.
                v.txdivsafe := '0';
            END IF;
        END IF;

        -- Pass falling edge of txen signal as soon as possible.
        IF xmiti.txen = '0' THEN
            v.txenreg := '0';
        END IF;

        -- Store requests for FCT transmission.
        IF xmiti.fct_in = '1' AND r.allow_fct = '1' THEN
            v.pend_fct := '1';
        END IF;

        IF xmiti.txen = '0' THEN

            -- Transmitter disabled; reset state.
            v.sysflip0 := '0';
            v.sysflip1 := '0';
            v.tokmux := '0';
            v.pend_fct := '0';
            v.pend_char := '0';
            v.pend_tick := '0';
            v.allow_fct := '0';
            v.allow_char := '0';
            v.sent_fct := '0';

        ELSE

            -- Determine if a new token is needed.
            IF r.tokmux = '0' THEN
                IF r.sysflip0 = syncsys.txflip0 THEN
                    v_needtoken := '1';
                END IF;
            ELSE
                IF r.sysflip1 = syncsys.txflip1 THEN
                    v_needtoken := '1';
                END IF;
            END IF;

            -- Prepare new token.
            IF r.allow_char = '1' AND r.pend_tick = '1' THEN
                -- prepare to send time code
                v_token.tick := '1';
                v_token.fct := '0';
                v_token.fctpiggy := '0';
                v_token.flag := '0';
                v_token.char := r.pend_time;
                v_havetoken := '1';
                IF v_needtoken = '1' THEN
                    v.pend_tick := '0';
                END IF;
            ELSE
                IF r.allow_fct = '1' AND (xmiti.fct_in = '1' OR r.pend_fct = '1') THEN
                    -- prepare to send FCT
                    v_token.fct := '1';
                    v_havetoken := '1';
                    IF v_needtoken = '1' THEN
                        v.pend_fct := '0';
                        v.sent_fct := '1';
                    END IF;
                END IF;
                IF r.allow_char = '1' AND r.pend_char = '1' THEN
                    -- prepare to send N-Char
                    -- Note: it is possible to send an FCT and an N-Char
                    -- together by enabling the fctpiggy flag.
                    v_token.fctpiggy := v_token.fct;
                    v_token.flag := r.pend_data(8);
                    v_token.char := r.pend_data(7 DOWNTO 0);
                    v_havetoken := '1';
                    IF v_needtoken = '1' THEN
                        v.pend_char := '0';
                    END IF;
                END IF;
            END IF;

            -- Put new token in slot.
            IF v_havetoken = '1' THEN
                IF r.tokmux = '0' THEN
                    IF r.sysflip0 = syncsys.txflip0 THEN
                        v.sysflip0 := NOT r.sysflip0;
                        v.token0 := v_token;
                        v.tokmux := '1';
                    END IF;
                ELSE
                    IF r.sysflip1 = syncsys.txflip1 THEN
                        v.sysflip1 := NOT r.sysflip1;
                        v.token1 := v_token;
                        v.tokmux := '0';
                    END IF;
                END IF;
            END IF;

            -- Determine whether we are allowed to send FCTs and characters
            v.allow_fct := NOT xmiti.stnull;
            v.allow_char := (NOT xmiti.stnull) AND (NOT xmiti.stfct) AND r.sent_fct;

            -- Store request for data transmission.
            IF xmiti.txwrite = '1' AND r.allow_char = '1' AND r.pend_char = '0' THEN
                v.pend_char := '1';
                v.pend_data := xmiti.txflag & xmiti.txdata;
            END IF;

            -- Store requests for time tick transmission.
            IF xmiti.tick_in = '1' THEN
                v.pend_tick := '1';
                v.pend_time := xmiti.ctrl_in & xmiti.time_in;
            END IF;

        END IF;

        -- Synchronous reset of system clock domain.
        IF rst = '1' THEN
            v := regs_reset;
        END IF;

        -- Drive outputs.
        -- Note: the outputs are combinatorially dependent on certain inputs.

        -- Set fctack high if (FCT requested) and (FCTs allowed) AND
        -- (no FCT pending)
        xmito.fctack <= xmiti.fct_in AND xmiti.txen AND r.allow_fct AND
        (NOT r.pend_fct);

        -- Set txrdy high if (character requested) AND (characters allowed) AND
        -- (no character pending)
        xmito.txack <= xmiti.txwrite AND xmiti.txen AND r.allow_char AND
        (NOT r.pend_char);

        -- Update registers.
        rin <= v;
        rtxin <= vtx;
    END PROCESS;

    -- Synchronous process in txclk domain
    PROCESS (txclk) IS
    BEGIN
        IF rising_edge(txclk) THEN
            -- drive spacewire output signals
            s_spwdo <= rtx.f_spwdo;
            s_spwso <= rtx.f_spwso;
            -- update registers
            rtx <= rtxin;
        END IF;
    END PROCESS;

    -- Synchronous process in system clock domain
    PROCESS (clk) IS
    BEGIN
        IF rising_edge(clk) THEN
            -- update registers
            r <= rin;
        END IF;
    END PROCESS;

END ARCHITECTURE spwxmit_fast_arch;