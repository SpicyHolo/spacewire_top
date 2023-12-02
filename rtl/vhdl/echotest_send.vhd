library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.spwpkg.all;

entity echotest_send is

    generic (
        sysfreq:    real;
        txclkfreq:  real; -- (if tximpl = impl_fast).
        -- log2 of division factor from system clock freq to timecode freq.
        tickdiv:    integer range 12 to 24 := 20;
        rximpl:     spw_implementation_type := impl_generic;
        rxchunk:    integer range 1 to 4 := 1; -- Max bits per sysclock (impl_fast only).
        tximpl:     spw_implementation_type := impl_generic;
        rxfifosize_bits: integer range 6 to 14 := 11;
        txfifosize_bits: integer range 2 to 14 := 11
    );
    port (
        clk:        in  std_logic;
        rxclk:      in  std_logic; -- Receiver sample clock (only for impl_fast).
        txclk:      in  std_logic; -- Transmit clock (only for impl_fast).
        rst:        in  std_logic;
        linkstart:  in  std_logic; -- Enables spontaneous link start.
        autostart:  in  std_logic; -- link start on NULL
        linkdisable: in std_logic;
        senddata:   in  std_logic;
        sendtick:   in  std_logic;
        -- Scaling factor minus 1 for TX bitrate.
        txdivcnt:   in  std_logic_vector(7 downto 0);

        -- State
        linkstarted: out std_logic;
        linkconnecting: out std_logic;
        linkrun:    out std_logic;
        linkerror:  out std_logic; -- one pulse, not for LED
        -- High when taking a byte from the receive FIFO.
        gotdata:    out std_logic;
        dataerror:  out std_logic; -- (sticky)
        tickerror:  out std_logic; -- (sticky)

        -- SpaceWire signals.
        spw_di:     in  std_logic;
        spw_si:     in  std_logic;
        spw_do:     out std_logic;
        spw_so:     out std_logic
    );

end entity echotest_send;

architecture echotest_send_arch of echotest_send is

    -- Update 16-bit maximum length LFSR by 8 steps
    function lfsr16(x: in std_logic_vector) return std_logic_vector is
        variable y: std_logic_vector(15 downto 0);
    begin
        -- poly = x^16 + x^14 + x^13 + x^11 + 1
        -- tap positions = x(0), x(2), x(3), x(5)
        y(7 downto 0)  := x(15 downto 8);
        y(15 downto 8) := x(7 downto 0) xor x(9 downto 2) xor x(10 downto 3) xor x(12 downto 5);
        return y;
    end function;

    -- Sending side state.
    type tx_state_type is ( txst_idle, txst_prepare, txst_data );

    -- Receiving side state.
    type rx_state_type is ( rxst_idle, rxst_data );

    -- Registers.
    type regs_type is record
        tx_state:       tx_state_type;
        tx_timecnt:     std_logic_vector((tickdiv-1) downto 0);
        tx_quietcnt:    std_logic_vector(15 downto 0);
        tx_pktlen:      std_logic_vector(15 downto 0);
        tx_lfsr:        std_logic_vector(15 downto 0);
        tx_enabledata:  std_ulogic;
        rx_state:       rx_state_type;
        rx_quietcnt:    std_logic_vector(15 downto 0);
        rx_enabledata:  std_ulogic;
        rx_gottick:     std_ulogic;
        rx_expecttick:  std_ulogic;
        rx_expectglitch: unsigned(5 downto 0);
        rx_badpacket:   std_ulogic;
        rx_pktlen:      std_logic_vector(15 downto 0);
        rx_prev:        std_logic_vector(15 downto 0);
        rx_lfsr:        std_logic_vector(15 downto 0);
        running:        std_ulogic;
        tick_in:        std_ulogic;
        time_in:        std_logic_vector(5 downto 0);
        txwrite:        std_ulogic;
        txflag:         std_ulogic;
        txdata:         std_logic_vector(7 downto 0);
        rxread:         std_ulogic;
        gotdata:        std_ulogic;
        dataerror:      std_ulogic;
        tickerror:      std_ulogic;
    end record;

    -- Reset state.
    constant regs_reset: regs_type := (
        tx_state        => txst_idle,
        tx_timecnt      => (others => '0'),
        tx_quietcnt     => (others => '0'),
        tx_pktlen       => (others => '0'),
        tx_lfsr         => (1 => '1', others => '0'),
        tx_enabledata   => '0',
        rx_state        => rxst_idle,
        rx_quietcnt     => (others => '0'),
        rx_enabledata   => '0',
        rx_gottick      => '0',
        rx_expecttick   => '0',
        rx_expectglitch => "000001",
        rx_badpacket    => '0',
        rx_pktlen       => (others => '0'),
        rx_prev         => (others => '0'),
        rx_lfsr         => (others => '0'),
        running         => '0',
        tick_in         => '0',
        time_in         => (others => '0'),
        txwrite         => '0',
        txflag          => '0',
        txdata          => (others => '0'),
        rxread          => '0',
        gotdata         => '0',
        dataerror       => '0',
        tickerror       => '0' );

    signal r:   regs_type := regs_reset;
    signal rin: regs_type;

    -- Interface signals.
    signal s_txrdy:     std_logic;
    signal s_tickout:   std_logic;
    signal s_timeout:   std_logic_vector(5 downto 0);
    signal s_rxvalid:   std_logic;
    signal s_rxflag:    std_logic;
    signal s_rxdata:    std_logic_vector(7 downto 0);
    signal s_running:   std_logic;
    signal s_errdisc:   std_logic;
    signal s_errpar:    std_logic;
    signal s_erresc:    std_logic;
    signal s_errcred:   std_logic;

begin

    -- spwstream instance
    spwstream_inst: spwstream
        generic map (
            sysfreq         => sysfreq,
            txclkfreq       => txclkfreq,
            rximpl          => rximpl,
            rxchunk         => rxchunk,
            tximpl          => tximpl,
            rxfifosize_bits => rxfifosize_bits,
            txfifosize_bits => txfifosize_bits )
        port map (
            clk         => clk,
            rxclk       => rxclk,
            txclk       => txclk,
            rst         => rst,
            autostart   => autostart,
            linkstart   => linkstart,
            linkdis     => linkdisable,
            txdivcnt    => txdivcnt,
            tick_in     => r.tick_in,
            ctrl_in     => (others => '0'),
            time_in     => r.time_in,
            txwrite     => r.txwrite,
            txflag      => r.txflag,
            txdata      => r.txdata,
            txrdy       => s_txrdy,
            txhalff     => open,
            tick_out    => s_tickout,
            ctrl_out    => open,
            time_out    => s_timeout,
            rxvalid     => s_rxvalid,
            rxhalff     => open,
            rxflag      => s_rxflag,
            rxdata      => s_rxdata,
            rxread      => r.rxread,
            started     => linkstarted,
            connecting  => linkconnecting,
            running     => s_running,
            errdisc     => s_errdisc,
            errpar      => s_errpar,
            erresc      => s_erresc,
            errcred     => s_errcred,
            spw_di      => spw_di,
            spw_si      => spw_si,
            spw_do      => spw_do,
            spw_so      => spw_so
        );

    -- Drive status indications.
    linkrun     <= s_running;
    linkerror   <= s_errdisc or s_errpar or s_erresc or s_errcred;
    gotdata     <= r.gotdata;
    dataerror   <= r.dataerror;
    tickerror   <= r.tickerror;

    process (r, rst, senddata, sendtick, s_txrdy, s_tickout, s_timeout, s_rxvalid, s_rxflag, s_rxdata, s_running) is
        variable v: regs_type;
    begin
        v           := r;

        -- Initiate timecode transmissions.
        v.tx_timecnt := std_logic_vector(unsigned(r.tx_timecnt) + 1);
        if unsigned(v.tx_timecnt) = 0 then
            v.tick_in   := sendtick;
        else
            v.tick_in   := '0';
        end if;
        if r.tick_in = '1' then
            v.time_in   := std_logic_vector(unsigned(r.time_in) + 1);
            v.rx_expecttick := '1';
            v.rx_gottick    := '0';
        end if;

        -- Turn data generator on/off at regular intervals.
        v.tx_quietcnt := std_logic_vector(unsigned(r.tx_quietcnt) + 1);
        if unsigned(r.tx_quietcnt) = 61000 then
            v.tx_quietcnt := (others => '0');
        end if;
        v.tx_enabledata := senddata and (not r.tx_quietcnt(15)); -- ?????

        -- Generate data packets.
        case r.tx_state is
            when txst_idle =>
                -- generate packet length
                v.tx_state  := txst_prepare;
                v.tx_pktlen := r.tx_lfsr;
                v.txwrite   := '0';
                v.tx_lfsr   := lfsr16(r.tx_lfsr);
            when txst_prepare =>
                -- generate first byte of packet
                v.tx_state  := txst_data;
                v.txwrite   := r.tx_enabledata;
                v.txflag    := '0';
                v.txdata    := r.tx_lfsr(15 downto 8);
                v.tx_lfsr   := lfsr16(r.tx_lfsr);
            when txst_data =>
                -- generate data bytes and EOP
                v.txwrite   := r.tx_enabledata;
                if r.txwrite = '1' and s_txrdy = '1' then
                    -- just sent one byte
                    v.tx_pktlen := std_logic_vector(unsigned(r.tx_pktlen) - 1);
                    if unsigned(r.tx_pktlen) = 0 then
                        -- done with packet
                        v.tx_state  := txst_idle;
                        v.txwrite   := '0';
                    elsif unsigned(r.tx_pktlen) = 1 then
                        -- generate EOP
                        v.txwrite   := r.tx_enabledata;
                        v.txflag    := '1';
                        v.txdata    := (others => '0');
                        v.tx_lfsr   := lfsr16(r.tx_lfsr);
                    else
                        -- generate next data byte
                        v.txwrite   := r.tx_enabledata;
                        v.txflag    := '0';
                        v.txdata    := r.tx_lfsr(15 downto 8);
                        v.tx_lfsr   := lfsr16(r.tx_lfsr);
                    end if;
                end if;
        end case;

        -- Blink light when receiving data.
        v.gotdata   := s_rxvalid and r.rxread;

        -- Detect missing timecodes.
        if r.tick_in = '1' and r.rx_expecttick = '1' then
            -- This is bad; a new timecode is being generated while
            -- we have not even received the previous one yet.
            v.tickerror := '1';
        end if;

        -- Receive and check incoming timecodes.
        if s_tickout = '1' then
            if unsigned(s_timeout) + 1 /= unsigned(r.time_in) then
                -- Received time code does not match last transmitted code.
                v.tickerror := '1';
            end if;
            if r.rx_gottick = '1' then
                -- Already received the last transmitted time code.
                v.tickerror := '1';
            end if;
            v.rx_expecttick := '0';
            v.rx_gottick    := '1';
        end if;

        -- Turn data receiving on/off at regular intervals
        v.rx_quietcnt := std_logic_vector(unsigned(r.rx_quietcnt) + 1);
        if unsigned(r.rx_quietcnt) = 55000 then
            v.rx_quietcnt := (others => '0');
        end if;
        v.rx_enabledata := not r.rx_quietcnt(15);

        case r.rx_state is
            when rxst_idle =>
                -- get expected packet length
                v.rx_state  := rxst_data;
                v.rx_pktlen := r.rx_lfsr;
                v.rx_lfsr   := lfsr16(r.rx_lfsr);
                v.rx_prev   := (others => '0');
            when rxst_data =>
                v.rxread    := r.rx_enabledata;
                if r.rxread = '1' and s_rxvalid = '1' then
                    -- got next byte
                    v.rx_pktlen := std_logic_vector(unsigned(r.rx_pktlen) - 1);
                    v.rx_prev   := s_rxdata & r.rx_prev(15 downto 8);
                    if s_rxflag = '1' then
                        -- got EOP or EEP
                        v.rxread    := '0';
                        v.rx_state  := rxst_idle;
                        if s_rxdata = "00000000" then
                            -- got EOP
                            if unsigned(r.rx_pktlen) /= 0 then
                                -- unexpected EOP
                                v.rx_badpacket := '1';
                            end if;
                            -- count errors against expected glitches
                            if v.rx_badpacket = '1' then
                                -- got glitch
                                if r.rx_expectglitch = 0 then
                                    v.dataerror := '1';
                                else
                                    v.rx_expectglitch := r.rx_expectglitch - 1;
                                end if;
                            end if;
                            -- resynchronize LFSR
                            v.rx_lfsr   := lfsr16(lfsr16(r.rx_prev));
                        else
                            -- got EEP
                            v.rx_badpacket := '1';
                        end if;
                        v.rx_badpacket := '0';
                    else
                        -- got next byte
                        v.rx_lfsr   := lfsr16(r.rx_lfsr);
                        if unsigned(r.rx_pktlen) = 0 then
                            -- missing EOP
                            v.rx_badpacket := '1';
                        end if;
                        if s_rxdata /= r.rx_lfsr(15 downto 8) then
                            -- bad data
                            v.rx_badpacket := '1';
                        end if;
                    end if;
                end if;
        end case;

        -- If the link goes away, we should expect inconsistency on the receiving side.
        v.running := s_running;
        if r.running = '1' and s_running = '0' then
            if r.rx_expectglitch /= "111111" then
                v.rx_expectglitch := r.rx_expectglitch + 1;
            end if;
        end if;

        -- If there is no link, we should not expect to receive time codes.
        if s_running = '0' then
            v.rx_expecttick := '0';
        end if;

        -- Synchronous reset.
        if rst = '1' then
            v := regs_reset;
        end if;

        -- Update registers.
        rin <= v;
    end process;

    -- Update registers.
    process (clk) is
    begin
        if rising_edge(clk) then
            r <= rin;
        end if;
    end process;

end architecture echotest_send_arch;
