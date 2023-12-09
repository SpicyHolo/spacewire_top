--
--  Test application for spwstream.
--
--  This entity implements one spwstream instance with SpaceWire signals
--  routed to external ports. The SpaceWire port is assumed to be looped back
--  to itself externally, either directly (tx pins wired to rx pins) or
--  through a remote SpaceWire device which is programmed to echo anything
--  it receives.
--
--  This entity submits a series of test patterns to the transmit side of
--  spwstream. At the same time it monitors the receive side of spwstream
--  and verifies that received data matches the transmitted data pattern.
--
--  Link mode and tx bit rate may be programmed through digital inputs
--  (presumably connected to switches or buttons). Link state and progress of
--  the test are reported through digital outputs (presumably connected to
--  LEDs).
--
--  Note: there is no check on the integrity of the first packet received
--  after the link goes up.
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.spwpkg.all;

entity echotest_recv is

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
        spw_so:     out std_logic;

        linkerror_disc: out std_logic;
        linkerror_par: out std_logic;
        linkerror_esc: out std_logic;
        linkerror_cred: out std_logic
    );

end entity echotest_recv;

architecture echotest_recv_arch of echotest_recv is

    -- Receiving side state.
    type rx_state_type is ( rxst_idle, rxst_data );
    type tx_state_type is ( txst_idle, txst_prepare, txst_data );
    -- Registers.
    type regs_type is record
        tx_state:       tx_state_type;
        tx_timecnt:     std_logic_vector((tickdiv-1) downto 0);
        tx_quietcnt:    std_logic_vector(15 downto 0);
        tx_enabledata:  std_ulogic;
        rx_state:       rx_state_type;
        rx_quietcnt:    std_logic_vector(15 downto 0);
        rx_enabledata:  std_ulogic;
        rx_badpacket:   std_ulogic;
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
        tx_enabledata   => '0',
        rx_state        => rxst_idle,
        rx_quietcnt     => (others => '0'),
        rx_enabledata   => '0',
        rx_badpacket    => '0',
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
    linkerror_disc  <= s_errdisc;
    linkerror_par   <= s_errpar;
    linkerror_esc   <= s_erresc;
    linkerror_cred  <= s_errcred;
    gotdata     <= r.gotdata;
    dataerror   <= r.dataerror;
    tickerror   <= r.tickerror;

    process (r, rst, senddata, sendtick, s_txrdy, s_tickout, s_timeout, s_rxvalid, s_rxflag, s_rxdata, s_running) is
        variable v: regs_type;
    begin
        v           := r;


        v.tick_in   := s_tickout;
        v.time_in   := s_timeout;
        -- Receive and check incoming timecodes.
        if s_tickout = '1' then
            if unsigned(s_timeout) + 1 /= unsigned(r.time_in) then
                -- Received time code does not match last transmitted code.
                v.tickerror := '1';
				else
					 v.tickerror := '0';
            end if;
        end if;


        -- TODO:
        -- Create receive FIFO
        -- Handle EEP
        -- Implement isEOP & isEEP

        -- -- Turn data generator on/off at regular intervals.
        -- v.tx_quietcnt := std_logic_vector(unsigned(r.tx_quietcnt) + 1);
        -- if unsigned(r.tx_quietcnt) = 61000 then
        --     v.tx_quietcnt := (others => '0');
        -- end if;
        -- v.tx_enabledata := senddata and (not r.tx_quietcnt(15));

        -- -- Generate data packets.
        -- case r.tx_state is
        --     when txst_idle =>
        --         v.tx_state  := txst_prepare; -- if reg not empty
        --         v.txwrite   := '0';
        --     when txst_prepare =>
        --         -- generate first byte of packet
        --         v.tx_state  := txst_data;
        --         v.txwrite   := r.tx_enabledata;
        --         v.txflag    := '0';
        --         v.txdata    := r.tx_lfsr(15 downto 8); -- get first byte in reg
        --     when txst_data =>
        --         -- generate data bytes and EOP
        --         v.txwrite   := r.tx_enabledata;
        --         if r.txwrite = '1' and s_txrdy = '1' then
        --             -- just sent one byte
        --             if unsigned(r.tx_pktlen) = 0 then -- if r.txdata = EOP or EEP
        --                 -- done with packet
        --                 v.tx_state  := txst_idle;
        --                 v.txwrite   := '0';
        --             else
        --                 -- generate next data byte
        --                 v.txwrite   := r.tx_enabledata;
        --                 v.txdata    := r.tx_lfsr(15 downto 8); -- get next from reg
        --                 v.txflag    := '0'; -- := isEOP(v.txdata)
        --             end if;
        --         end if;
        -- end case;




 

        -- -- Turn data receiving on/off at regular intervals
        v.rx_quietcnt := std_logic_vector(unsigned(r.rx_quietcnt) + 1);
        if unsigned(r.rx_quietcnt) = 55000 then
            v.rx_quietcnt := (others => '0');
        end if;
        v.rx_enabledata := not r.rx_quietcnt(15);


        v.rxread    := r.rx_enabledata;
        if r.rxread = '1' and s_rxvalid = '1' then
            -- got next byte
            v.txwrite   := '1';
            v.txflag    := s_rxflag;
            v.txdata    := s_rxdata;
        else
            v.txwrite   := '0';    
        end if;


        -- -- Blink light when receiving data.
        v.gotdata   := s_rxvalid and r.rxread;


        -- If the link goes away, we should expect inconsistency on the receiving side.
        v.running := s_running;

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

end architecture echotest_recv_arch;
